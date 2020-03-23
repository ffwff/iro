use crate::arch::context;
use crate::arch::x86_64::{encoder, isa};
use crate::ssa::isa::*;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;

static ARG_REGS: [isa::Reg; 6] = [
    isa::Reg::Rdi,
    isa::Reg::Rsi,
    isa::Reg::Rdx,
    isa::Reg::Rcx,
    isa::Reg::R8,
    isa::Reg::R9,
];

static ALLOC_REGS: [isa::Reg; 14] = [
    isa::Reg::R15,
    isa::Reg::R14,
    isa::Reg::R13,
    isa::Reg::R12,
    isa::Reg::R11,
    isa::Reg::R10,
    isa::Reg::Rbx,
    isa::Reg::R9,
    isa::Reg::R8,
    isa::Reg::Rax,
    isa::Reg::Rcx,
    isa::Reg::Rdx,
    isa::Reg::Rsi,
    isa::Reg::Rdi,
];

static CALLEE_SAVED: [isa::Reg; 5] = [
    isa::Reg::Rbx,
    isa::Reg::R12,
    isa::Reg::R13,
    isa::Reg::R14,
    isa::Reg::R15,
];

#[derive(Debug, Clone)]
pub enum Error {
    None,
}

pub struct FuncContextVisitor {
    unflattened_code: HashMap<Rc<FunctionName>, Vec<isa::Block>>,
    constant_operands: BTreeMap<usize, isa::Operand>,
}

impl FuncContextVisitor {
    pub fn new() -> Self {
        FuncContextVisitor {
            unflattened_code: HashMap::new(),
            constant_operands: BTreeMap::new(),
        }
    }

    pub fn process(&mut self, contexts: &FuncContexts) -> Result<context::Contexts, Error> {
        dbg_println!("processing {:#?}", contexts);
        for (name, context) in contexts {
            let context = context.as_ref().unwrap();
            self.visit_context(&context, &name, contexts);
        }
        let mut isa_contexts = context::Contexts::new();
        for (name, context) in &self.unflattened_code {
            isa_contexts.insert(name.clone(), encoder::encode_blocks(&context));
        }
        Ok(isa_contexts)
    }

    pub fn visit_context(
        &mut self,
        context: &Context,
        name: &Rc<FunctionName>,
        contexts: &FuncContexts,
    ) {
        if context.intrinsic != IntrinsicType::None {
            return self.visit_intrinsic(context);
        }
        let mut blocks = vec![];
        for block in &context.blocks {
            self.constant_operands.clear();
            let mut isa_ins = vec![];
            dbg_println!("{:#?}", block);
            for ins in &block.ins {
                self.visit_ins(ins, &mut isa_ins, context, contexts);
            }
            dbg_println!("{:#?}", isa_ins);
            blocks.push(isa::Block { ins: isa_ins });
        }
        dbg_println!("---\nbefore: {:#?}\n===", blocks);
        self.allocate_registers(&mut blocks, context);
        self.setup_stack_and_locals(&mut blocks);
        self.remove_unnecessary_movs(&mut blocks);
        self.remove_unnecessary_jmps(&mut blocks);
        dbg_println!("after: {:#?}\n---", blocks);
        self.unflattened_code.insert(name.clone(), blocks);
    }

    fn get_register_for_var(&mut self, arg: usize) -> isa::Operand {
        self.constant_operands
            .get(&arg)
            .cloned()
            .unwrap_or(isa::Operand::UndeterminedMapping(arg))
    }

    pub fn visit_ins(
        &mut self,
        ins: &Ins,
        isa_ins: &mut Vec<isa::Ins>,
        context: &Context,
        _contexts: &FuncContexts,
    ) {
        dbg_println!("ins: {:#?}", ins);
        match &ins.typed {
            InsType::Nop => (),
            InsType::LoadVar(arg) => {
                assert_eq!(&context.variables[*arg], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                        src: self.get_register_for_var(*arg),
                    }),
                });
            }
            InsType::LoadArg(arg) => {
                assert_eq!(&context.variables[*arg], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                        src: isa::Operand::Register(ARG_REGS[*arg]),
                    }),
                });
            }
            InsType::LoadI32(x) => {
                self.constant_operands
                    .insert(ins.retvar().unwrap(), isa::Operand::U32(*x as u32));
            }
            InsType::Call { name, args } => {
                for (idx, &arg) in args.iter().enumerate() {
                    assert_eq!(&context.variables[arg], &Type::I32);
                    let reg = ARG_REGS[idx];
                    isa_ins.push(isa::Ins {
                        typed: isa::InsType::Clobber {
                            reg,
                            except_for_var: Some(arg),
                        },
                    });
                    isa_ins.push(isa::Ins {
                        typed: isa::InsType::MovI32(isa::TwoOperands {
                            dest: isa::Operand::Register(reg),
                            src: self.get_register_for_var(arg),
                        }),
                    });
                }
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Clobber {
                        reg: isa::Reg::Rax,
                        except_for_var: None,
                    },
                });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Call(name.clone()),
                });
                for (idx, _) in args.iter().enumerate() {
                    isa_ins.push(isa::Ins {
                        typed: isa::InsType::Unclobber(ARG_REGS[idx]),
                    });
                }
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: self.get_register_for_var(ins.retvar().unwrap()),
                        src: isa::Operand::Register(isa::Reg::Rax),
                    }),
                });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Unclobber(isa::Reg::Rax),
                });
            }
            InsType::Return(x) => {
                assert_eq!(&context.variables[*x], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: isa::Operand::Register(isa::Reg::Rax),
                        src: self.get_register_for_var(*x),
                    }),
                });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Ret,
                });
            }
            InsType::Exit => {
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Ret,
                });
            }
            InsType::IfJmp {
                condvar,
                iftrue,
                iffalse,
            } => {
                match isa_ins.last() {
                    Some(isa::Ins {
                        typed: isa::InsType::Lt(threeops),
                    })
                    | Some(isa::Ins {
                        typed: isa::InsType::Gt(threeops),
                    }) if threeops.dest == *condvar => {
                        let threeops = threeops.clone();
                        let last = isa_ins.pop().unwrap();
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::CmpI32(isa::TwoOperands {
                                dest: self.get_register_for_var(threeops.left),
                                src: self.get_register_for_var(threeops.right),
                            }),
                        });
                        isa_ins.push(isa::Ins {
                            typed: match last.typed {
                                isa::InsType::Lt(_) => isa::InsType::Jlt(*iftrue),
                                isa::InsType::Gt(_) => isa::InsType::Jgt(*iftrue),
                                _ => unreachable!(),
                            },
                        });
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::Jmp(*iffalse),
                        });
                        return;
                    }
                    _ => (),
                }
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::IfJmp {
                        condvar: isa::Operand::UndeterminedMapping(*condvar),
                        iftrue: *iftrue,
                        iffalse: *iffalse,
                    },
                });
            }
            InsType::Jmp(x) => {
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Jmp(*x),
                });
            }
            InsType::Lt((x, y)) => {
                let operands = isa::VirtualThreeOperands {
                    dest: ins.retvar().unwrap(),
                    left: *x,
                    right: *y,
                };
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Lt(operands),
                });
            }
            InsType::Gt((x, y)) => {
                let operands = isa::VirtualThreeOperands {
                    dest: ins.retvar().unwrap(),
                    left: *x,
                    right: *y,
                };
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Gt(operands),
                });
            }
            InsType::Add((x, y))
            | InsType::Sub((x, y))
            | InsType::Mul((x, y))
            | InsType::Div((x, y)) => {
                let left = &context.variables[*x];
                let right = &context.variables[*y];
                match (left, right) {
                    (Type::I32, Type::I32) => {
                        let dest = isa::Operand::UndeterminedMapping(ins.retvar().unwrap());
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::MovI32(isa::TwoOperands {
                                dest: dest.clone(),
                                src: self.get_register_for_var(*x),
                            }),
                        });
                        isa_ins.push(isa::Ins {
                            typed: {
                                let operands = isa::TwoOperands {
                                    dest: dest.clone(),
                                    src: self.get_register_for_var(*y),
                                };
                                match &ins.typed {
                                    InsType::Add(_) => isa::InsType::AddI32(operands),
                                    InsType::Sub(_) => isa::InsType::SubI32(operands),
                                    InsType::Mul(_) => isa::InsType::MulI32(operands),
                                    InsType::Div(_) => isa::InsType::DivI32(operands),
                                    _ => unreachable!(),
                                }
                            },
                        });
                    }
                    (_, _) => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    pub fn visit_intrinsic(&mut self, _context: &Context) {
        // TODO
    }

    pub fn allocate_registers(&mut self, blocks: &mut Vec<isa::Block>, context: &Context) {
        for (node, cblock) in context.blocks.iter().enumerate() {
            let mut isa_block = std::mem::replace(&mut blocks[node], isa::Block { ins: vec![] });
            let mut variable_factored: BTreeSet<usize> = btreeset![];
            // A vector of (ins index, variable) which maps the position in the block.ins
            // to the variable being deallocated
            let mut deallocation: Vec<(usize, usize)> = vec![];
            // Infer lifetimes for each variable
            for (idx, ins) in isa_block.ins.iter().enumerate().rev() {
                ins.each_used_var(true, |var| match var {
                    isa::Operand::UndeterminedMapping(var) => {
                        if !variable_factored.contains(&var) {
                            deallocation.push((idx, *var));
                            variable_factored.insert(*var);
                        }
                    }
                    _ => (),
                });
            }

            // Map of variable to (register, allocated interval)
            // if register is none, additional work must be done
            // to retrieve it from local stack
            let mut unused_regs = ALLOC_REGS.to_vec();
            let mut var_to_reg: BTreeMap<usize, (Option<isa::Reg>, usize)> = btreemap![];
            let mut prelude = vec![];
            let mut body = vec![];
            let mut postlude = vec![];
            for (idx, ins) in isa_block.ins.iter_mut().enumerate() {
                dbg_println!("!!! {:#?} {:?} {:?}", ins, var_to_reg, deallocation);
                match &ins.typed {
                    isa::InsType::Clobber {
                        reg,
                        except_for_var,
                    } => {
                        if unused_regs.contains(&reg) {
                            unused_regs.remove_item(&reg);
                        } else {
                            let mut to_remove = None;
                            for (var, vdata) in &mut var_to_reg {
                                if vdata.0 == Some(*reg) {
                                    if Some(*var) == *except_for_var {
                                        break;
                                    }
                                    if let Some(newreg) = unused_regs.pop() {
                                        // FIXME: correct mov type pls
                                        body.push(isa::Ins {
                                            typed: isa::InsType::MovI32(isa::TwoOperands {
                                                dest: isa::Operand::Register(newreg),
                                                src: isa::Operand::Register(*reg),
                                            }),
                                        });
                                        vdata.0 = Some(newreg);
                                    } else {
                                        body.push(isa::Ins {
                                            typed: isa::InsType::MovI32(isa::TwoOperands {
                                                dest: isa::Operand::UndeterminedMapping(*var),
                                                src: isa::Operand::Register(*reg),
                                            }),
                                        });
                                        to_remove = Some(*var);
                                    }
                                    break;
                                }
                            }
                            if let Some(to_remove) = to_remove {
                                var_to_reg.remove(&to_remove);
                            }
                        }
                        continue;
                    }
                    isa::InsType::Unclobber(reg) => {
                        unused_regs.push(*reg);
                        continue;
                    }
                    _ => (),
                }
                ins.rename_var_by(false, |var| {
                    dbg_println!(" => {:?} {:?} {:?}", var, var_to_reg, deallocation);
                    dbg_println!("  => {:?}", unused_regs);
                    if let isa::Operand::UndeterminedMapping(mapping) = var.clone() {
                        let _reg = if let Some(maybe_reg) = var_to_reg.get_mut(&mapping) {
                            match maybe_reg.clone() {
                                (Some(reg), _) => {
                                    *var = isa::Operand::Register(reg);
                                    reg
                                }
                                (None, _) => {
                                    // The variable used to store this node has previously
                                    // been clobbered, so hopefully we can retrieve
                                    // the clobbered register again
                                    let reg = unused_regs.pop().unwrap();
                                    body.push(isa::Ins {
                                        typed: isa::InsType::MovI32(isa::TwoOperands {
                                            dest: isa::Operand::Register(reg),
                                            src: var.clone(),
                                        }),
                                    });
                                    maybe_reg.0 = Some(reg);
                                    reg
                                }
                            }
                        } else if let Some(reg) = unused_regs.pop() {
                            // Unspill the local variable if it's used in the precceding blocks
                            if cblock.vars_in.contains(&mapping) {
                                // FIXME: correct mov type pls
                                body.push(isa::Ins {
                                    typed: isa::InsType::MovI32(isa::TwoOperands {
                                        dest: isa::Operand::Register(reg),
                                        src: var.clone(),
                                    }),
                                });
                            }
                            *var = isa::Operand::Register(reg);
                            var_to_reg.insert(mapping, (Some(reg), idx));
                            reg
                        } else {
                            // save one of the vars to stack
                            unimplemented!()
                        };
                        if let Some((didx, dvar)) = deallocation.last() {
                            if idx == *didx && mapping == *dvar {
                                deallocation.pop();
                                let (reg, _) = var_to_reg[&mapping];
                                // We defer despilling of the output variables to cleanup stage
                                // or when we run out of variables
                                if !cblock.vars_out.contains(&mapping) {
                                    unused_regs.push(reg.unwrap());
                                }
                            }
                        }
                    }
                });
                if ins.typed.is_jmp() {
                    postlude.push(ins.clone());
                } else {
                    body.push(ins.clone());
                }
            }

            // cleanup
            for (var, (reg, _)) in &var_to_reg {
                let reg = reg.unwrap();
                if cblock.vars_out.contains(&var) {
                    body.push(isa::Ins {
                        typed: isa::InsType::MovI32(isa::TwoOperands {
                            dest: isa::Operand::UndeterminedMapping(*var),
                            src: isa::Operand::Register(reg),
                        }),
                    });
                    unused_regs.push(reg);
                }
            }

            // insert to new block
            let new_isa_block = &mut blocks[node];
            new_isa_block.ins.append(&mut prelude);
            new_isa_block.ins.append(&mut body);
            new_isa_block.ins.append(&mut postlude);

            dbg_println!("free vars {:?}", unused_regs);
            dbg_println!(
                "dealloc: {:#?}\n{:#?}\n{:#?}\n{:#?}",
                deallocation,
                isa_block,
                cblock,
                new_isa_block
            );
            // panic!("...");
        }
    }

    pub fn setup_stack_and_locals(&mut self, blocks: &mut Vec<isa::Block>) {
        let mut map_to_stack_offset = BTreeMap::new();
        let stack_offset = -8; // skip 1 dword value for saved rbp
        let mut alloc = 0u32;
        let mut stack_used = false;
        let mut save_regs = BTreeSet::new();
        for block in blocks.iter_mut() {
            for ins in &mut block.ins {
                if let Some(size) = ins.typed.mov_size() {
                    ins.rename_var_by(true, |var| match var.clone() {
                        isa::Operand::UndeterminedMapping(mapping) => {
                            let disp = if let Some(offset) = map_to_stack_offset.get(&mapping) {
                                *offset
                            } else {
                                let offset = alloc;
                                map_to_stack_offset.insert(mapping, offset);
                                alloc += size as u32;
                                offset
                            };
                            *var = isa::Operand::Memory {
                                disp: -(disp as i32) + stack_offset,
                                base: isa::Reg::Rbp,
                            };
                        }
                        isa::Operand::Memory {
                            base: isa::Reg::Rsp,
                            ..
                        }
                        | isa::Operand::Memory {
                            base: isa::Reg::Rbp,
                            ..
                        } => {
                            stack_used = true;
                        }
                        isa::Operand::Register(reg) => {
                            if CALLEE_SAVED.contains(&reg) {
                                save_regs.insert(reg);
                            }
                        }
                        _ => (),
                    });
                }
            }
        }
        if alloc != 0 || stack_used {
            let save_regs: Vec<isa::Reg> = save_regs.iter().cloned().collect();
            if let Some(block) = blocks.first_mut() {
                block.ins.insert(
                    0,
                    isa::Ins {
                        typed: isa::InsType::Enter {
                            local_size: alloc,
                            save_regs: save_regs.clone(),
                        },
                    },
                );
            }
            if save_regs.is_empty() {
                for block in blocks.iter_mut() {
                    if let Some(last) = block.ins.last_mut() {
                        if last.typed.is_ret() {
                            last.typed = isa::InsType::LeaveAndRet { save_regs: vec![] };
                        }
                    }
                }
            } else {
                dbg_println!("{:#?}", save_regs);
                unimplemented!()
            }
        }
    }

    pub fn remove_unnecessary_movs(&mut self, blocks: &mut Vec<isa::Block>) {
        for block in blocks {
            block.ins.retain(|ins| match &ins.typed {
                isa::InsType::MovI32(operands) => {
                    if operands.dest == operands.src {
                        false
                    } else {
                        true
                    }
                }
                _ => true,
            })
        }
    }

    pub fn remove_unnecessary_jmps(&mut self, blocks: &mut Vec<isa::Block>) {
        for (idx, block) in blocks.iter_mut().enumerate() {
            if let Some(target) = block
                .ins
                .last()
                .map(|last| match &last.typed {
                    isa::InsType::Jmp(x) => Some(*x),
                    _ => None,
                })
                .flatten()
            {
                if target == idx + 1 {
                    block.ins.pop();
                }
            }
        }
    }
}
