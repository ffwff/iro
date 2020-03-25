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
    isa::Reg::R15,
    isa::Reg::R14,
    isa::Reg::R13,
    isa::Reg::R12,
    isa::Reg::Rbx,
];

static CALLER_SAVED: [isa::Reg; 2] = [isa::Reg::R11, isa::Reg::R10];

static RETURN_REG: isa::Reg = isa::Reg::Rax;

#[derive(Debug, Clone)]
pub enum Error {
    None,
}

pub struct Codegen {
    unflattened_code: HashMap<Rc<FunctionName>, Vec<isa::Block>>,
    constant_operands: BTreeMap<usize, isa::Operand>,
}

impl Codegen {
    pub fn new() -> Self {
        Codegen {
            unflattened_code: HashMap::new(),
            constant_operands: BTreeMap::new(),
        }
    }

    pub fn process(&mut self, program: &Program) -> Result<context::Contexts, Error> {
        dbg_println!("processing {:#?}", program);
        for (name, context) in &program.contexts {
            self.visit_context(&context, &name);
        }
        let mut isa_contexts = context::Contexts::new();
        for (name, context) in &self.unflattened_code {
            isa_contexts.insert(name.clone(), encoder::encode_blocks(&context));
        }
        Ok(isa_contexts)
    }

    pub fn visit_context(&mut self, context: &Context, name: &Rc<FunctionName>) {
        if context.intrinsic != IntrinsicType::None {
            return self.visit_intrinsic(context);
        }
        let mut blocks = vec![];
        self.constant_operands.clear();
        for (idx, block) in context.blocks.iter().enumerate() {
            let mut isa_ins = vec![];
            dbg_println!("{:#?}", block);
            for ins in &block.ins {
                self.visit_ins(ins, &mut isa_ins, idx, context);
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
        block_idx: usize,
        context: &Context,
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
                let mut clobbers: Vec<(isa::Reg, Option<usize>)> =
                    CALLER_SAVED.iter().map(|reg| (*reg, None)).collect();
                for (idx, &reg) in ARG_REGS.iter().enumerate() {
                    if let Some(arg) = args.get(idx).cloned() {
                        assert_eq!(&context.variables[idx], &Type::I32);
                        clobbers.push((reg, Some(arg)));
                    } else {
                        clobbers.push((reg, None));
                    }
                }
                // FIXME: check for no return/struct returns
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Clobber({
                        let mut clobbers = clobbers.clone();
                        clobbers.push((RETURN_REG, None));
                        clobbers
                    }),
                });
                for (&arg, &reg) in args.iter().zip(ARG_REGS.iter()) {
                    isa_ins.push(isa::Ins {
                        typed: isa::InsType::MovI32(isa::TwoOperands {
                            dest: isa::Operand::Register(reg),
                            src: self.get_register_for_var(arg),
                        }),
                    });
                }
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Call(name.clone()),
                });
                // We store the clobbered values in Unclobber to extend the lifetime of the variables
                // until the end of the call instruction
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Unclobber(clobbers.clone()),
                });
                // FIXME: calculate return types
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                        src: isa::Operand::Register(RETURN_REG),
                    }),
                });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Unclobber(vec![(RETURN_REG, None)]),
                });
            }
            InsType::Return(x) => {
                assert_eq!(&context.variables[*x], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Clobber(vec![(isa::Reg::Rax, Some(*x))]),
                });
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
                    })
                    | Some(isa::Ins {
                        typed: isa::InsType::Lte(threeops),
                    })
                    | Some(isa::Ins {
                        typed: isa::InsType::Gte(threeops),
                    }) if threeops.dest == *condvar => {
                        let threeops = threeops.clone();
                        let last = isa_ins.pop().unwrap();
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::CmpI32 {
                                ops: isa::TwoOperands {
                                    dest: self.get_register_for_var(threeops.left),
                                    src: self.get_register_for_var(threeops.right),
                                },
                                is_postlude: true,
                            },
                        });
                        match last.typed {
                            isa::InsType::Lt(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jge(*iffalse),
                                    });
                                } else {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jlt(*iftrue),
                                    });
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jmp(*iffalse),
                                    });
                                }
                            }
                            isa::InsType::Gt(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jle(*iffalse),
                                    });
                                } else {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jgt(*iftrue),
                                    });
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jmp(*iffalse),
                                    });
                                }
                            }
                            isa::InsType::Lte(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jgt(*iffalse),
                                    });
                                } else {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jle(*iftrue),
                                    });
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jmp(*iffalse),
                                    });
                                }
                            }
                            isa::InsType::Gte(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jlt(*iffalse),
                                    });
                                } else {
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jge(*iftrue),
                                    });
                                    isa_ins.push(isa::Ins {
                                        typed: isa::InsType::Jmp(*iffalse),
                                    });
                                }
                            }
                            _ => unreachable!(),
                        }
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
            InsType::Lt((x, y))
            | InsType::Gt((x, y))
            | InsType::Lte((x, y))
            | InsType::Gte((x, y)) => {
                let operands = isa::VirtualThreeOperands {
                    dest: ins.retvar().unwrap(),
                    left: *x,
                    right: *y,
                };
                isa_ins.push(match &ins.typed {
                    InsType::Lt(_) => isa::Ins {
                        typed: isa::InsType::Lt(operands),
                    },
                    InsType::Lte(_) => isa::Ins {
                        typed: isa::InsType::Lte(operands),
                    },
                    InsType::Gt(_) => isa::Ins {
                        typed: isa::InsType::Gt(operands),
                    },
                    InsType::Gte(_) => isa::Ins {
                        typed: isa::InsType::Gte(operands),
                    },
                    _ => unreachable!(),
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
        self.allocate_registers_for_block(0, blocks, context, ALLOC_REGS.to_vec(), BTreeMap::new());
    }

    fn allocate_registers_for_block(
        &mut self,
        node: usize,
        blocks: &mut Vec<isa::Block>,
        context: &Context,
        mut unused_regs: Vec<isa::Reg>,
        // Map of variable to (register, allocated interval).
        // if register is none, additional work must be done
        // to retrieve it from local stack.
        mut var_to_reg: BTreeMap<usize, (Option<isa::Reg>, usize)>,
    ) {
        let cblock = &context.blocks[node];
        let mut isa_block = std::mem::replace(&mut blocks[node], isa::Block { ins: vec![] });
        // We defer despilling of the output variables to cleanup stage
        // or when we run out of variables
        let mut variable_factored: BTreeSet<usize> = cblock.vars_out.clone();
        // A map of (ins index, variable set) which maps instruction indices
        // to the variables deallocated at that timestep
        let mut deallocation: BTreeMap<usize, BTreeSet<usize>> = btreemap![];
        // Infer lifetimes for each variable
        for (idx, ins) in isa_block.ins.iter().enumerate().rev() {
            ins.each_used_var(true, |var| match var {
                isa::Operand::UndeterminedMapping(var) => {
                    if !variable_factored.contains(&var) {
                        if let Some(set) = deallocation.get_mut(&idx) {
                            set.insert(*var);
                        } else {
                            deallocation.insert(idx, btreeset![*var]);
                        }
                        variable_factored.insert(*var);
                    }
                }
                _ => (),
            });
        }

        let mut prelude = vec![];
        let mut body = vec![];
        let mut postlude = vec![];
        for (idx, ins) in isa_block.ins.iter_mut().enumerate() {
            dbg_println!(
                "!!! {:#?} {:?} {:?} {:?}",
                ins,
                unused_regs,
                var_to_reg,
                deallocation
            );
            match &ins.typed {
                isa::InsType::Clobber(clobbers) => {
                    dbg_println!("clobbering: {:#?}", clobbers);
                    for (reg, _) in clobbers {
                        unused_regs.remove_item(&reg);
                    }
                    let mut set = deallocation.get_mut(&idx);
                    for (reg, except_for_var) in clobbers {
                        let mut to_remove_reg = None;
                        let mut to_remove_var = None;
                        for (var, vdata) in &mut var_to_reg {
                            if vdata.0 == Some(*reg) {
                                if Some(*var) == *except_for_var {
                                    break;
                                }
                                if let Some(set) = set.as_mut() {
                                    if set.remove(&var) {
                                        dbg_println!("dealloc from clobber {:?}", var);
                                        to_remove_var = Some(*var);
                                        break;
                                    }
                                }
                                if let Some(newreg) = unused_regs.pop() {
                                    body.push(isa::Ins {
                                        typed: isa::InsType::MovI32(isa::TwoOperands {
                                            dest: isa::Operand::Register(newreg),
                                            src: isa::Operand::Register(*reg),
                                        }),
                                    });
                                    vdata.0.replace(newreg);
                                } else {
                                    body.push(isa::Ins {
                                        typed: isa::InsType::MovI32(isa::TwoOperands {
                                            dest: isa::Operand::UndeterminedMapping(*var),
                                            src: isa::Operand::Register(*reg),
                                        }),
                                    });
                                    to_remove_reg = Some(*var);
                                }
                                break;
                            }
                        }
                        if let Some(to_remove_reg) = to_remove_reg {
                            dbg_println!(
                                " => moving var {} ({:?})",
                                to_remove_reg,
                                var_to_reg[&to_remove_reg].0
                            );
                            let var_ref = var_to_reg.get_mut(&to_remove_reg).unwrap();
                            var_ref.0.take();
                        } else if let Some(to_remove_var) = to_remove_var {
                            var_to_reg.remove(&to_remove_var);
                        }
                    }
                    continue;
                }
                isa::InsType::Unclobber(clobbers) => {
                    for (reg, _) in clobbers {
                        debug_assert!(!unused_regs.contains(reg));
                        unused_regs.push(*reg);
                    }
                    continue;
                }
                _ => (),
            }
            ins.rename_var_by(false, |var| {
                dbg_println!(" => {:?} {:?} {:?}", var, var_to_reg, deallocation);
                dbg_println!("  => {:?}", unused_regs);
                if let isa::Operand::UndeterminedMapping(mapping) = var.clone() {
                    if let Some(maybe_reg) = var_to_reg.get_mut(&mapping) {
                        match maybe_reg.clone() {
                            (Some(reg), _) => {
                                *var = isa::Operand::Register(reg);
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
                    } else {
                        // save one of the vars to stack
                        unimplemented!()
                    }
                    if let Some(set) = deallocation.get_mut(&idx) {
                        if set.remove(&mapping) {
                            let (reg, _) = var_to_reg.remove(&mapping).unwrap();
                            unused_regs.push(reg.unwrap());
                        }
                    }
                }
            });
            if ins.typed.is_postlude() {
                postlude.push(ins.clone());
            } else {
                body.push(ins.clone());
            }
        }

        // cleanup and create new envs
        let mut new_var_to_reg: BTreeMap<usize, (Option<isa::Reg>, usize)> = BTreeMap::new();
        for (var, (reg, _)) in &var_to_reg {
            if let Some(reg) = reg.clone() {
                if cblock.vars_out.contains(&var) {
                    if cblock.vars_in.contains(&var) {
                        body.push(isa::Ins {
                            typed: isa::InsType::MovI32(isa::TwoOperands {
                                dest: isa::Operand::UndeterminedMapping(*var),
                                src: isa::Operand::Register(reg),
                            }),
                        });
                        unused_regs.push(reg);
                    } else {
                        new_var_to_reg.insert(*var, (Some(reg), 0));
                    }
                }
            }
        }

        // insert to new block
        let new_isa_block = &mut blocks[node];
        new_isa_block.ins.append(&mut prelude);
        new_isa_block.ins.append(&mut body);
        new_isa_block.ins.append(&mut postlude);

        dbg_println!("free vars {:?}", unused_regs);
        dbg_println!("new_var_to_reg {:?}", new_var_to_reg);
        dbg_println!(
            "dealloc: {:#?}\n{:#?}\n{:#?}\n{:#?}",
            deallocation,
            isa_block,
            cblock,
            new_isa_block
        );

        // Repeat the process for each descendent
        for succ in &cblock.succs {
            // Since this is a DFS tree, all non-looping descendents
            // must have larger indices that its parent
            if *succ > node {
                self.allocate_registers_for_block(
                    *succ,
                    blocks,
                    context,
                    unused_regs.clone(),
                    new_var_to_reg.clone(),
                );
            }
        }
    }

    pub fn setup_stack_and_locals(&mut self, blocks: &mut Vec<isa::Block>) {
        dbg_println!("stacks for: {:#?}", blocks);
        let mut map_to_stack_offset = BTreeMap::new();
        let stack_offset = 8; // skip 1 dword value for saved rbp
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
                                disp: -(disp as i32) - stack_offset,
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
                                stack_used = true;
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
                            local_size: alloc + (stack_offset as u32),
                            save_regs: save_regs.clone(),
                        },
                    },
                );
            }
            for block in blocks.iter_mut() {
                if let Some(last) = block.ins.last_mut() {
                    if last.typed.is_ret() {
                        last.typed = isa::InsType::LeaveAndRet {
                            save_regs: save_regs.clone(),
                        };
                    }
                }
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
