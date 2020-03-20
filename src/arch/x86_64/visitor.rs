use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;
use crate::arch::x86_64::isa;
use crate::ssa::isa::*;

static ARG_REGS: [isa::Reg; 6] = [
    isa::Reg::Rdi,
    isa::Reg::Rsi,
    isa::Reg::Rdx,
    isa::Reg::Rcx,
    isa::Reg::R8,
    isa::Reg::R9,
];

static ALLOC_REGS: [isa::Reg; 14] = [
    isa::Reg::Rdi,
    isa::Reg::Rsi,
    isa::Reg::Rdx,
    isa::Reg::Rcx,
    isa::Reg::Rax,
    isa::Reg::Rbx,
    isa::Reg::R8,
    isa::Reg::R9,
    isa::Reg::R10,
    isa::Reg::R11,
    isa::Reg::R12,
    isa::Reg::R13,
    isa::Reg::R14,
    isa::Reg::R15
];

pub enum Error {
    None,
}

pub struct FuncContextVisitor {
    block: isa::Block,
    unflattened_code: HashMap<Rc<str>, Vec<isa::Block>>,
    constant_operands: BTreeMap<usize, isa::Operand>,
}

impl FuncContextVisitor {
    pub fn new() -> Self {
        FuncContextVisitor {
            block: isa::Block { ins: vec![] },
            unflattened_code: HashMap::new(),
            constant_operands: BTreeMap::new(),
        }
    }

    pub fn process(&mut self, contexts: &FuncContexts) -> Result<isa::IsaContexts, Error> {
        for (_, context) in contexts {
            if let Some(context) = context {
                self.visit_context(&context, contexts);
            }
        }
        Ok(isa::IsaContexts::new())
    }

    pub fn visit_context(&mut self, context: &Context, contexts: &FuncContexts) {
        if context.intrinsic != IntrinsicType::None {
            return self.visit_intrinsic(context);
        }
        let mut blocks = vec![];
        for block in &context.blocks {
            self.constant_operands.clear();
            let mut isa_ins = vec![];
            println!("{:#?}", block);
            for ins in &block.ins {
                self.visit_ins(ins, &mut isa_ins, context, contexts);
            }
            println!("{:#?}", isa_ins);
            let isa_block = std::mem::replace(&mut self.block, isa::Block { ins: isa_ins });
            blocks.push(isa_block);
        }
        println!("---\nbefore: {:#?}\n===", blocks);
        self.allocate_registers(&mut blocks);
        self.optimize_instructions(&mut blocks);
        println!("after: {:#?}\n---", blocks);
        self.unflattened_code.insert(context.real_name.as_ref().unwrap().clone(), blocks);
    }

    fn get_register_for_var(&mut self, arg: usize) -> isa::Operand {
        self.constant_operands.get(&arg).cloned()
            .unwrap_or(isa::Operand::UndeterminedMapping(arg))
    }

    pub fn visit_ins(&mut self,
                     ins: &Ins,
                     isa_ins: &mut Vec<isa::Ins>,
                     context: &Context, contexts: &FuncContexts) {
        match &ins.typed {
            InsType::Nop => (),
            InsType::LoadArg(arg) => {
                assert_eq!(&context.variables[*arg], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Mov(isa::TwoOperands::new(
                        isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                        isa::Operand::Register(ARG_REGS[*arg]),
                    ))
                });
            },
            InsType::LoadI32(x) => {
                self.constant_operands.insert(ins.retvar().unwrap(), isa::Operand::U32(*x as u32));
            },
            InsType::Call { name, args } => {
                for (idx, &arg) in args.iter().enumerate() {
                    assert_eq!(&context.variables[arg], &Type::I32);
                    isa_ins.push(isa::Ins {
                        typed: isa::InsType::Mov(isa::TwoOperands::new(
                            isa::Operand::Register(ARG_REGS[idx]),
                            self.get_register_for_var(arg),
                        ))
                    });
                }
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Call(
                        contexts[name].as_ref().unwrap().real_name.as_ref().unwrap().clone()
                    )
                });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Mov(isa::TwoOperands::new(
                        self.get_register_for_var(ins.retvar().unwrap()),
                        isa::Operand::Register(isa::Reg::Rax),
                    ))
                });
            }
            InsType::Return(x) => {
                assert_eq!(&context.variables[*x], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Mov(isa::TwoOperands::new(
                        isa::Operand::Register(isa::Reg::Rax),
                        self.get_register_for_var(*x)
                    ))
                });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Ret
                });
            }
            InsType::Add((x, y)) => {
                let left = &context.variables[*x];
                let right = &context.variables[*y];
                match (left, right) {
                    (Type::I32, Type::I32) => {
                        let dest = isa::Operand::UndeterminedMapping(ins.retvar().unwrap());
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::Mov(isa::TwoOperands::new(
                                dest.clone(),
                                self.get_register_for_var(*x)
                            ))
                        });
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::Add(isa::TwoOperands::new(
                                dest.clone(),
                                self.get_register_for_var(*y)
                            ))
                        });
                    }
                    (_, _) => unimplemented!(),
                }
            }
            _ => unimplemented!()
        }
    }

    pub fn visit_intrinsic(&mut self, context: &Context) {
        // TODO
    }

    pub fn allocate_registers(&mut self, blocks: &mut Vec<isa::Block>) {
        // Infer some lifetimes
        {
            let mut used: BTreeSet<usize> = btreeset![];
            for block in blocks.iter_mut().rev() {
                for ins in block.ins.iter_mut().rev() {
                    match &mut ins.typed {
                        isa::InsType::Mov(operands) |
                        isa::InsType::Add(operands) => {
                            let dest =
                                if let isa::Operand::UndeterminedMapping(reg) = operands.dest {
                                    used.insert(reg)
                                } else {
                                    false
                                };
                            let src =
                                if let isa::Operand::UndeterminedMapping(reg) = operands.src {
                                    used.insert(reg)
                                } else {
                                    false
                                };
                            match (dest, src) {
                                (true, true) =>
                                    operands.expires = isa::ExpirationPolicy::Both,
                                (true, false) =>
                                    operands.expires = isa::ExpirationPolicy::Dest,
                                (false, true) =>
                                    operands.expires = isa::ExpirationPolicy::Src,
                                (false, false) => (),
                            }
                        }
                        _ =>  (),
                    }
                }
            }
        }
        // Fill in the undetermined mappings
        let mut unused_regs: Vec<isa::Reg> = ALLOC_REGS.iter().rev().cloned().collect();
        let mut mapping: BTreeMap<usize, isa::Reg> = btreemap![];
        fn map_undetermined(operand: &mut isa::Operand,
                            mapping: &mut BTreeMap<usize, isa::Reg>,
                            unused_regs: &mut Vec<isa::Reg>) -> Option<usize> {
            if let isa::Operand::UndeterminedMapping(u) = operand.clone() {
                if let Some(register) = mapping.get(&u) {
                    std::mem::replace(operand, isa::Operand::Register(*register));
                } else if let Some(register) = unused_regs.pop() {
                    std::mem::replace(operand, isa::Operand::Register(register));
                    mapping.insert(u, register);
                } else {
                    unimplemented!()
                }
                return Some(u);
            }
            None
        }
        for block in blocks {
            for ins in &mut block.ins {
                match &mut ins.typed {
                    isa::InsType::Mov(operands) |
                    isa::InsType::Add(operands) => {
                        let old_dest = map_undetermined(&mut operands.dest, &mut mapping, &mut unused_regs);
                        let old_src = map_undetermined(&mut operands.src, &mut mapping, &mut unused_regs);
                        if operands.expires == isa::ExpirationPolicy::Dest ||
                           operands.expires == isa::ExpirationPolicy::Both {
                            if let isa::Operand::Register(reg) = operands.dest.clone() {
                                mapping.remove(old_dest.as_ref().unwrap());
                                unused_regs.push(reg);
                            } else {
                                unreachable!()
                            }
                        }
                        if operands.expires == isa::ExpirationPolicy::Src ||
                           operands.expires == isa::ExpirationPolicy::Both {
                            if let isa::Operand::Register(reg) = operands.src.clone() {
                                mapping.remove(old_src.as_ref().unwrap());
                                unused_regs.push(reg);
                            } else {
                                unreachable!()
                            }
                        }
                    }
                    _ =>  (),
                }
            }
        }
    }

    pub fn optimize_instructions(&mut self, blocks: &mut Vec<isa::Block>) {
        for block in blocks {
            block.ins.retain(|ins| {
                match &ins.typed {
                    isa::InsType::Mov(operands) => {
                        if operands.dest == operands.src {
                            false
                        } else {
                            true
                        }
                    },
                    _ => true,
                }
            })
        }
    }
}