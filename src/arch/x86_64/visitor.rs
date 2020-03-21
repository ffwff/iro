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

    pub fn process(&mut self, contexts: &FuncContexts) -> Result<isa::IsaContexts, Error> {
        for (name, context) in contexts {
            let context = context.as_ref().unwrap();
            self.visit_context(&context, &name, contexts);
        }
        println!("{:#?}", self.unflattened_code);
        Ok(isa::IsaContexts::new())
    }

    pub fn visit_context(&mut self, context: &Context, name: &Rc<FunctionName>, contexts: &FuncContexts) {
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
            blocks.push(isa::Block { ins: isa_ins });
        }
        println!("---\nbefore: {:#?}\n===", blocks);
        // self.allocate_registers(&mut blocks);
        // self.optimize_instructions(&mut blocks);
        println!("after: {:#?}\n---", blocks);
        self.unflattened_code.insert(name.clone(), blocks);
    }

    fn get_register_for_var(&mut self, arg: usize) -> isa::Operand {
        self.constant_operands.get(&arg).cloned()
            .unwrap_or(isa::Operand::UndeterminedMapping(arg))
    }

    pub fn visit_ins(&mut self,
                     ins: &Ins,
                     isa_ins: &mut Vec<isa::Ins>,
                     context: &Context, contexts: &FuncContexts) {
        println!("ins: {:#?}", ins);
        match &ins.typed {
            InsType::Nop => (),
            InsType::LoadVar(arg) => {
                assert_eq!(&context.variables[*arg], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                        src: self.get_register_for_var(*arg),
                    })
                });
            },
            InsType::LoadArg(arg) => {
                assert_eq!(&context.variables[*arg], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                        src: isa::Operand::Register(ARG_REGS[*arg]),
                    })
                });
            },
            InsType::LoadI32(x) => {
                self.constant_operands.insert(ins.retvar().unwrap(), isa::Operand::U32(*x as u32));
            },
            InsType::Call { name, args } => {
                for (idx, &arg) in args.iter().enumerate() {
                    assert_eq!(&context.variables[arg], &Type::I32);
                    isa_ins.push(isa::Ins {
                        typed: isa::InsType::MovI32(isa::TwoOperands {
                            dest: isa::Operand::Register(ARG_REGS[idx]),
                            src: self.get_register_for_var(arg),
                        })
                    });
                }
                isa_ins.push(isa::Ins { typed: isa::InsType::Call(name.clone()) });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: self.get_register_for_var(ins.retvar().unwrap()),
                        src: isa::Operand::Register(isa::Reg::Rax),
                    })
                });
            }
            InsType::Return(x) => {
                assert_eq!(&context.variables[*x], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::MovI32(isa::TwoOperands {
                        dest: isa::Operand::Register(isa::Reg::Rax),
                        src: self.get_register_for_var(*x)
                    })
                });
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Ret
                });
            }
            InsType::IfJmp { condvar, iftrue, iffalse } => {
                match isa_ins.last() {
                    Some(isa::Ins { typed: isa::InsType::Lt(threeops) })
                        if threeops.dest == *condvar => {
                        let threeops = threeops.clone();
                        isa_ins.pop();
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::Cmp(isa::TwoOperands {
                                dest: self.get_register_for_var(threeops.left),
                                src: self.get_register_for_var(threeops.right)
                            })
                        });
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::Jlt(*iftrue)
                        });
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::Jmp(*iffalse)
                        });
                        return;
                    }
                    _ => (),
                }
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::IfJmp {
                        condvar: isa::Operand::UndeterminedMapping(*condvar),
                        iftrue: *iftrue,
                        iffalse: *iffalse
                    }
                });
            }
            InsType::Jmp(x) => {
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Jmp(*x)
                });
            }
            InsType::Lt((x, y)) => {
                let operands = isa::VirtualThreeOperands {
                    dest: ins.retvar().unwrap(),
                    left: *x,
                    right: *y,
                };
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Lt(operands)
                });
            }
            InsType::Add((x, y)) |
            InsType::Sub((x, y)) |
            InsType::Mul((x, y)) |
            InsType::Div((x, y)) => {
                let left = &context.variables[*x];
                let right = &context.variables[*y];
                match (left, right) {
                    (Type::I32, Type::I32) => {
                        let dest = isa::Operand::UndeterminedMapping(ins.retvar().unwrap());
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::MovI32(isa::TwoOperands {
                                dest: dest.clone(),
                                src: self.get_register_for_var(*x)
                            })
                        });
                        isa_ins.push(isa::Ins {
                            typed: {
                                let operands = isa::TwoOperands {
                                    dest: dest.clone(),
                                    src: self.get_register_for_var(*y)
                                };
                                match &ins.typed {
                                    InsType::Add(_) => isa::InsType::AddI32(operands),
                                    InsType::Sub(_) => isa::InsType::SubI32(operands),
                                    InsType::Mul(_) => isa::InsType::MulI32(operands),
                                    InsType::Div(_) => isa::InsType::DivI32(operands),
                                    _ => unreachable!(),
                                }
                            }
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
        // TODO
    }

    pub fn optimize_instructions(&mut self, blocks: &mut Vec<isa::Block>) {
        for block in blocks {
            block.ins.retain(|ins| {
                match &ins.typed {
                    isa::InsType::MovI32(operands) => {
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