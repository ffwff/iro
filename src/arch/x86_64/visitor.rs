use std::collections::{BTreeMap, HashMap};
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
        self.allocate_registers(&mut blocks);
        self.optimize_instructions(&mut blocks);
        println!("{:#?}\n---", blocks);
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
                    typed: isa::InsType::Mov(isa::TwoOperands {
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
                        typed: isa::InsType::Mov(isa::TwoOperands {
                            dest: isa::Operand::Register(ARG_REGS[idx]),
                            src: self.get_register_for_var(arg),
                        })
                    });
                }
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Call(
                        contexts[name].as_ref().unwrap().real_name.as_ref().unwrap().clone()
                    )
                });
            }
            InsType::Return(x) => {
                assert_eq!(&context.variables[*x], &Type::I32);
                isa_ins.push(isa::Ins {
                    typed: isa::InsType::Mov(isa::TwoOperands {
                        dest: isa::Operand::Register(isa::Reg::Rax),
                        src: self.get_register_for_var(*x),
                    })
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
                            typed: isa::InsType::Mov(isa::TwoOperands {
                                dest: dest.clone(),
                                src: self.get_register_for_var(*x),
                            })
                        });
                        isa_ins.push(isa::Ins {
                            typed: isa::InsType::Add(isa::TwoOperands {
                                dest: dest.clone(),
                                src: self.get_register_for_var(*y),
                            })
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
        let mut mapping: BTreeMap<usize, isa::Reg> = btreemap![];
        let mut registers = 0;
        let mut map_undetermined = |operand: &mut isa::Operand| {
            if let isa::Operand::UndeterminedMapping(u) = operand.clone() {
                if let Some(register) = mapping.get(&u) {
                    std::mem::replace(operand, isa::Operand::Register(*register));
                } else {
                    let register = ALLOC_REGS[registers];
                    std::mem::replace(operand, isa::Operand::Register(register));
                    mapping.insert(u, register);
                    registers += 1;
                }
            }
        };
        println!("blocks: {:#?}", blocks);
        for block in blocks {
            for ins in &mut block.ins {
                match &mut ins.typed {
                    isa::InsType::Mov(operands) |
                    isa::InsType::Add(operands) => {
                        map_undetermined(&mut operands.dest);
                        map_undetermined(&mut operands.src);
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