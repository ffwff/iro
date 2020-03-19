use std::collections::BTreeSet;
use crate::ssa::isa::*;

pub fn eliminate_unused(mut contexts: FuncContexts) -> FuncContexts {
    for (name, context) in &mut contexts {
        let context = context.as_mut().unwrap();
        let mut u: BTreeSet<usize> = BTreeSet::new();
        for block in context.blocks.iter_mut().rev() {
            for ins in block.ins.iter().rev() {
                match &ins.typed {
                    InsType::Call { name: _, args } => {
                        u.insert(ins.retvar().unwrap());
                        for arg in args {
                            u.insert(*arg);
                        }
                    }
                    InsType::Return(x) => {
                        u.insert(*x);
                    }
                    InsType::IfJmp { condvar, iftrue: _, iffalse: _ } => {
                        u.insert(*condvar);
                    }
                    other if !other.is_jmp() => {
                        if !u.contains(&ins.retvar().unwrap()) {
                            continue;
                        }
                        match other {
                            InsType::LoadVar(x) => {
                                u.insert(*x);
                            }
                            InsType::Phi { vars } => {
                                for arg in vars {
                                    u.insert(*arg);
                                }
                            }
                            InsType::Add((x, y)) => { u.insert(*x); u.insert(*y); },
                            InsType::Sub((x, y)) => { u.insert(*x); u.insert(*y); },
                            InsType::Mul((x, y)) => { u.insert(*x); u.insert(*y); },
                            InsType::Div((x, y)) => { u.insert(*x); u.insert(*y); },
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
            block.ins.retain(|ins| {
                if let Some(retvar) = ins.retvar() {
                    u.contains(&retvar)
                } else {
                    true
                }
            });
        }
    }
    contexts
}