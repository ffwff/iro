use std::collections::{BTreeSet, BTreeMap, HashMap};
use crate::ssa::isa::*;

pub fn eliminate_unused(mut contexts: FuncContexts) -> FuncContexts {
    for (_, context) in &mut contexts {
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
        }
        for block in &mut context.blocks {
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

pub fn eliminate_consts(mut contexts: FuncContexts) -> FuncContexts {
    for (_, context) in &mut contexts {
        let context = context.as_mut().unwrap();
        let mut u: HashMap<InsType, usize> = HashMap::new();
        let mut replace: BTreeMap<usize, usize> = BTreeMap::new();
        for block in &mut context.blocks {
            block.ins.retain(|ins| {
                if ins.typed.is_const() {
                    let retvar = ins.retvar().unwrap();
                    if let Some(var) = u.get(&ins.typed) {
                        replace.insert(retvar, *var);
                        false
                    } else {
                        u.insert(ins.typed.clone(), retvar);
                        true
                    }
                } else if let InsType::LoadVar(var) = &ins.typed {
                    replace.insert(ins.retvar().unwrap(), *var);
                    false
                } else {
                    true
                }
            });
        }
        for block in &mut context.blocks {
            for ins in &mut block.ins {
                ins.rename_var_by(|oldvar| {
                    if let Some(var) = replace.get(&oldvar) {
                        *var
                    } else {
                        oldvar
                    }
                });
            }
        }
    }
    contexts
}