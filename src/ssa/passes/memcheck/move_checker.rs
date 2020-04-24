use crate::ssa::isa::*;
use crate::ssa::passes::memcheck::path;
use crate::compiler::Flow;
use std::rc::Rc;
use std::collections::{BTreeMap, HashSet};

macro_rules! check_moved {
    ($path:expr, $moved_set:expr, $previous_moved_set:expr) => {
        $moved_set.contains(&$path) ||
        $previous_moved_set.map(|set| set.contains(&$path)).unwrap_or(false)
    };
}

#[derive(Debug)]
struct MoveError {
    pub position: InsPosition,   
}

pub fn check(context: &mut Context) -> Flow {
    fn walk(
        block: &Block,
        block_idx: usize,
        context: &Context,
        previous_moved_set: Option<&HashSet<path::Path>>
    ) -> Result<HashSet<path::Path>, MoveError> {
        let mut moved_set = HashSet::new();
        for (ins_idx, ins) in block.ins.iter().enumerate() {
            match &ins.typed {
                InsType::Drop(var) | InsType::Move(var) | InsType::MarkMoved(var) => {
                    moved_set.insert(path::Path::Variable(*var));
                }
                InsType::Phi { .. } => (),
                InsType::MemberReference { left, indices, modifier } => {
                    let path = path::Path::Variable(*left);
                    if check_moved!(path, moved_set, previous_moved_set) {
                        return Err(MoveError {
                            position: InsPosition {
                                block_idx,
                                ins_idx,
                            }
                        });
                    }

                    let mut path_vec = vec![];
                    for index in indices {
                        match &index.var {
                            MemberExprIndexVar::StructIndex(x) => {
                                path_vec.push(path::Index::Struct(*x));
                            }
                            MemberExprIndexVar::Variable(_) => unimplemented!(),
                        }
                    }
                    let member_path = path::Path::Member {
                        left: *left,
                        indices: path_vec
                    };
                    if moved_set.contains(&member_path) {
                        return Err(MoveError {
                            position: InsPosition {
                                block_idx,
                                ins_idx,
                            }
                        });
                    }

                    if *modifier == ReferenceModifier::Move {
                        moved_set.insert(member_path);
                    }
                }
                _ => {
                    let mut error = None;
                    ins.each_used_var(|var| {
                        if error.is_some() { return; }
                        let path = path::Path::Variable(var);
                        if check_moved!(path, moved_set, previous_moved_set) {
                            error = Some(MoveError {
                                position: InsPosition {
                                    block_idx,
                                    ins_idx,
                                }
                            });
                        }
                    });
                    if let Some(error) = error {
                        return Err(error);
                    }
                },
            }
        }
        for &succ in &block.succs {
            if succ > block_idx {
                let new_set = walk(
                    &context.blocks[succ],
                    succ,
                    context,
                    Some(&moved_set)
                )?;
                moved_set.extend(new_set.into_iter());
            }
        }
        Ok(moved_set)
    }
    walk(&context.blocks[0], 0, context, None).unwrap();
    Flow::Continue
}