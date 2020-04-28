use crate::compiler::error::{Code, MemoryErrorType};
use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::memcheck::path::*;
use crate::utils::overlay::OverlayHashMap;

pub fn check(context: &mut Context) -> Flow {
    fn walk(
        block: &Block,
        block_idx: usize,
        context: &Context,
        previous_moved_set: Option<&Paths>,
    ) -> Result<Paths, Code> {
        let mut moved_set = Paths::new();
        for ins in &block.ins {
            match &ins.typed {
                InsType::Drop(var) | InsType::Move(var) | InsType::MarkMoved(var) => {
                    if let Some(sub_path) =
                        moved_set.insert(*var, MemoryState::FullyMoved(ins.source_location()))
                    {
                        return Err(Code::MemoryError {
                            position: ins.source_location(),
                            var: Variable::from(*var),
                            typed: MemoryErrorType::Move,
                            last_used: sub_path.last_used(),
                        });
                    }
                }
                InsType::Phi { .. } => (),
                InsType::MemberReference {
                    left,
                    indices,
                    modifier,
                } => {
                    let mut overlay = overlay_hashmap![&mut moved_set, previous_moved_set];
                    let mut directory = if let Some(memory_state) = overlay.get_or_clone(*left) {
                        match memory_state {
                            MemoryState::PartiallyMoved(dir) => dir,
                            MemoryState::FullyMoved(last_used) => {
                                return Err(Code::MemoryError {
                                    position: ins.source_location(),
                                    var: Variable::from(*left),
                                    typed: MemoryErrorType::Move,
                                    last_used: *last_used,
                                });
                            }
                        }
                    } else if *modifier == ReferenceModifier::Copy {
                        continue;
                    } else if let MemoryState::PartiallyMoved(dict) = overlay
                        .map_mut()
                        .entry(*left)
                        .or_insert(MemoryState::PartiallyMoved(Directory::new(
                            ins.source_location(),
                        )))
                    {
                        dict
                    } else {
                        unreachable!()
                    };
                    for index in indices {
                        let index = match &index.var {
                            MemberExprIndexVar::StructIndex(x) => Index::Struct(*x),
                            MemberExprIndexVar::Variable(_) => Index::Dynamic,
                        };
                        // We may not move a sub path which is already moved
                        if let Some(sub_path) = directory.sub_paths.get(&index) {
                            return Err(Code::MemoryError {
                                position: ins.source_location(),
                                var: Variable::from(*left),
                                typed: MemoryErrorType::Move,
                                last_used: sub_path.last_used,
                            });
                        }
                        match modifier {
                            ReferenceModifier::Copy => {
                                // Continue checking whether we can use this sub-path
                                if let Some(maybe_dir) = directory.sub_paths.get_mut(&index) {
                                    directory = maybe_dir;
                                } else {
                                    break;
                                }
                            }
                            ReferenceModifier::Move => {
                                // Insert a new sub-path to the moved chain
                                directory
                                    .sub_paths
                                    .insert(index, Directory::new(ins.source_location()));
                                directory = directory.sub_paths.get_mut(&index).unwrap();
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
                _ => {
                    let mut error: Option<Code> = None;
                    ins.each_used_var(|var| {
                        if error.is_some() {
                            return;
                        }
                        if let Some(sub_path) =
                            overlay_hashmap![&mut moved_set, previous_moved_set].get(var.into())
                        {
                            error = Some(Code::MemoryError {
                                position: ins.source_location(),
                                var,
                                typed: MemoryErrorType::Move,
                                last_used: sub_path.last_used(),
                            });
                        }
                    });
                    if let Some(error) = error {
                        return Err(error);
                    }
                }
            }
        }
        // The new variable state is the sum of the current variable state and
        // all of the successor's variable state
        let mut delta_moved_set = Paths::new();
        let mut overlay_move_set = overlay_hashmap![&mut delta_moved_set, Some(&moved_set)];
        for &succ in &block.succs {
            if succ > block_idx {
                let new_set = walk(
                    &context.blocks[succ],
                    succ,
                    context,
                    overlay_move_set.map_imm(),
                )?;
                for (key, new_state) in new_set {
                    if let Some(old_state) = overlay_move_set.get_or_clone(key) {
                        match (old_state, new_state) {
                            (
                                MemoryState::PartiallyMoved(old_dir),
                                MemoryState::PartiallyMoved(new_dir),
                            ) => {
                                old_dir.extend(new_dir);
                            }
                            (MemoryState::PartiallyMoved(dict), MemoryState::FullyMoved(_)) => {
                                let last_used = dict.last_used;
                                overlay_move_set
                                    .map_mut()
                                    .insert(key, MemoryState::FullyMoved(last_used));
                            }
                            (MemoryState::FullyMoved(_), MemoryState::FullyMoved(_)) => (),
                            (_, _) => {
                                // These states are unreachable since we've already handled it while
                                // checking the instructions above, hopefully.
                                unreachable!();
                            }
                        }
                    } else {
                        overlay_move_set.map_mut().insert(key, new_state);
                    }
                }
            }
        }
        moved_set.extend(delta_moved_set.into_iter());
        Ok(moved_set)
    }
    match walk(&context.blocks[0], 0, context, None) {
        Ok(_) => Flow::Continue,
        Err(code) => Flow::Err(code),
    }
}
