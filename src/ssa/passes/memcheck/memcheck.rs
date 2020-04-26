use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::memcheck::path::*;
use crate::utils::overlay::OverlayHashMap;

#[derive(Debug)]
struct MoveError {
    pub position: InsPosition,
}

pub fn check(context: &mut Context) -> Flow {
    fn walk(
        block: &Block,
        block_idx: usize,
        context: &Context,
        previous_moved_set: Option<&Paths>,
    ) -> Result<Paths, MoveError> {
        let mut moved_set = Paths::new();
        for (ins_idx, ins) in block.ins.iter().enumerate() {
            match &ins.typed {
                InsType::Drop(var) | InsType::Move(var) | InsType::MarkMoved(var) => {
                    // FIXME
                    moved_set.insert(*var, MemoryState::FullyMoved);
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
                            MemoryState::FullyMoved => {
                                panic!("v{} already moved", *left);
                                return Err(MoveError {
                                    position: InsPosition { block_idx, ins_idx },
                                })
                            }
                        }
                    } else if let MemoryState::PartiallyMoved(dict) = overlay
                        .map_mut()
                        .entry(*left)
                        .or_insert(MemoryState::PartiallyMoved(Directory::new()))
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
                        if directory.sub_paths.contains_key(&index) {
                            panic!("v{} already moved", *left);
                            return Err(MoveError {
                                position: InsPosition { block_idx, ins_idx },
                            });
                        }
                        match *modifier {
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
                                directory.sub_paths.insert(index, Directory::new());
                                directory = directory.sub_paths.get_mut(&index).unwrap();
                            }
                        }
                    }
                }
                _ => {
                    let mut error = None;
                    ins.each_used_var(|var| {
                        if error.is_some() {
                            return;
                        }
                        if overlay_hashmap![&mut moved_set, previous_moved_set].contains_key(var) {
                            panic!("v{} already moved", var);
                            error = Some(MoveError {
                                position: InsPosition { block_idx, ins_idx },
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
                            (MemoryState::PartiallyMoved(_), MemoryState::FullyMoved) => {
                                overlay_move_set
                                    .map_mut()
                                    .insert(key, MemoryState::FullyMoved);
                            }
                            (MemoryState::FullyMoved, MemoryState::FullyMoved) => (),
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
        Err(_) => Flow::Err,
    }
}
