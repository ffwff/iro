use crate::compiler::error::{Code, MemoryErrorType};
use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::memcheck::drop_effect::*;
use crate::ssa::passes::memcheck::path::*;
use crate::utils::overlay::OverlayHashMap;
use fnv::FnvHashMap;

pub fn check(context: &mut Context) -> Flow {
    fn walk(
        block: &Block,
        block_idx: usize,
        context: &Context,
        previous_mem_state: Option<&Paths>,
        drops: &mut Drops,
    ) -> Result<Paths, Code> {
        let mut mem_state = Paths::default();
        for ins in &block.ins {
            match &ins.typed {
                InsType::Drop(var) => {
                    if let Some(state) = mem_state
                        .insert(*var, MemoryState::FullyMoved(ins.source_location()))
                        .map(|x| x.as_opt())
                        .flatten()
                    {
                        return Err(Code::MemoryError {
                            position: ins.source_location(),
                            var: Variable::from(*var),
                            typed: MemoryErrorType::Move,
                            last_used: state.last_used(),
                        });
                    }
                    if let Some(drop_effect) = drops.remove(var) {
                        dbg_println!("dropping {:?}", var);
                        match drop_effect {
                            DropEffect::UnbindBorrowed { borrower, target } => {
                                let mem_state_ref = mem_state.get_mut(&target).unwrap();
                                let old = std::mem::replace(mem_state_ref, MemoryState::None);
                                dbg_println!(" old {:?}", old);
                                *mem_state_ref = old.unborrow(borrower);
                                dbg_println!("  => {:?}", *mem_state_ref);
                            }
                            DropEffect::UnbindBorrowedMut {
                                borrower: _,
                                target,
                            } => {
                                let mem_state_ref = mem_state.get_mut(&target).unwrap();
                                *mem_state_ref = MemoryState::None;
                            }
                        }
                    }
                }
                InsType::Copy(var) => {
                    let retvar = ins.retvar().unwrap();
                    if let Some(old) = drops.get(&var).cloned() {
                        drops.insert(retvar, old);
                    }
                },
                InsType::Move(var) | InsType::MarkMoved(var) => {
                    if let Some(state) = mem_state
                        .insert(*var, MemoryState::FullyMoved(ins.source_location()))
                        .map(|x| x.as_opt())
                        .flatten()
                    {
                        return Err(Code::MemoryError {
                            position: ins.source_location(),
                            var: Variable::from(*var),
                            typed: MemoryErrorType::Move,
                            last_used: state.last_used(),
                        });
                    }
                    match ins.typed {
                        InsType::Move(var) => {
                            let retvar = ins.retvar().unwrap();
                            if let Some(old) = drops.remove(&var) {
                                drops.insert(retvar, old);
                            }
                        }
                        InsType::MarkMoved(var) => {
                            drops.remove(&var);
                        }
                        _ => unreachable!(),
                    }
                }
                InsType::Phi { .. } => (),
                InsType::MemberReference {
                    left,
                    indices,
                    modifier,
                } => {
                    let mut overlay = overlay_hashmap![&mut mem_state, previous_mem_state];
                    let mut directory = if let Some(memory_state) = overlay.get_or_clone(*left) {
                        match memory_state {
                            MemoryState::PartiallyMoved(dir) => dir,
                            _ => {
                                return Err(Code::MemoryError {
                                    position: ins.source_location(),
                                    var: Variable::from(*left),
                                    typed: MemoryErrorType::Move,
                                    last_used: memory_state.last_used(),
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
                    for index in indices.iter() {
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
                                last_used: LastUsed::One(sub_path.last_used),
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
                InsType::Borrow { var, modifier } => {
                    let retvar = ins.retvar().unwrap();
                    match modifier {
                        BorrowModifier::Immutable => {
                            if let Some(state) =
                                overlay_hashmap![&mut mem_state, previous_mem_state]
                                    .get_or_clone(*var)
                                    .map(|x| x.as_opt_mut())
                                    .flatten()
                            {
                                if let MemoryState::FullyBorrowed(map) = state {
                                    map.insert(*var, ins.source_location());
                                } else {
                                    return Err(Code::MemoryError {
                                        position: ins.source_location(),
                                        var: *var,
                                        typed: MemoryErrorType::Borrow,
                                        last_used: state.last_used(),
                                    });
                                }
                            } else {
                                mem_state.insert(
                                    *var,
                                    MemoryState::FullyBorrowed({
                                        let mut hashmap = FnvHashMap::with_capacity_and_hasher(
                                            1,
                                            Default::default(),
                                        );
                                        hashmap.insert(retvar, ins.source_location());
                                        hashmap
                                    }),
                                );
                            }
                            drops.insert(
                                retvar,
                                DropEffect::UnbindBorrowed {
                                    borrower: retvar,
                                    target: *var,
                                },
                            );
                        }
                        BorrowModifier::Mutable => {
                            if let Some(state) = mem_state
                                .insert(*var, MemoryState::FullyBorrowedMut(ins.source_location()))
                                .map(|x| x.as_opt())
                                .flatten()
                            {
                                return Err(Code::MemoryError {
                                    position: ins.source_location(),
                                    var: Variable::from(*var),
                                    typed: MemoryErrorType::Borrow,
                                    last_used: state.last_used(),
                                });
                            }
                            drops.insert(
                                retvar,
                                DropEffect::UnbindBorrowedMut {
                                    borrower: retvar,
                                    target: *var,
                                },
                            );
                        }
                        _ => unreachable!(),
                    }
                }
                _ => {
                    let mut error: Option<Code> = None;
                    ins.each_used_var(|var| {
                        if error.is_some() {
                            return;
                        }
                        if let Some(state) = overlay_hashmap![&mut mem_state, previous_mem_state]
                            .get(var.into())
                            .map(|x| x.as_opt_ref())
                            .flatten()
                        {
                            error = Some(Code::MemoryError {
                                position: ins.source_location(),
                                var,
                                typed: MemoryErrorType::Move,
                                last_used: state.last_used(),
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
        let mut delta_mem_state = Paths::default();
        let mut overlay_move_set = overlay_hashmap![&mut delta_mem_state, Some(&mem_state)];
        for &succ in &block.succs {
            if succ > block_idx {
                let new_set = walk(
                    &context.blocks[succ],
                    succ,
                    context,
                    overlay_move_set.map_imm(),
                    drops,
                )?;
                for (key, new_state) in new_set {
                    if let Some(old_state) = overlay_move_set.get_or_clone(key) {
                        match (old_state, new_state) {
                            (MemoryState::None, new_state) => {
                                overlay_move_set.map_mut().insert(key, new_state);
                            }
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
                            (x, y) => {
                                // These states are unreachable since we've already handled it while
                                // checking the instructions above, hopefully.
                                unreachable!("unhandled state transition {:?} -> {:?}", x, y);
                            }
                        }
                    } else if let Some(new_state) = new_state.as_opt() {
                        overlay_move_set.map_mut().insert(key, new_state);
                    } else {
                        overlay_move_set.map_mut().remove(&key);
                    }
                }
            }
        }
        mem_state.extend(delta_mem_state.into_iter());
        Ok(mem_state)
    }
    let mut drops = Drops::default();
    match walk(&context.blocks[0], 0, context, None, &mut drops) {
        Ok(_) => Flow::Continue,
        Err(code) => Flow::Err(code),
    }
}
