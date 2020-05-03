use crate::compiler::Flow;
use crate::ssa::isa::*;
use smallvec::SmallVec;
use std::collections::{BTreeMap, BTreeSet};

pub fn eliminate_phi(context: &mut Context) -> Flow {
    if context.blocks.len() < 2 {
        return Flow::Continue;
    }

    let mut replacements: BTreeMap<Variable, SmallVec<[Variable; 4]>> = BTreeMap::new();
    for block in &mut context.blocks {
        for ins in &block.ins {
            let retvar = ins.retvar();
            if let InsType::Phi { vars, .. } = &ins.typed {
                let retvar = retvar.unwrap();
                for var in vars.iter() {
                    if let Some(vec) = replacements.get_mut(var) {
                        vec.push(retvar);
                    } else {
                        replacements.insert(*var, smallvec![retvar]);
                    }
                }
            }
        }
    }
    while !replacements.is_empty() {
        for (block, block_vars) in context.blocks.iter_mut().zip(context.block_vars.iter_mut()) {
            let old_len = block.ins.len();
            let old_ins = std::mem::replace(&mut block.ins, Vec::with_capacity(old_len));

            let mut replacement_body = vec![];
            let mut block_local_replacements = btreemap![];
            for ins in old_ins {
                if let InsType::Phi { .. } = &ins.typed {
                    continue;
                }
                let source_location = ins.source_location();
                let retvar = ins.retvar();
                block.ins.push(ins);
                if let Some(retvar) = retvar {
                    let copyable = context.variables[usize::from(retvar)].is_copyable();
                    if let Some(new_vars) = replacements.remove(&retvar) {
                        for new_var in new_vars {
                            replacement_body.push(Ins::new(
                                new_var,
                                if copyable {
                                    InsType::Copy(retvar)
                                } else {
                                    InsType::Move(retvar)
                                },
                                source_location,
                            ));
                            block_vars.vars_phi.insert(new_var);
                            block_local_replacements.insert(retvar, new_var);
                        }
                    }
                }
            }
            block.ins.extend(replacement_body.into_iter());
            block.postlude.as_mut().unwrap().rename_var_by(|var| {
                if let Some(replaced) = block_local_replacements.get(&var) {
                    *replaced
                } else {
                    var
                }
            });
        }
    }
    Flow::Continue
}

pub fn calculate_block_variable_declaration(context: &mut Context) -> Flow {
    for (block, block_vars) in context.blocks.iter_mut().zip(context.block_vars.iter_mut()) {
        let mut vars_declared_in_this_block = BTreeSet::new();
        let mut vars_used = BTreeSet::new();
        for ins in block.ins.iter() {
            if let Some(retvar) = ins.retvar() {
                vars_declared_in_this_block.insert(retvar);
            }
            ins.each_used_var(|used| {
                vars_used.insert(used);
            });
        }
        // Phi assignments are not declared in this block
        vars_declared_in_this_block = vars_declared_in_this_block
            .difference(&block_vars.vars_phi)
            .cloned()
            .collect();
        let vars_imported = vars_used
            .difference(&vars_declared_in_this_block)
            .cloned()
            .collect();
        block_vars.vars_imported = vars_imported;
        block_vars.vars_declared_in_this_block = vars_declared_in_this_block;
        block_vars.vars_used = vars_used;
    }
    Flow::Continue
}

pub fn calculate_data_flow(context: &mut Context) -> Flow {
    if context.blocks.len() < 2 {
        return Flow::Continue;
    }

    fn walk(
        block_idx: usize,
        blocks: &[Block],
        block_vars: &mut Vec<BlockVars>,
        prev_vars_exported: Option<&mut BTreeSet<Variable>>,
    ) {
        let mut vars_exported = btreeset![];
        let block = &blocks[block_idx];
        for &succ in &block.succs {
            if succ > block_idx {
                walk(succ, blocks, block_vars, Some(&mut vars_exported));
            }
        }
        let block_vars = &mut block_vars[block_idx];
        block_vars.vars_exported = vars_exported;
        // Imported variables are used in this block but are not declared in this block
        if let Some(prev_vars_exported) = prev_vars_exported {
            prev_vars_exported.extend(block_vars.vars_imported.iter().cloned());
        }
    }
    walk(0, &context.blocks, &mut context.block_vars, None);

    // Fix up step
    let mut worklist = (0..context.blocks.len()).collect::<Vec<usize>>();
    while let Some(node) = worklist.pop() {
        let block = &mut context.blocks[node];
        let block_vars = &mut context.block_vars[node];
        let mut new_vars_imported: BTreeSet<Variable> =
            &block_vars.vars_exported | &block_vars.vars_used;
        new_vars_imported = new_vars_imported
            .difference(&block_vars.vars_declared_in_this_block)
            .cloned()
            .collect();
        dbg_println!("new_vars_imported: {:?}", new_vars_imported);

        // Borrow preds temporarily so as to not borrow multiple blocks in context.blocks
        if block_vars.vars_imported != new_vars_imported {
            for &pred in &block.preds {
                worklist.push(pred);
            }
            block_vars.vars_imported = new_vars_imported.clone();
        }

        for &pred in &block.preds {
            let pred_block_vars = &mut context.block_vars[pred];
            pred_block_vars
                .vars_exported
                .extend(new_vars_imported.iter());
        }
    }

    Flow::Continue
}

pub fn reference_drop_insertion(context: &mut Context) -> Flow {
    for idx in 0..context.blocks.len() {
        let block = &mut context.blocks[idx];
        let mut vars_total_imported = btreeset![];

        for &pred in &block.preds {
            let pred_block_vars = &mut context.block_vars[pred];
            vars_total_imported.extend(pred_block_vars.vars_exported.iter().cloned());
        }

        let block_vars = &mut context.block_vars[idx];
        block_vars.vars_total_imported = vars_total_imported;
    }

    for (block, block_vars) in context
        .blocks
        .iter_mut()
        .zip(context.block_vars.iter())
    {
        // Variables that die in this block are variables which
        // flow into the block or are declared in this block, and are never exported
        let mut dead_vars = block_vars.vars_declared_in_this_block
            .union(&block_vars.vars_total_imported)
            .cloned()
            .collect::<Vec<_>>();
        let context_variables = &context.variables;
        dead_vars.retain(move |var| {
            if block_vars.vars_exported.contains(&var) {
                false
            } else if !context_variables[usize::from(*var)].is_value_type() {
                // We should only attempt NLL for non-value-types, (aka pointers)
                // anything else should already be dropped when scope ends
                true
            } else {
                false
            }
        });

        if !dead_vars.is_empty() {
            let old_len = block.ins.len();
            let old_ins = std::mem::replace(&mut block.ins, Vec::with_capacity(old_len));

            // Calculate the usage for each dead var
            let mut dead_var_usage = btreemap![];
            for &var in &dead_vars {
                dead_var_usage.insert(var, 0);
            }
            for ins in &old_ins {
                if let Some(retvar) = ins.retvar() {
                    if let Some(usage) = dead_var_usage.get_mut(&retvar) {
                        *usage += 1;
                    }
                }
                let moved = ins.each_moved_var(|var| {
                    dead_var_usage.remove(&var);
                });
                if !moved {
                    ins.each_used_var(|var| {
                        if let Some(usage) = dead_var_usage.get_mut(&var) {
                            *usage += 1;
                        }
                    });
                }
            }

            // Postlude instruction does not count towards usage
            block.postlude.as_mut().unwrap().each_used_var(|var| {
                dead_var_usage.remove(&var);
            });

            // Remove all vars with zero count first
            for var in &dead_vars {
                if let Some(usage) = dead_var_usage.get(var).cloned() {
                    if usage == 0 {
                        dead_var_usage.remove(var);
                        block.ins.push(Ins::empty_ret(InsType::Drop(*var), 0));
                    }
                }
            }

            fn maybe_insert_drop(
                var: Variable,
                dead_var_usage: &mut BTreeMap<Variable, i32>,
                block: &mut Block,
            ) {
                if let Some(usage) = dead_var_usage.get_mut(&var) {
                    *usage -= 1;
                    if *usage == 0 {
                        dead_var_usage.remove(&var);
                        block.ins.push(Ins::empty_ret(InsType::Drop(var), 0));
                    }
                }
            }

            // Loop through each instruction, check the variables used in it,
            // once the usage for a dead var reaches zero,
            // insert a drop instruction immediately after
            for ins in &old_ins {
                block.ins.push(ins.clone());
                ins.each_used_var(|var| {
                    maybe_insert_drop(var, &mut dead_var_usage, block);
                });
                if let Some(var) = ins.retvar() {
                    maybe_insert_drop(var, &mut dead_var_usage, block);
                }
            }
        }
    }
    dbg_println!("{}", context.print());
    Flow::Continue
}

pub fn register_to_memory(context: &mut Context) -> Flow {
    dbg_println!("before r2m: {}", context.print());

    // Turn primitive variables with borrows into memory registers
    let mut borrowed_vars: BTreeSet<Variable> = BTreeSet::new();
    for block in context.blocks.iter_mut() {
        for ins in &block.ins {
            if let InsType::Borrow { var, .. } = &ins.typed {
                borrowed_vars.insert(*var);
            }
        }
    }
    if borrowed_vars.is_empty() {
        return Flow::Continue;
    }
    let borrowed_vars = borrowed_vars
        .into_iter()
        .filter(|var| context.variable(*var).is_primitive())
        .collect::<Vec<Variable>>();
    for &var in &borrowed_vars {
        let mut declared = false;
        let mut new_var_count = context.variables.len();
        for block in &mut context.blocks {
            let old_len = block.ins.len();
            let old_ins = std::mem::replace(&mut block.ins, Vec::with_capacity(old_len));

            for mut ins in old_ins {
                let location = ins.source_location();
                match ins.typed {
                    InsType::Drop(dropped) => {
                        if dropped == var {
                            block.ins.push(ins);
                            continue;
                        }
                    }
                    InsType::Borrow {
                        var: borrowed_var, ..
                    } => {
                        if borrowed_var == var {
                            assert_ne!(ins.retvar().unwrap(), borrowed_var);
                            block.ins.push(ins);
                            continue;
                        }
                    }
                    _ => (),
                }
                ins.rename_var_by(|ren_var| {
                    if ren_var == var {
                        // Rewrite any other vars used with new vars,
                        // loaded from the source var
                        let new_var = Variable::from(new_var_count);
                        new_var_count += 1;
                        block
                            .ins
                            .push(Ins::new(new_var, InsType::Load(var), location));
                        new_var
                    } else {
                        ren_var
                    }
                });
                if let Some(retvar) = ins.mut_retvar() {
                    if *retvar == var {
                        if !declared {
                            block.ins.push(Ins::new(var, InsType::Alloca, location));
                            declared = true;
                        }

                        // Transform the following SSA instruction
                        //   v0 = ...
                        // into
                        //   v? = ...
                        //   store v? -> v0
                        let new_var = Variable::from(new_var_count);
                        new_var_count += 1;
                        *retvar = new_var;

                        block.ins.push(ins);
                        block.ins.push(Ins::empty_ret(
                            InsType::Store {
                                source: new_var,
                                dest: var,
                            },
                            location,
                        ));

                        continue;
                    }
                }
                block.ins.push(ins);
            }
        }
        let typed = context.variable(var).clone();
        for _ in context.variables.len()..new_var_count {
            context.variables.push(typed.clone());
        }
    }

    dbg_println!("after r2m: {}", context.print());
    Flow::Continue
}
