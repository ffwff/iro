use crate::compiler::Flow;
use crate::ssa::isa::*;
use std::collections::{BTreeMap, BTreeSet};

pub fn eliminate_phi(context: &mut Context) -> Flow {
    let mut replacements: BTreeMap<Variable, Vec<Variable>> = BTreeMap::new();
    for block in &mut context.blocks {
        for ins in &block.ins {
            let retvar = ins.retvar();
            match &ins.typed {
                InsType::Phi { vars, .. } => {
                    let retvar = retvar.unwrap();
                    for var in vars.iter() {
                        if let Some(vec) = replacements.get_mut(var) {
                            vec.push(retvar);
                        } else {
                            replacements.insert(*var, vec![retvar]);
                        }
                    }
                }
                _ => (),
            }
        }
    }
    while !replacements.is_empty() {
        for block in &mut context.blocks {
            let old_len = block.ins.len();
            let old_ins = std::mem::replace(&mut block.ins, Vec::with_capacity(old_len));

            let mut replacement_body = vec![];
            let mut block_local_replacements = btreemap![];
            for ins in old_ins {
                match &ins.typed {
                    InsType::Phi { .. } => continue,
                    _ => (),
                }
                let source_location = ins.source_location();
                let retvar = ins.retvar();
                block.ins.push(ins);
                if let Some(retvar) = retvar {
                    if let Some(new_vars) = replacements.remove(&retvar) {
                        for new_var in new_vars {
                            replacement_body.push(Ins::new(
                                new_var,
                                InsType::Move(retvar),
                                source_location,
                            ));
                            block.vars_phi.insert(new_var);
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
    for block in &mut context.blocks {
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
            .difference(&block.vars_phi)
            .cloned()
            .collect();
        let vars_imported = vars_used
            .difference(&vars_declared_in_this_block)
            .cloned()
            .collect();
        block.vars_imported = vars_imported;
        block.vars_declared_in_this_block = vars_declared_in_this_block;
        block.vars_used = vars_used;
    }
    Flow::Continue
}

pub fn calculate_data_flow(context: &mut Context) -> Flow {
    fn walk(
        block_idx: usize,
        context: &mut Context,
        prev_vars_exported: Option<&mut BTreeSet<Variable>>,
    ) {
        let mut vars_exported = btreeset![];
        let block = {
            let block = &mut context.blocks[block_idx];
            let succs = std::mem::replace(&mut block.succs, vec![]);
            for &succ in &succs {
                if succ > block_idx {
                    walk(succ, context, Some(&mut vars_exported));
                }
            }
            let block = &mut context.blocks[block_idx];
            block.succs = succs;
            block
        };
        block.vars_exported = vars_exported;
        // Imported variables are used in this block but are not declared in this block
        if let Some(prev_vars_exported) = prev_vars_exported {
            prev_vars_exported.extend(block.vars_imported.clone());
        }
    }
    walk(0, context, None);

    // Fix up step
    let mut worklist = (0..context.blocks.len()).collect::<Vec<usize>>();
    while let Some(node) = worklist.pop() {
        let block = &mut context.blocks[node];
        let mut new_vars_imported: BTreeSet<Variable> = &block.vars_exported | &block.vars_used;
        new_vars_imported = new_vars_imported
            .difference(&block.vars_declared_in_this_block)
            .cloned()
            .collect();
        dbg_println!("new_vars_imported: {:?}", new_vars_imported);

        // Borrow preds temporarily so as to not borrow multiple blocks in context.blocks
        let preds = std::mem::replace(&mut block.preds, vec![]);
        if block.vars_imported != new_vars_imported {
            for &pred in &preds {
                worklist.push(pred);
            }
            block.vars_imported = new_vars_imported.clone();
        }

        for &pred in &preds {
            let block = &mut context.blocks[pred];
            block.vars_exported.extend(new_vars_imported.iter());
        }

        // Give preds back to the current block
        let block = &mut context.blocks[node];
        block.preds = preds;
    }

    Flow::Continue
}

pub fn drop_insertion(context: &mut Context) -> Flow {
    for idx in 0..context.blocks.len() {
        let block = &mut context.blocks[idx];
        let preds = std::mem::replace(&mut block.preds, vec![]);
        let mut vars_total_imported = btreeset![];

        for &pred in &preds {
            let block = &mut context.blocks[pred];
            vars_total_imported.extend(block.vars_exported.iter().cloned());
        }

        // Give preds back to the current block
        let block = &mut context.blocks[idx];
        block.preds = preds;
        block.vars_total_imported = vars_total_imported;
    }

    for (idx, block) in context.blocks.iter_mut().enumerate() {
        // Variables that die in this block are variables which
        // flow into the block or are declared in this block, and are never exported
        let mut dead_vars = &block.vars_declared_in_this_block | &block.vars_total_imported;
        for exported in &block.vars_exported {
            dead_vars.remove(exported);
        }
        dbg_println!(
            "dead_vars for {}: {:?} {:?}, {:?}",
            idx,
            block.vars_declared_in_this_block,
            block.vars_total_imported,
            dead_vars
        );

        if !dead_vars.is_empty() {
            let old_len = block.ins.len();
            let old_ins = std::mem::replace(&mut block.ins, Vec::with_capacity(old_len));

            // Mapping of a variable which has been lifetime-extended
            // to a borrowing of that variable
            let mut lifetime_extension_mapping = btreemap![];

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
                    if ins.each_borrowed_var(|var| {
                        if dead_vars.contains(&var) {
                            lifetime_extension_mapping.insert(var, retvar);
                        } else {
                            unimplemented!()
                            // dead_vars.remove(&var);
                            // lifetime_extension_mapping.remove(&retvar);
                        }
                    }) {
                        continue;
                    }
                }
                if !ins.each_moved_var(|var| {
                    dead_var_usage.remove(&var);
                }) {
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

            dbg_println!(
                "lifetime_extension_mapping: {:?}",
                lifetime_extension_mapping
            );
            // Lifetime-extended variables are manually dropped by maybe_insert_drop
            for (&var, _) in &lifetime_extension_mapping {
                dead_var_usage.remove(&var);
            }
            let lifetime_extension_mapping_inv = lifetime_extension_mapping
                .into_iter()
                .map(|(x, y)| (y, x))
                .collect::<BTreeMap<Variable, Variable>>();

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
                lifetime_extension_mapping_inv: &BTreeMap<Variable, Variable>,
                block: &mut Block,
            ) {
                if let Some(usage) = dead_var_usage.get_mut(&var) {
                    *usage -= 1;
                    if *usage == 0 {
                        dead_var_usage.remove(&var);
                        block.ins.push(Ins::empty_ret(InsType::Drop(var), 0));
                        if let Some(to_drop) = lifetime_extension_mapping_inv.get(&var) {
                            block.ins.push(Ins::empty_ret(InsType::Drop(*to_drop), 0));
                        }
                    }
                }
            }

            // Loop through each instruction, check the variables used in it,
            // once the usage for a dead var reaches zero,
            // insert a drop instruction immediately after
            for ins in &old_ins {
                block.ins.push(ins.clone());
                ins.each_used_var(|var| {
                    maybe_insert_drop(
                        var,
                        &mut dead_var_usage,
                        &lifetime_extension_mapping_inv,
                        block,
                    );
                });
                if let Some(var) = ins.retvar() {
                    maybe_insert_drop(
                        var,
                        &mut dead_var_usage,
                        &lifetime_extension_mapping_inv,
                        block,
                    );
                }
            }
        }
    }
    dbg_println!("{}", context.print());
    Flow::Continue
}

pub fn register_to_memory(context: &mut Context) -> Flow {
    dbg_println!("before r2m: {}", context.print());

    let mut borrowed_vars: BTreeSet<Variable> = BTreeSet::new();
    for (idx, block) in context.blocks.iter_mut().enumerate() {
        for ins in &block.ins {
            match &ins.typed {
                InsType::Borrow { var, .. } => {
                    borrowed_vars.insert(*var);
                }
                _ => (),
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

                        // Map the following SSA instruction
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
