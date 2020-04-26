use crate::compiler::Flow;
use crate::ssa::isa::*;
use std::collections::BTreeSet;

pub fn calculate_block_variable_usage(context: &mut Context) -> Flow {
    fn walk(
        block_idx: usize,
        context: &mut Context,
        prev_vars_exported: Option<&mut BTreeSet<usize>>,
    ) {
        let mut vars_declared_in_this_block = BTreeSet::new();
        let mut vars_used = BTreeSet::new();
        let mut vars_exported = BTreeSet::new();
        let block = &context.blocks[block_idx];
        for ins in block.ins.iter() {
            if let Some(retvar) = ins.retvar() {
                vars_declared_in_this_block.insert(retvar);
            }
            ins.each_used_var(|used| {
                vars_used.insert(used);
            });
        }
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
        // Imported variables are used in this block but are not declared in this block
        if let Some(prev_vars_exported) = prev_vars_exported {
            let imported = vars_used.difference(&vars_declared_in_this_block).cloned();
            prev_vars_exported.extend(imported);
        }
        // Block locals are declared in this block but are not exported
        let vars_block_local = vars_declared_in_this_block
            .difference(&vars_exported)
            .cloned()
            .collect();
        // Set block variables
        block.vars_declared_in_this_block = vars_declared_in_this_block;
        block.vars_used = vars_used;
        block.vars_exported = vars_exported;
        block.vars_block_local = vars_block_local;
    }
    walk(0, context, None);
    Flow::Continue
}

pub fn drop_insertion(context: &mut Context) -> Flow {
    for (idx, block) in context.blocks.iter_mut().enumerate() {
        // Variables that die in this block are variables which are
        // used/declared in the block and are never exported
        let dead_vars_in_this_block = block.vars_used
            .difference(&block.vars_exported)
            .chain(&block.vars_block_local)
            .cloned()
            .collect::<BTreeSet<Variable>>();
        dbg_println!(
            "{}: -> dead vars: {:?}",
            idx,
            dead_vars_in_this_block
        );
        if !dead_vars_in_this_block.is_empty() {
            let old_ins = std::mem::replace(&mut block.ins, vec![]);
            // Calculate the usage for each dead var
            let mut dead_var_usage = btreemap![];
            for &var in &dead_vars_in_this_block {
                dead_var_usage.insert(var, 0);
            }
            for ins in &old_ins {
                if let Some(var) = ins.retvar() {
                    if let Some(usage) = dead_var_usage.get_mut(&var) {
                        *usage += 1;
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
            block.postlude.each_used_var(|var| {
                dead_var_usage.remove(&var);
            });
            // Loop through each instruction, check the variables used in it,
            // once the usage for a dead var reaches zero,
            // insert a drop instruction immediately after
            for ins in &old_ins {
                block.ins.push(ins.clone());
                ins.each_used_var(|var| {
                    if let Some(usage) = dead_var_usage.get_mut(&var) {
                        *usage -= 1;
                        if *usage == 0 {
                            dead_var_usage.remove(&var);
                            block.ins.push(Ins::new(0, InsType::Drop(var)));
                        }
                    }
                });
                if let Some(var) = ins.retvar() {
                    if let Some(usage) = dead_var_usage.get_mut(&var) {
                        *usage -= 1;
                        if *usage == 0 {
                            dead_var_usage.remove(&var);
                            block.ins.push(Ins::new(0, InsType::Drop(var)));
                        }
                    }
                }
            }
        }
    }
    dbg_println!("{}", context.print());
    Flow::Continue
}
