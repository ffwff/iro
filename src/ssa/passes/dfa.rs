use crate::compiler::Flow;
use crate::ssa::isa::*;
use std::collections::BTreeSet;

pub fn data_flow_analysis(context: &mut Context) -> Flow {
    // Generate the initial worklist by putting returning blocks
    let mut worklist: Vec<usize> = (1..context.blocks.len()).rev().collect();

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
        // Reset every variable flow information
        block.vars_in = btreeset![];
        block.vars_out = btreeset![];
        block.vars_declared_in_this_block = vars_declared_in_this_block;
        block.vars_used = vars_used;
        block.vars_block_local = btreeset![];
    }

    while let Some(node) = worklist.pop() {
        let block = &mut context.blocks[node];
        let mut new_vars_in: BTreeSet<usize> = &block.vars_out | &block.vars_used;
        new_vars_in = new_vars_in
            .difference(&block.vars_declared_in_this_block)
            .cloned()
            .collect();
        dbg_println!("new_vars_in: {:?}", new_vars_in);
        // Borrow preds temporarily so as to not borrow multiple blocks in context.blocks
        let preds = std::mem::replace(&mut block.preds, vec![]);
        if block.vars_in != new_vars_in {
            for &pred in &preds {
                worklist.push(pred);
            }
            block.vars_in = new_vars_in.clone();
        }
        std::mem::drop(block);
        for &pred in &preds {
            let block = &mut context.blocks[pred];
            block.vars_out.extend(new_vars_in.iter());
        }
        // Give preds back to the current block
        {
            let block = &mut context.blocks[node];
            block.preds = preds;
        }
    }

    // dbg_println!("after dfa: {:#?}", context);
    Flow::Continue
}

pub fn drop_insertion(context: &mut Context) -> Flow {
    for (idx, block) in context.blocks.iter_mut().enumerate() {
        let dead_vars_in_this_block = (&block.vars_in | &block.vars_declared_in_this_block)
            .difference(&block.vars_out)
            .cloned()
            .collect::<BTreeSet<Variable>>();
        dbg_println!(
            "{}: {:?} -> dead vars: {:?}",
            idx,
            block.vars_out,
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
