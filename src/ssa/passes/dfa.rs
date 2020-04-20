use crate::ssa::isa::*;
use crate::utils::pipeline::Flow;
use std::collections::{BTreeMap, BTreeSet};

pub fn data_flow_analysis(context: &mut Context) -> Flow {
    // Generate the initial worklist by putting returning blocks
    let mut worklist: Vec<usize> = context
        .blocks
        .iter()
        .enumerate()
        .filter(|(_idx, block)| block.ins.iter().any(|ins| ins.typed.has_side_effects()))
        .map(|(idx, _block)| idx)
        .collect();
    dbg_println!("initial worklist: {:#?}", worklist);

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
