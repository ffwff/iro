use crate::ssa::isa::*;
use crate::utils::pipeline::Flow;
use std::collections::{BTreeMap, BTreeSet};

pub fn rename_vars_and_insert_phis(context: &mut Context) -> Flow {
    let mut defsites: Vec<BTreeSet<usize>> = vec![btreeset![]; context.variables.len()];
    let num_blocks = context.blocks.len();

    if num_blocks > 1 {
        for (idx, block) in context.blocks.iter_mut().enumerate() {
            for ins in &block.ins {
                if let Some(retvar) = ins.retvar() {
                    let set = &mut defsites[retvar];
                    set.insert(idx);
                }
            }
            block.vars_block_local = block
                .vars_declared_in_this_block
                .difference(&block.vars_out)
                .cloned()
                .collect();
        }

        // Build the post-order traversal array
        let mut rpo = Vec::with_capacity(context.blocks.len());
        {
            fn walk(node: usize, context: &Context, post_order: &mut Vec<usize>) {
                let block = &context.blocks[node];
                for succ in &block.succs {
                    // Since this is a DFS tree, all non-looping descendents
                    // must have larger indices that its parent
                    if *succ > node {
                        walk(*succ, context, post_order);
                    }
                }
                post_order.push(node);
            }
            walk(0, &context, &mut rpo);
        }

        let rpo_ordering: Vec<usize> = {
            let mut zipped: Vec<(usize, usize)> = rpo
                .iter()
                .enumerate()
                .map(|(idx, node)| (*node, idx))
                .collect();
            zipped.sort_by_key(|(k, _v)| *k);
            zipped.into_iter().map(|(_idx, node)| node).collect()
        };
        rpo.reverse();

        // Calculate the dominators for each block
        // Reference: https://www.doc.ic.ac.uk/~livshits/classes/CO444H/reading/dom14.pdf
        let mut doms: BTreeMap<usize, usize> = btreemap![ 0 => 0 ];
        let mut changed = true;
        while changed {
            changed = false;
            for &b in rpo.iter().skip(1) {
                // Skip first block
                let intersect = |mut b1: usize, mut b2: usize| {
                    while b1 != b2 {
                        while rpo_ordering[b1] < rpo_ordering[b2] {
                            b1 = doms[&b1];
                        }
                        while rpo_ordering[b1] > rpo_ordering[b2] {
                            b2 = doms[&b2];
                        }
                    }
                    b1
                };
                let new_idom = context.blocks[b]
                    .preds
                    .iter()
                    .filter(|p| doms.contains_key(&p))
                    .fold(None, |last, &curr| match last {
                        None => Some(curr),
                        Some(last) => Some(intersect(curr, last)),
                    })
                    .unwrap();
                if !doms.contains_key(&b) || doms[&b] != new_idom {
                    doms.insert(b, new_idom);
                    changed = true;
                }
            }
        }

        // Calculate the dominance tree
        let mut dom_tree: Vec<Vec<usize>> = vec![vec![]; num_blocks];
        for (&idx, &dominator) in &doms {
            if idx == dominator {
                continue;
            }
            dom_tree[dominator].push(idx);
        }

        dbg_println!("dom_tree: {:#?}", dom_tree);

        // Find the dominance frontier for each block
        let mut dom_frontier: Vec<BTreeSet<usize>> = vec![btreeset![]; num_blocks];
        for (b, blocks) in context.blocks.iter_mut().enumerate() {
            if blocks.preds.len() >= 2 {
                for &p in &blocks.preds {
                    let mut runner: usize = p;
                    while runner != doms[&b] {
                        dbg_println!(
                            "dom runner: {:?} {:?} {:?} {:?}",
                            runner,
                            b,
                            doms[&b],
                            doms[&runner]
                        );
                        dom_frontier[runner].insert(b);
                        runner = doms[&runner];
                    }
                }
            }
        }
        dbg_println!("---\ndom_frontier: {:#?}", dom_frontier);
        dbg_println!("---\ndefsites: {:#?}", defsites);

        let mut origin: Vec<Vec<usize>> = vec![vec![]; num_blocks];
        for (var, defsites) in defsites.iter().enumerate() {
            for defsite in defsites {
                origin[*defsite].push(var);
            }
        }

        // Insert phi nodes
        for (var, defsites) in defsites.iter_mut().enumerate() {
            let mut worklist: Vec<usize> = defsites.iter().cloned().collect();
            let mut phi_inserted = btreeset![];
            while let Some(n) = worklist.pop() {
                for &y in &dom_frontier[n] {
                    if !phi_inserted.contains(&y) {
                        let block = &mut context.blocks[y];
                        if !block.vars_block_local.contains(&var) {
                            block.ins.insert(
                                0,
                                Ins::new(
                                    var,
                                    InsType::Phi {
                                        vars: std::iter::repeat(var)
                                            .take(block.preds.len())
                                            .collect(),
                                        defines: var,
                                    },
                                ),
                            );
                        }
                        phi_inserted.insert(y);
                        if !origin[y].contains(&var) {
                            worklist.push(y);
                        }
                    }
                }
            }
        }

        // Rename variables
        // Reference: https://iith.ac.in/~ramakrishna/fc5264/ssa-intro-construct.pdf
        fn rename_variables(
            var: usize,
            node: usize,
            version: &mut usize,
            version_stack: &mut Vec<usize>,
            context: &mut Context,
            dom_tree: &Vec<Vec<usize>>,
        ) {
            dbg_println!("use node {}", node);
            let version_start = version_stack.last().unwrap().clone();
            let mut new_variable_len = context.variables.len();
            let tmp_succs = {
                let block = &mut context.blocks[node];
                let mut has_block_version = false;
                for ins in &mut block.ins {
                    if !ins.typed.is_phi() {
                        ins.rename_var(var, *version_stack.last().clone().unwrap());
                    }
                    if let Some(retvar) = ins.mut_retvar() {
                        if *retvar == var {
                            if has_block_version {
                                *retvar = new_variable_len;
                                *version_stack.last_mut().unwrap() = new_variable_len;
                            } else if *version > 1 {
                                *retvar = new_variable_len;
                                version_stack.push(new_variable_len);
                                has_block_version = true;
                            }
                            dbg_println!("new: {:#?}", *retvar);
                            new_variable_len += 1;
                            *version += 1;
                        }
                    }
                }
                std::mem::replace(&mut block.succs, vec![])
            };
            {
                let typed = context.variables[var].clone();
                let curr_len = context.variables.len();
                for _ in curr_len..new_variable_len {
                    context.variables.push(typed.clone());
                }
            }
            for &succ in &tmp_succs {
                // j is predecessor index of node wrt succ
                let succ_block: &mut Block = &mut context.blocks[succ];
                let j = succ_block
                    .preds
                    .iter()
                    .position(|&pred| pred == node)
                    .unwrap();
                for ins in &mut succ_block.ins {
                    match &mut ins.typed {
                        InsType::Phi { vars, defines } => {
                            if *defines == var {
                                vars[j] = *version_stack.last().unwrap();
                                dbg_println!("rename phi {} = {:?}", var, vars);
                            }
                        }
                        _ => (),
                    }
                }
            }
            {
                let block = &mut context.blocks[node];
                block.succs = tmp_succs;
            }
            for &dominated in &dom_tree[node] {
                rename_variables(var, dominated, version, version_stack, context, dom_tree);
            }
            while *version_stack.last().unwrap() != version_start {
                version_stack.pop();
            }
        }
        let old_len = context.variables.len();
        for var in 0..old_len {
            let mut version_stack = vec![var];
            let mut version = 1;
            rename_variables(var, 0, &mut version, &mut version_stack, context, &dom_tree);
            // dbg_println!("rename var {} ==> {:#?}", var, context);
        }
    } else {
        let mut mapping: Vec<Option<usize>> = (0..context.variables.len()).map(|_| None).collect();
        if mapping.is_empty() {
            return Flow::Continue;
        }
        let block = &mut context.blocks[0];
        for ins in &mut block.ins {
            ins.rename_var_by(|var| mapping[var].unwrap());
            if let Some(retvar) = ins.mut_retvar() {
                if mapping[*retvar] == None {
                    mapping[*retvar] = Some(*retvar);
                } else {
                    let newlen = context.variables.len();
                    let typed = context.variables[*retvar].clone();
                    context.variables.push(typed);
                    mapping[*retvar] = Some(newlen);
                    *retvar = newlen;
                }
            }
        }
    }

    Flow::Continue
}

pub fn eliminate_phi(context: &mut Context) -> Flow {
    if context.blocks.len() < 2 {
        return Flow::Continue;
    }

    let mut replacements: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    for block in &context.blocks {
        for ins in &block.ins {
            let retvar = ins.retvar();
            match &ins.typed {
                InsType::Phi { vars, .. } => {
                    let retvar = retvar.unwrap();
                    for var in vars {
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
            let oldins = std::mem::replace(&mut block.ins, Vec::new());
            for ins in &oldins {
                match &ins.typed {
                    InsType::Phi { .. } => continue,
                    _ => (),
                }
                block.ins.push(ins.clone());
                if let Some(retvar) = ins.retvar() {
                    if let Some(newvars) = replacements.remove(&retvar) {
                        for newvar in newvars {
                            block.ins.push(Ins::new(newvar, InsType::Move(retvar)));
                        }
                    }
                }
            }
        }
    }
    Flow::Continue
}
