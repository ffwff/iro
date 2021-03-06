use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::ContextLocalData;
use smallvec::SmallVec;
use std::collections::BTreeSet;

pub fn rename_vars_and_insert_phis(_: &mut ContextLocalData, context: &mut Context) -> Flow {
    let num_blocks = context.blocks.len();

    if num_blocks > 1 {
        let mut defsites: Vec<BTreeSet<usize>> = vec![btreeset![]; context.variables.len()];

        for (idx, (block, block_vars)) in context
            .blocks
            .iter()
            .zip(context.block_vars.iter_mut())
            .enumerate()
        {
            let mut vars_used = BTreeSet::new();
            for ins in &block.ins {
                if let Some(retvar) = ins.retvar() {
                    let set = &mut defsites[usize::from(retvar)];
                    set.insert(idx);
                }
                ins.each_used_var(|used| {
                    vars_used.insert(used);
                });
            }
            block_vars.vars_used = vars_used;
        }

        // Build the post-order traversal array
        let mut rpo = Vec::with_capacity(context.blocks.len());
        {
            fn walk(node: usize, context: &Context, post_order: &mut Vec<usize>) {
                let block = &context.blocks[node];
                for &succ in &block.succs {
                    // Since this is a DFS tree, all non-looping descendents
                    // must have larger indices that its parent
                    if succ > node {
                        walk(succ, context, post_order);
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
        let mut doms = fnv_hashmap![ 0 => 0 ];

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
        let mut dom_tree: Vec<SmallVec<[usize; 2]>> = vec![smallvec![]; num_blocks];
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

        // Calculate block-local variables
        'outer: for (idx, defsite) in defsites.iter_mut().enumerate() {
            let mut count = 0;
            for block_vars in &context.block_vars {
                if block_vars.vars_used.contains(&Variable::from(idx)) {
                    count += 1;
                    if count == 2 {
                        continue 'outer;
                    }
                }
            }
            debug_assert!(count <= 1);
            // We will not insert phi's for block-local variables
            defsite.clear();
        }

        let mut origin: Vec<SmallVec<[Variable; 4]>> = vec![smallvec![]; num_blocks];
        for (var, defsites) in defsites.iter().enumerate() {
            for defsite in defsites {
                origin[*defsite].push(Variable::from(var));
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
                        block.ins.insert(
                            0,
                            Ins::new(
                                Variable::from(var),
                                InsType::Phi {
                                    vars: std::iter::repeat(Variable::from(var))
                                        .take(block.preds.len())
                                        .collect(),
                                    defines: Variable::from(var),
                                },
                                0,
                            ),
                        );
                        phi_inserted.insert(y);
                        if !origin[y].contains(&Variable::from(var)) {
                            worklist.push(y);
                        }
                    }
                }
            }
        }

        // Rename variables
        // Reference: https://iith.ac.in/~ramakrishna/fc5264/ssa-intro-construct.pdf
        fn rename_variables(
            var: Variable,
            node: usize,
            version: &mut usize,
            version_stack: &mut Vec<usize>,
            context: &mut Context,
            dom_tree: &[SmallVec<[usize; 2]>],
        ) {
            dbg_println!("use node {}", node);
            let version_start = *version_stack.last().unwrap();
            let mut new_variable_len = context.variables.len();
            let tmp_succs = {
                let block = &mut context.blocks[node];
                let mut has_block_version = false;
                for ins in &mut block.ins {
                    if !ins.typed.is_phi() {
                        ins.rename_var(var, Variable::from(*version_stack.last().clone().unwrap()));
                    }
                    if let Some(retvar) = ins.mut_retvar() {
                        if *retvar == var {
                            if has_block_version {
                                *retvar = Variable::from(new_variable_len);
                                *version_stack.last_mut().unwrap() = new_variable_len;
                            } else if *version > 1 {
                                *retvar = Variable::from(new_variable_len);
                                version_stack.push(new_variable_len);
                                has_block_version = true;
                            }
                            dbg_println!("new: {:#?}", *retvar);
                            new_variable_len += 1;
                            *version += 1;
                        }
                    }
                }
                std::mem::replace(&mut block.succs, smallvec![])
            };
            {
                let typed = context.variable(var).clone();
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
                    if let InsType::Phi { vars, defines } = &mut ins.typed {
                        if *defines == var {
                            vars[j] = Variable::from(*version_stack.last().unwrap());
                            dbg_println!("rename phi {} = {:?}", var, vars);
                        }
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
            rename_variables(
                Variable::with_u32(var as u32),
                0,
                &mut version,
                &mut version_stack,
                context,
                &dom_tree,
            );
        }

        // Deduplicate phi variables
        for block in &mut context.blocks {
            for ins in &mut block.ins {
                if let InsType::Phi { vars, .. } = &mut ins.typed {
                    let mut old_vars =
                        std::mem::replace(vars, vec![].into_boxed_slice()).into_vec();
                    old_vars.sort_unstable();
                    old_vars.dedup();
                    *vars = old_vars.into_boxed_slice();
                }
            }
        }
    } else {
        let mut mapping: Vec<Option<Variable>> = std::iter::repeat(None)
            .take(context.variables.len())
            .collect();
        let block = &mut context.blocks[0];
        for ins in &mut block.ins {
            ins.rename_var_by(|var| mapping[usize::from(var)].unwrap());
            if let Some(retvar) = ins.mut_retvar() {
                let index = usize::from(*retvar);
                if mapping[index] == None {
                    mapping[index] = Some(*retvar);
                } else {
                    // FIXME: Rust currently does not allow us to call a mutable
                    // method whilst also borrowing mutably, even if the
                    // borrows do not interfere with each other, so we
                    // have to do things manually. (#1)
                    let new_var = Variable::from(context.variables.len());
                    let typed = context.variables[usize::from(*retvar)].clone();
                    context.variables.push(typed);
                    mapping[usize::from(*retvar)] = Some(new_var);
                    *retvar = new_var;
                }
            }
        }
    }

    Flow::Continue
}
