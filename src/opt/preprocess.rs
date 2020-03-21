use std::collections::{BTreeSet, BTreeMap, VecDeque};
use crate::ssa::isa::*;

pub fn build_graph_and_rename_vars(mut contexts: FuncContexts) -> FuncContexts {
    for (_, context) in &mut contexts {
        let context = context.as_mut().unwrap();
        let mut defsites : Vec<BTreeSet<usize>> =
            (0..context.variables.len()).map(|_| btreeset![]).collect();

        let num_blocks = context.blocks.len();

        // Build the successor/predecessor set corresponding to each block
        let mut predecessors_map: Vec<Vec<usize>> = (0..num_blocks).map(|_| vec![]).collect();
        let mut successors_map: Vec<Vec<usize>> = (0..num_blocks).map(|_| vec![]).collect();
        let mut insert_node = |succ: usize, pred: usize| {
            predecessors_map[succ].push(pred);
            successors_map[pred].push(succ);
        };

        // FIlter out nops and build the graph maps
        for (idx, block) in context.blocks.iter_mut().enumerate() {
            let mut jumped = false;
            block.ins.retain(|ins| {
                if jumped {
                    return false;
                }
                match &ins.typed {
                    InsType::Nop => false,
                    InsType::Return(_) => {
                        jumped = true;
                        true
                    }
                    InsType::IfJmp { condvar: _, iftrue, iffalse } => {
                        insert_node(*iftrue, idx);
                        insert_node(*iffalse, idx);
                        jumped = true;
                        true
                    }
                    InsType::Jmp(target) => {
                        insert_node(*target, idx);
                        jumped = true;
                        true
                    }
                    _ => {
                        let retvar = ins.retvar().unwrap();
                        let set = &mut defsites[retvar];
                        set.insert(idx);
                        true
                    },
                }
            });
            if !jumped && idx + 1 < num_blocks {
                insert_node(idx + 1, idx);
            }
        }

        // Store predecessors and successors in blocks
        for ((preds, succs), block) in predecessors_map.into_iter().zip(successors_map).zip(context.blocks.iter_mut()) {
            block.preds = preds;
            block.succs = succs;
        }

        if num_blocks > 1 {
            // Build the post-order traversal array
            let mut rpo = Vec::with_capacity(context.blocks.len());
            {
                let mut visited = btreeset![];
                fn walk(node: usize,
                        context: &Context,
                        post_order: &mut Vec<usize>,
                        visited: &mut BTreeSet<usize>) {
                    visited.insert(node);
                    let block = &context.blocks[node];
                    for succ in &block.succs {
                        if !visited.contains(succ) {
                            walk(*succ, context, post_order, visited);
                        }
                    }
                    post_order.push(node);
                }
                walk(0, &context, &mut rpo, &mut visited);
            }
            
            let rpo_ordering: Vec<usize> = {
                let mut zipped: Vec<(usize, usize)> = rpo.iter().enumerate().map(|(idx, node)| (*node, idx)).collect();
                zipped.sort_by_key(|(k, v)| *k);
                zipped.into_iter().map(|(idx, node)| node).collect()
            };
            rpo.reverse();

            // Calculate the dominators for each block
            // See https://www.doc.ic.ac.uk/~livshits/classes/CO444H/reading/dom14.pdf for algorithm
            let mut doms: BTreeMap<usize, usize> = btreemap![ 0 => 0 ];
            let mut changed = true;
            while changed {
                changed = false;
                for &b in rpo.iter().skip(1) { // Skip first block
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
                    let new_idom =
                        context.blocks[b].preds
                            .iter()
                            .filter(|p| doms.contains_key(&p))
                            .fold(None, |last, &curr| {
                                match last {
                                    None => Some(curr),
                                    Some(last) => Some(intersect(curr, last))
                                }
                            })
                            .unwrap();
                    if !doms.contains_key(&b) || doms[&b] != new_idom {
                        doms.insert(b, new_idom);
                        changed = true;
                    }
                }
            }
            // println!("---\nsucc:{:#?}\npreds: {:#?}\ndoms: {:#?}\n---", successors_map, predecessors_map, doms);

            // Calculate the dominance tree
            let mut dom_tree: BTreeMap<usize, Vec<usize>> = btreemap![];
            for (&idx, &dominator) in &doms {
                if idx == dominator { continue; }
                if let Some(mut vec) = dom_tree.get_mut(&dominator) {
                    vec.push(idx);
                } else {
                    dom_tree.insert(dominator, vec![ idx ]);
                }
            }

            println!("dom_tree: {:#?}", dom_tree);

            // Flatten the dominance tree in DFS order
            let mut dominance_dfs = vec![];
            {
                fn walk(node: usize,
                        dominance_dfs: &mut Vec<usize>,
                        dom_tree: &BTreeMap<usize, Vec<usize>>) {
                    dominance_dfs.push(node);
                    if let Some(children) = dom_tree.get(&node) {
                        for &child in children {
                            walk(child, dominance_dfs, dom_tree);
                        }
                    }
                }
                walk(0, &mut dominance_dfs, &dom_tree);
            }
            println!("dominance_dfs: {:?}", dominance_dfs);

            // Find the dominance frontier for each block
            let mut dom_frontier: Vec<BTreeSet<usize>> = (0..num_blocks).map(|_| btreeset![]).collect();
            for (b, blocks) in context.blocks.iter_mut().enumerate() {
                if blocks.preds.len() >= 2 {
                    for &p in &blocks.preds {
                        let mut runner: usize = p;
                        while runner != doms[&b] {
                            println!("dom runner: {:?} {:?} {:?} {:?}", runner, b, doms[&b], doms[&runner]);
                            dom_frontier[runner].insert(b);
                            runner = doms[&runner];
                        }
                    }
                }
            }
            println!("---\ndom_frontier: {:#?}", dom_frontier);
            println!("---\ndefsites: {:#?}", defsites);
            
            // Insert some phi nodes
            for (var, defsites) in defsites.iter_mut().enumerate() {
                let mut has_phi: BTreeSet<usize> = btreeset![];
                for &defsite in defsites.iter() {
                    for &node in &dom_frontier[defsite] {
                        has_phi.insert(node);
                    }
                }
                for &frontier_node in &has_phi {
                    context.blocks[frontier_node].ins.insert(0, Ins::new(
                        var,
                        InsType::Phi {
                            vars: vec![ var ],
                        }
                    ));
                }
            }

            println!("AAA {:#?}", context);

            // Rename variables
            let orig_varlen = context.variables.len();
            let mut last_defined_at_block: Vec<usize> = (0..orig_varlen).map(|_| 0).collect();
            for var in 0..orig_varlen {
                println!("renaming variable {}", var);
                let mut did_initial_assignment = false;
                for last_defined in &mut last_defined_at_block {
                    *last_defined = var;
                }
                println!("rpo: {:#?}", rpo);
                let typed = context.variables[var].clone();
                for &node in rpo.iter() {
                    let block = &mut context.blocks[node];
                    let mut last_defined = last_defined_at_block[node];
                    for ins in &mut block.ins {
                        ins.rename_var(false, var, last_defined);
                        if let Some(retvar) = ins.mut_retvar() {
                            if *retvar == var {
                                let old = *retvar;
                                if !did_initial_assignment {
                                    did_initial_assignment = true;
                                } else {
                                    context.variables.push(typed.clone());
                                    *retvar = context.variables.len() - 1;
                                }
                                println!("changed {} to {}", old, *retvar);
                                last_defined = *retvar;
                            }
                        }
                    }
                    last_defined_at_block[node] = last_defined;
                    for &succ in &block.succs {
                        last_defined_at_block[succ] = last_defined;
                    }
                }
                // Insert vars for phi-node
                for block in &mut context.blocks {
                    for ins in &mut block.ins {
                        let retvar = ins.retvar();
                        match &mut ins.typed {
                            InsType::Phi { vars } => {
                                println!("{:#?}", vars);
                                if vars.len() == 1 && vars[0] == var {
                                    let mut v: Vec<usize> = block.preds
                                        .iter()
                                        .map(|pred| last_defined_at_block[*pred])
                                        .collect();
                                    let retvar = retvar.unwrap();
                                    v.retain(|var| *var != retvar);
                                    *vars = v;
                                    break;
                                }
                            },
                            _ => (),
                        }
                    }
                }
                println!("{} ==> {:#?}", var, context);
                //panic!("!!!");
            }
        } else {
            let mut mapping: Vec<Option<usize>> = (0..context.variables.len()).map(|_| None).collect();
            let block = &mut context.blocks[0];
            for ins in &mut block.ins {
                ins.rename_var_by(true, |var| mapping[var].unwrap());
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
    }
    contexts
}

pub fn remove_defined_never_used(mut contexts: FuncContexts) -> FuncContexts {
    for (_, context) in &mut contexts {
        let context = context.as_mut().unwrap();
        let mut to_remove: BTreeSet<usize> = BTreeSet::new();
        for block in &mut context.blocks {
            for ins in &block.ins {
                if let Some(retvar) = ins.retvar() {
                    to_remove.insert(retvar);
                }
                ins.each_used_var(|used| {
                    to_remove.remove(&used);
                })
            }
        }
        for block in &mut context.blocks {
            block.ins.retain(|ins| {
                if let Some(retvar) = ins.retvar() {
                    !to_remove.contains(&retvar)
                } else {
                    true
                }
            })
        }
    }
    contexts
}

pub fn data_flow_analysis(mut contexts: FuncContexts) -> FuncContexts {
    for (_, context) in &mut contexts {
        let context = context.as_mut().unwrap();

        // Generate the initial worklist by putting blocks with side-effects
        let mut worklist: Vec<usize> =
            context.blocks
                .iter()
                .enumerate()
                .filter(|(idx, block)| block.ins.iter().any(|ins| ins.typed.is_return()))
                .map(|(idx, block)| idx).collect();
        
        println!("initial worklist: {:#?}", worklist);
        while let Some(node) = worklist.pop() {
            let mut vars_in = BTreeSet::new();
            let mut vars_declared_in_this_block = BTreeSet::new();
            // Temporarily borrow preds
            let (preds, state_changed) = {
                let block = &mut context.blocks[node];
                std::mem::replace(&mut vars_in, block.vars_in.clone());
                // Calculate input variables for this block, this will be
                // used as output variables for preceding blocks
                for ins in &block.ins {
                    if let Some(retvar) = ins.retvar() {
                        vars_declared_in_this_block.insert(retvar);
                    }
                    ins.each_used_var(|var| {
                        if !vars_declared_in_this_block.contains(&var) {
                            vars_in.insert(var);
                        }
                    });
                }
                let state_changed = vars_in != block.vars_in;
                (std::mem::replace(&mut block.preds, Vec::new()), state_changed)
            };
            // Add preceding blocks to the worklist if old in-state != new in-state
            if state_changed {
                for &pred in &preds {
                    let block = &mut context.blocks[pred];
                    block.vars_out = vars_in.clone();
                    worklist.push(pred);
                }
            }
            // Give preds back and fill vars_in
            {
                let block = &mut context.blocks[node];
                block.preds = preds;
                block.vars_in = vars_in;
            }
            println!("worklist: {:#?}", worklist);
        }
        println!("worklist: {:#?}", worklist);
    }
    contexts
}

pub fn remove_unused_local_vars(mut contexts: FuncContexts) -> FuncContexts {
    for (_, context) in &mut contexts {
        let context = context.as_mut().unwrap();
        for block in &mut context.blocks {
            let mut to_remove: BTreeSet<usize> = BTreeSet::new();
            for ins in &block.ins {
                if let Some(retvar) = ins.retvar() {
                    to_remove.insert(retvar);
                }
                ins.each_used_var(|used| {
                    to_remove.remove(&used);
                })
            }
            for var in &block.vars_in {
                to_remove.remove(&var);
            }
            for var in &block.vars_out {
                to_remove.remove(&var);
            }
            block.ins.retain(|ins| {
                if let Some(retvar) = ins.retvar() {
                    !to_remove.contains(&retvar)
                } else {
                    true
                }
            });
        }
    }
    contexts
}
