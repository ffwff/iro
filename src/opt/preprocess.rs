use std::collections::{BTreeSet, BTreeMap};
use crate::ssa::isa::*;

pub fn preprocess(mut contexts: FuncContexts) -> FuncContexts {
    for (name, context) in &mut contexts {
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

        if num_blocks > 1 {
            // Build the post-order traversal array
            let mut rpo = Vec::with_capacity(context.blocks.len());
            {
                let mut visited = btreeset![];
                fn walk(node: usize,
                        successors_map: &Vec<Vec<usize>>,
                        post_order: &mut Vec<usize>,
                        visited: &mut BTreeSet<usize>) {
                    visited.insert(node);
                    for succ in &successors_map[node] {
                        if !visited.contains(succ) {
                            walk(*succ, successors_map, post_order, visited);
                        }
                    }
                    post_order.push(node);
                };
                walk(0, &successors_map, &mut rpo, &mut visited);
            }
            
            let rpo_ordering: Vec<usize> = {
                let mut zipped: Vec<(usize, usize)> = rpo.iter().enumerate().map(|(idx, node)| (*node, idx)).collect();
                zipped.sort_by_key(|(k, v)| *k);
                zipped.into_iter().map(|(idx, node)| node).collect()
            };
            rpo.pop();
            rpo.reverse();

            // Calculate the dominators for each block
            // See https://www.doc.ic.ac.uk/~livshits/classes/CO444H/reading/dom14.pdf for algorithm
            let mut doms: BTreeMap<usize, usize> = btreemap![ 0 => 0 ];
            let mut changed = true;
            while changed {
                changed = false;
                for &b in rpo.iter() {
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
                        predecessors_map[b]
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
            println!("---\nsucc:{:#?}\npreds: {:#?}\ndoms: {:#?}\n---", successors_map, predecessors_map, doms);

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
            for b in 0..num_blocks {
                let predecessors = &predecessors_map[b];
                if predecessors.len() >= 2 {
                    for &p in predecessors {
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
            
            for (var, defsites) in defsites.iter_mut().enumerate() {
                // Insert some phi nodes
                let mut has_phi: BTreeMap<usize, Vec<usize>> = btreemap![];
                for &defsite in defsites.iter() {
                    for &node in &dom_frontier[defsite] {
                        // println!("var: {} {:?}", var, node);
                        if let Some(vec) = has_phi.get_mut(&node) {
                            vec.push(defsite);
                        } else {
                            has_phi.insert(node, vec![ defsite ]);
                        }
                    }
                }

                for (&frontier_node, _) in &has_phi {
                    context.blocks[frontier_node].ins.insert(0, Ins::new(
                        var,
                        InsType::Phi {
                            vars: vec![ var ],
                        }
                    ));
                }

                // Rename variables in each block and record the
                // final type of that variable in the block
                let mut did_initial_assignment = false;
                let mut newvar = var;
                let mut last_at_block: Vec<usize> = vec![];
                for &node in &dominance_dfs {
                    let block = &mut context.blocks[node];
                    last_at_block.push(newvar);
                    for ins in &mut block.ins {
                        if var != newvar {
                            ins.rename_var(var, newvar);
                        }
                        if let Some(retvar) = ins.mut_retvar() {
                            if *retvar == var {
                                if !did_initial_assignment {
                                    did_initial_assignment = true;
                                } else if newvar == var {
                                    newvar = context.variables.len();
                                    *retvar = newvar;
                                } else {
                                    newvar += 1;
                                    *retvar = newvar;
                                }
                                last_at_block[node] = *retvar;
                            }
                        }
                    }
                }

                if newvar != var {
                    // Fill in variables for each phi nodes and filter out duplicates
                    for (&node, phivars) in &has_phi {
                        let block = &mut context.blocks[node];
                        let ins = &mut block.ins[0];
                        let retvar = ins.retvar().unwrap();
                        if let InsType::Phi { vars } = &mut ins.typed {
                            for &phivar in phivars {
                                vars.push(last_at_block[phivar]);
                            }
                            vars.retain(|v| *v != retvar);
                            vars.sort();
                            vars.dedup();
                        } else {
                            unreachable!()
                        }
                    }
                    // Create types for each new instance of this variable
                    let typed = context.variables[var].clone();
                    let len = context.variables.len();
                    for _ in len..=newvar {
                        context.variables.push(typed.clone());
                    }
                }
            }
        } else {
            let mut mapping: Vec<Option<usize>> = (0..context.variables.len()).map(|_| None).collect();
            for block in context.blocks.iter_mut() {
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
        }

        // TODO: fill duplicate variables
    }
    contexts
}