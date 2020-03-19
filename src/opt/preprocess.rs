use std::collections::{BTreeSet, BTreeMap};
use crate::ssa::isa::*;

pub fn preprocess(mut contexts: FuncContexts) -> FuncContexts {
    for (name, context) in &mut contexts {
        let context = context.as_mut().unwrap();
        let mut defsites : Vec<BTreeMap<usize, Option<usize>>> =
            (0..context.variables.len()).map(|_| btreemap![]).collect();

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
                        set.insert(idx, None);
                        true
                    },
                }
            });
        }

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

        if num_blocks > 0 {
            // Calculate the dominators for each block
            // See https://www.doc.ic.ac.uk/~livshits/classes/CO444H/reading/dom14.pdf for algorithm
            let mut doms: BTreeMap<usize, usize> = btreemap![ 0 => 0 ];
            let mut changed = true;
            while changed {
                changed = false;
                for &b in rpo.iter() {
                    let intersect = |mut b1: usize, mut b2: usize| {
                        while rpo_ordering[b1] != rpo_ordering[b2] {
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
            // unimplemented!();

            // Find the dominance frontier for each block
            let mut dom_frontier: Vec<BTreeSet<usize>> = (0..num_blocks).map(|_| btreeset![]).collect();
            for b in 0..num_blocks {
                let predecessors = &predecessors_map[b];
                if predecessors.len() >= 2 {
                    for &p in predecessors {
                        let mut runner: usize = p;
                        while runner != doms[&b] {
                            // println!("{:?} {:?} {:?} {:?}", runner, b, doms[&b], doms[&runner]);
                            dom_frontier[runner].insert(b);
                            runner = doms[&runner];
                        }
                    }
                }
            }
            println!("---\ndom_frontier: {:#?}", dom_frontier);
            for (var, mut defsites) in defsites.iter_mut().enumerate() {
                let defsites_vec: Vec<usize> = defsites.iter().map(|(x, _)| *x).collect();
                let mut has_phi: BTreeSet<usize> = btreeset![];
                for &defsite in &defsites_vec {
                    for &block in &dom_frontier[defsite] {
                        if !has_phi.contains(&block) {
                            println!("var: {} {:?}", var, block);
                            context.blocks[block].ins.insert(0, Ins::new(
                                var,
                                InsType::Phi {
                                    vars: vec![],
                                }
                            ));
                            has_phi.insert(block);
                        }
                    }
                }
            }
        }
    }
    contexts
}