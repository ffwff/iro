use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::ContextLocalData;
use smallvec::SmallVec;
use std::collections::BTreeMap;

pub fn preprocess(_: &mut ContextLocalData, context: &mut Context) -> Flow {
    dbg_println!("start: {}", context.print());
    let len = context.blocks.len();
    if len == 0 {
        return Flow::Break;
    }
    for (idx, block) in context.blocks.iter_mut().enumerate() {
        if let Some(ins) = block.ins.last() {
            if !ins.typed.is_jmp() {
                block.ins.push(Ins::empty_ret(InsType::Jmp(idx + 1), 0));
            }
        } else if idx + 1 != len {
            block.ins.push(Ins::empty_ret(InsType::Jmp(idx + 1), 0));
        }
    }
    context.block_vars = vec![BlockVars::new(); context.blocks.len()];
    Flow::Continue
}

pub fn cleanup_jump_blocks(data: &mut ContextLocalData, context: &mut Context) -> Flow {
    if context.blocks.len() < 2 {
        return Flow::Continue;
    }
    data.invalid_build_graph = true;

    // map of new block idx -> old block idx
    let mut idx_map: Vec<usize> = (0..context.blocks.len()).collect();
    // map of old jmp location -> new jmp location (in old block indices)
    let mut jmp_map: BTreeMap<usize, usize> = btreemap![];

    let len = context.blocks.len();
    let old_blocks = std::mem::replace(&mut context.blocks, Vec::with_capacity(len));
    let old_block_vars = std::mem::replace(&mut context.block_vars, Vec::with_capacity(len));

    for (idx, block) in old_blocks.iter().enumerate() {
        if let InsType::Jmp(n) = block.postlude.as_ref().unwrap().typed {
            if block.ins.is_empty() {
                jmp_map.insert(idx, n);
            }
        }
    }
    idx_map.retain(|n| !jmp_map.contains_key(n));

    dbg_println!("{:#?} {:#?}", jmp_map, idx_map);

    // Fix jump targets to new block indices
    let old_jmp_map = jmp_map.clone();
    for (idx, target) in &old_jmp_map {
        // Unroll the chain first
        let mut actual_target = *target;
        while let Some(jmp) = jmp_map.get(&actual_target) {
            actual_target = *jmp;
        }
        jmp_map.insert(*idx, idx_map.binary_search(&actual_target).unwrap());
    }

    let map_block_idx = |idx: usize| {
        let new = jmp_map
            .get(&idx)
            .copied()
            .unwrap_or_else(|| idx_map.binary_search(&idx).unwrap());
        dbg_println!("map {} => {}", idx, new);
        new
    };

    // Do the cleanup
    for (idx, (mut block, block_var)) in old_blocks
        .into_iter()
        .zip(old_block_vars.into_iter())
        .enumerate()
    {
        if !jmp_map.contains_key(&idx) {
            let postlude = block.postlude.as_mut().unwrap();
            match &mut postlude.typed {
                InsType::IfJmp {
                    iftrue, iffalse, ..
                } => {
                    *iftrue = map_block_idx(*iftrue);
                    *iffalse = map_block_idx(*iffalse);
                }
                InsType::Jmp(n) => {
                    *n = map_block_idx(*n);
                }
                _ => (),
            }
            context.blocks.push(block);
            context.block_vars.push(block_var);
        }
    }

    Flow::Continue
}

pub fn build_graph(data: &mut ContextLocalData, context: &mut Context) -> Flow {
    if context.blocks.len() < 2 || !data.invalid_build_graph {
        return Flow::Continue;
    }
    data.invalid_build_graph = false;

    let num_blocks = context.blocks.len();

    // Build the successor/predecessor set corresponding to each block
    let mut predecessors_map: Vec<SmallVec<[usize; 2]>> = vec![smallvec![]; num_blocks];
    let mut successors_map: Vec<SmallVec<[usize; 2]>> = vec![smallvec![]; num_blocks];
    let mut insert_node = |succ: usize, pred: usize| {
        predecessors_map[succ].push(pred);
        successors_map[pred].push(succ);
    };

    // Filter out nops and build the graph maps
    for (idx, block) in context.blocks.iter_mut().enumerate() {
        let mut jumped = false;
        block.ins.retain(|ins| {
            if jumped {
                return false;
            }
            match &ins.typed {
                InsType::Return(_) | InsType::Trap(_) | InsType::Exit => {
                    jumped = true;
                    true
                }
                InsType::IfJmp {
                    iftrue, iffalse, ..
                } => {
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
                _ => true,
            }
        });
        debug_assert!(jumped);
    }

    // Store predecessors and successors in blocks
    for ((preds, succs), block) in predecessors_map
        .into_iter()
        .zip(successors_map)
        .zip(context.blocks.iter_mut())
    {
        block.preds = preds;
        block.succs = succs;
    }

    Flow::Continue
}
