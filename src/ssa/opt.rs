use crate::ssa::isa::*;
use crate::utils::pipeline::Flow;
use std::collections::{BTreeMap, BTreeSet};

pub fn insert_jmps(context: &mut Context) -> Flow {
    let len = context.blocks.len();
    for (idx, block) in context.blocks.iter_mut().enumerate() {
        if let Some(ins) = block.ins.last() {
            if !ins.typed.is_jmp() {
                block.ins.push(Ins::new(0, InsType::Jmp(idx + 1)));
            }
        } else if idx + 1 != len {
            block.ins.push(Ins::new(0, InsType::Jmp(idx + 1)));
        }
    }
    Flow::Continue
}

pub fn build_graph_and_rename_vars(context: &mut Context) -> Flow {
    if context.blocks.is_empty() {
        return Flow::Break;
    }
    dbg_println!("begin: {:#?}", context);

    let mut defsites: Vec<BTreeSet<usize>> = vec![btreeset![]; context.variables.len()];
    let num_blocks = context.blocks.len();

    // Build the successor/predecessor set corresponding to each block
    let mut predecessors_map: Vec<Vec<usize>> = vec![vec![]; num_blocks];
    let mut successors_map: Vec<Vec<usize>> = vec![vec![]; num_blocks];
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
                InsType::Nop => false,
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
                _ => {
                    if let Some(retvar) = ins.retvar() {
                        let set = &mut defsites[retvar];
                        set.insert(idx);
                    }
                    true
                }
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

    if num_blocks > 1 {
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
                        block.ins.insert(
                            0,
                            Ins::new(
                                var,
                                InsType::Phi {
                                    vars: std::iter::repeat(var).take(block.preds.len()).collect(),
                                    defines: var,
                                },
                            ),
                        );
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
                for ins in &mut block.ins {
                    if !ins.typed.is_phi() {
                        ins.rename_var(var, *version_stack.last().clone().unwrap());
                    }
                    if let Some(retvar) = ins.mut_retvar() {
                        if *retvar == var {
                            if *version > 1 {
                                *retvar = new_variable_len;
                                version_stack.push(new_variable_len);
                                dbg_println!("new: {:#?}", *retvar);
                                new_variable_len += 1;
                            }
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
            dbg_println!("rename var {} ==> {:#?}", var, context);
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

macro_rules! ins_to_const_ins {
    ($left:expr, $right:expr, $var_to_const:expr, $ins:expr, $typed:tt, $method:tt) => {{
        match ($var_to_const.get(&$left), $var_to_const.get(&$right)) {
            (None, Some(k)) => {
                $ins.typed = InsType::$typed(RegConst::RegLeft(($left, k.to_const().unwrap())));
            }
            (Some(k), None) => {
                $ins.typed = InsType::$typed(RegConst::RegRight((k.to_const().unwrap(), $right)));
            }
            (Some(kleft), Some(kright)) => {
                $ins.typed = InsType::load_const(
                    kleft
                        .to_const()
                        .unwrap()
                        .$method(kright.to_const().unwrap()),
                );
            }
            (None, None) => (),
        }
    }};
}

macro_rules! ins_index {
    ($ins:expr, $index:expr, $var:expr, $var_to_const:expr, $context:expr, $method:tt) => {
        if let Some(k) = $var_to_const.get(&$index) {
            let offset_index = match k.to_const().unwrap() {
                Constant::I32(x) => x,
                Constant::I64(x) => x as i32,
                _ => unreachable!(),
            };
            $ins.typed = InsType::$method {
                var: *$var,
                offset: offset_index,
            };
        }
    };
}

pub fn fold_constants(context: &mut Context) -> Flow {
    dbg_println!("before folding: {:#?}", context);
    let mut var_to_const = BTreeMap::new();
    for block in &mut context.blocks {
        for ins in &mut block.ins {
            match &ins.typed {
                const_ins if const_ins.is_const() => {
                    var_to_const.insert(ins.retvar().unwrap(), const_ins.clone());
                }
                InsType::Cast { var, typed } => {
                    if let Some(const_ins) = var_to_const.get(&var) {
                        if let Some(casted) = const_ins.const_cast(typed) {
                            var_to_const.insert(ins.retvar().unwrap(), casted.clone());
                            ins.typed = casted;
                        }
                    }
                }
                _ => (),
            }
        }
    }
    for block in &mut context.blocks {
        for ins in &mut block.ins {
            match &ins.typed {
                const_ins if const_ins.is_const() => (),
                InsType::Add((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, AddC, add)
                }
                InsType::Sub((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, SubC, sub)
                }
                InsType::Mul((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, MulC, mul)
                }
                InsType::Div((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, DivC, div)
                }
                InsType::Mod((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, ModC, imod)
                }
                InsType::Lt((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, LtC, lt)
                }
                InsType::Gt((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, GtC, gt)
                }
                InsType::Lte((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, LteC, lte)
                }
                InsType::Gte((left, right)) => {
                    ins_to_const_ins!(*left, *right, var_to_const, ins, GteC, gte)
                }
                InsType::PointerIndex { var, index } => {
                    ins_index!(ins, index, var, var_to_const, context, PointerIndexC)
                }
                InsType::FatIndex { var, index } => {
                    ins_index!(ins, index, var, var_to_const, context, FatIndexC)
                }
                InsType::BoundsCheck { var, index } => {
                    ins_index!(ins, index, var, var_to_const, context, BoundsCheckC)
                }
                _ => (),
            }
        }
    }
    dbg_println!("after folding: {:#?}", context);
    Flow::Continue
}

pub fn collect_garbage_vars(context: &mut Context) -> Flow {
    let mut var_to_ins: BTreeMap<usize, Ins> = BTreeMap::new();
    let mut roots = vec![];
    for block in &mut context.blocks {
        for ins in &mut block.ins {
            if let Some(retvar) = ins.retvar() {
                var_to_ins.insert(retvar, ins.clone());
            }
            if ins.typed.is_jmp() || ins.typed.has_side_effects() {
                if let Some(retvar) = ins.retvar() {
                    roots.push(retvar);
                }
                ins.each_used_var(|var| roots.push(var));
            }
        }
    }
    let mut alive: BTreeSet<usize> = btreeset![];
    fn trace(var: usize, var_to_ins: &BTreeMap<usize, Ins>, alive: &mut BTreeSet<usize>) {
        if alive.contains(&var) {
            return;
        }
        alive.insert(var);
        var_to_ins[&var].each_used_var(|cvar| trace(cvar, var_to_ins, alive));
    }
    for root in roots {
        trace(root, &var_to_ins, &mut alive);
    }
    for block in &mut context.blocks {
        block.ins.retain(|ins| {
            if let Some(retvar) = ins.retvar() {
                alive.contains(&retvar)
            } else {
                true
            }
        });
    }
    for (idx, var) in context.variables.iter_mut().enumerate() {
        if !alive.contains(&idx) {
            *var = Type::NeverUsed;
        }
    }
    dbg_println!("after tracing: {:#?}", context);
    Flow::Continue
}

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
        for ins in &block.ins {
            if let Some(retvar) = ins.retvar() {
                vars_declared_in_this_block.insert(retvar);
            }
            ins.each_used_var(|used| {
                vars_used.insert(used);
            });
        }
        block.vars_declared_in_this_block = vars_declared_in_this_block;
        block.vars_used = vars_used;
        // dbg_println!("block: {:#?}", block);
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

    dbg_println!("after dfa: {:#?}", context);
    Flow::Continue
}

pub fn separate_postlude(context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        if let Some(ins) = block.ins.last().clone() {
            if ins.typed.is_jmp() {
                block.postlude = ins.clone();
                block.ins.pop();
            } else {
                unreachable!()
            }
        }
    }
    Flow::Continue
}

pub fn cleanup_blocks(context: &mut Context) -> Flow {
    if context.blocks.len() < 2 {
        return Flow::Continue;
    }
    dbg_println!("cleanup: {:#?}", context);

    // map of new block idx -> old block idx
    let mut idx_map: Vec<usize> = (0..context.blocks.len()).collect();
    // map of old jmp location -> new jmp location (in old block indices)
    let mut jmp_map: BTreeMap<usize, usize> = btreemap![];
    let old_vec = std::mem::replace(&mut context.blocks, vec![]);
    for (idx, block) in old_vec.iter().enumerate() {
        if let InsType::Jmp(n) = block.postlude.typed {
            if block.ins.is_empty() {
                jmp_map.insert(idx, n);
            }
        }
    }
    idx_map.retain(|n| !jmp_map.contains_key(n));

    // Fix jump targets to new block indices
    for (_, target) in &mut jmp_map {
        *target = idx_map.binary_search(target).unwrap();
    }

    dbg_println!("{:#?} {:#?}", jmp_map, idx_map);

    let map_block_idx = |idx: usize| {
        let new = jmp_map
            .get(&idx)
            .copied()
            .unwrap_or_else(|| idx_map.binary_search(&idx).unwrap());
        dbg_println!("map {} => {}", idx, new);
        new
    };

    // Do the cleanup
    for (idx, mut block) in old_vec.into_iter().enumerate() {
        if !jmp_map.contains_key(&idx) {
            match &mut block.postlude.typed {
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
        }
    }

    // Rebuild the successor/predecessor set corresponding to each block
    let num_blocks = context.blocks.len();
    let mut predecessors_map: Vec<Vec<usize>> = vec![vec![]; num_blocks];
    let mut successors_map: Vec<Vec<usize>> = vec![vec![]; num_blocks];
    let mut insert_node = |succ: usize, pred: usize| {
        predecessors_map[succ].push(pred);
        successors_map[pred].push(succ);
    };

    // Filter out nops and build the graph maps
    for (idx, block) in context.blocks.iter_mut().enumerate() {
        match &mut block.postlude.typed {
            InsType::IfJmp {
                iftrue, iffalse, ..
            } => {
                insert_node(*iftrue, idx);
                insert_node(*iffalse, idx);
            }
            InsType::Jmp(target) => {
                insert_node(*target, idx);
            }
            _ => (),
        }
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

    dbg_println!("after cleanup: {:#?}", context);
    Flow::Continue
}

pub fn eliminate_phi(context: &mut Context) -> Flow {
    if context.blocks.len() < 2 {
        return Flow::Continue;
    }

    dbg_println!("before phis: {:#?}", context);
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
    dbg_println!("phis: {:?}", replacements);
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
                            block.ins.push(Ins::new(newvar, InsType::LoadVar(retvar)));
                        }
                    }
                }
            }
        }
    }
    dbg_println!("after phis: {:#?}", context);
    Flow::Continue
}

pub fn fuse_postlude(context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        let postlude = std::mem::replace(&mut block.postlude, Ins::new(0, InsType::Nop));
        block.ins.push(postlude);
    }
    Flow::Continue
}
