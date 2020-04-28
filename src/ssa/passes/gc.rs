use crate::compiler::Flow;
use crate::ssa::isa::*;
use std::collections::{BTreeMap, BTreeSet};
use bit_set::BitSet;

pub fn collect_garbage_vars_with_multiple_assigns(context: &mut Context) -> Flow {
    dbg_println!("before tracing: {}", context.print());
    let mut var_to_ins: Vec<Vec<Ins>> = vec![vec![]; context.variables.len()];
    let mut roots = vec![];
    for block in &mut context.blocks {
        for ins in &mut block.ins {
            if let Some(retvar) = ins.retvar() {
                var_to_ins[retvar].push(ins.clone());
            }
            if ins.typed.is_jmp() || ins.typed.has_side_effects() {
                if let Some(retvar) = ins.retvar() {
                    roots.push(retvar);
                }
                ins.each_used_var(|var| roots.push(var));
            }
        }
    }
    dbg_println!("roots: {:#?}", roots);
    for (name, vec) in var_to_ins.iter().enumerate() {
        for ins in vec {
            dbg_println!("var_to_ins: {} => {}", name, ins.print());
        }
    }
    let mut alive: BitSet = BitSet::with_capacity(context.variables.len());
    fn trace(var: usize, var_to_ins: &Vec<Vec<Ins>>, alive: &mut BitSet) {
        if alive.contains(var) {
            return;
        }
        alive.insert(var);
        for ins in &var_to_ins[var] {
            ins.each_used_var(|cvar| trace(cvar, var_to_ins, alive));
        }
    }
    for root in roots {
        trace(root, &var_to_ins, &mut alive);
    }
    for block in &mut context.blocks {
        block.ins.retain(|ins| {
            if let Some(retvar) = ins.retvar() {
                alive.contains(retvar)
            } else {
                true
            }
        });
    }
    let mut removed = 0;
    for (idx, var) in context.variables.iter_mut().enumerate() {
        if !alive.contains(idx) {
            removed += 1;
            *var = Type::NeverUsed;
        }
    }
    dbg_println!("removed: {}", removed);
    dbg_println!("after tracing: {}", context.print());
    Flow::Continue
}

pub fn collect_garbage_vars(context: &mut Context) -> Flow {
    dbg_println!("before tracing: {}", context.print());
    let mut var_to_ins: Vec<Option<Ins>> = vec![None; context.variables.len()];
    let mut roots = vec![];
    for block in &mut context.blocks {
        for ins in &mut block.ins {
            if let Some(retvar) = ins.retvar() {
                var_to_ins[retvar] = Some(ins.clone());
            }
            if ins.typed.is_jmp() || ins.typed.has_side_effects() {
                if let Some(retvar) = ins.retvar() {
                    roots.push(retvar);
                }
                ins.each_used_var(|var| roots.push(var));
            }
        }
    }
    let mut alive: BitSet = BitSet::with_capacity(context.variables.len());
    fn trace(var: usize, var_to_ins: &Vec<Option<Ins>>, alive: &mut BitSet) {
        if alive.contains(var) {
            return;
        }
        alive.insert(var);
        if let Some(used) = &var_to_ins[var] {
            used.each_used_var(|cvar| trace(cvar, var_to_ins, alive));
        } else {
            unreachable!("no entry found for {}", var)
        }
    }
    for root in roots {
        trace(root, &var_to_ins, &mut alive);
    }
    for block in &mut context.blocks {
        block.ins.retain(|ins| {
            if let Some(retvar) = ins.retvar() {
                alive.contains(retvar)
            } else {
                true
            }
        });
    }
    for (idx, var) in context.variables.iter_mut().enumerate() {
        if !alive.contains(idx) {
            *var = Type::NeverUsed;
        }
    }
    dbg_println!("after tracing: {}", context.print());
    Flow::Continue
}
