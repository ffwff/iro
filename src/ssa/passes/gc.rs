use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::ContextLocalData;
use bit_set::BitSet;

pub fn collect_garbage_vars(data: &mut ContextLocalData, context: &mut Context) -> Flow {
    dbg_println!("before tracing: {}", context.print());
    let mut var_to_ins: Vec<Option<&Ins>> = vec![None; context.variables.len()];
    let mut roots = vec![];
    for block in &context.blocks {
        for ins in &block.ins {
            if let Some(retvar) = ins.retvar() {
                var_to_ins[usize::from(retvar)] = Some(ins);
            }
            if ins.typed.is_jmp() || ins.typed.has_side_effects() {
                if let Some(retvar) = ins.retvar() {
                    roots.push(usize::from(retvar));
                }
                ins.each_used_var(|var| roots.push(usize::from(var)));
            }
        }
    }
    let mut alive: BitSet = BitSet::with_capacity(context.variables.len());
    fn trace(var: usize, var_to_ins: &[Option<&Ins>], alive: &mut BitSet) {
        if alive.contains(var) {
            return;
        }
        alive.insert(var);
        if let Some(used) = &var_to_ins[var] {
            used.each_used_var(|cvar| trace(usize::from(cvar), var_to_ins, alive));
        } else {
            unreachable!("no entry found for {}", var)
        }
    }
    for root in roots {
        trace(root, &var_to_ins, &mut alive);
    }
    std::mem::drop(var_to_ins);
    for block in &mut context.blocks {
        block.ins.retain(|ins| {
            if let Some(retvar) = ins.retvar() {
                alive.contains(usize::from(retvar))
            } else {
                true
            }
        });
    }
    for (idx, var) in context.variables.iter_mut().enumerate() {
        if !alive.contains(idx) {
            *var = Type::NeverUsed;
            data.unused_vars.push(Variable::from(idx));
        }
    }
    dbg_println!("after tracing: {}", context.print());
    Flow::Continue
}
