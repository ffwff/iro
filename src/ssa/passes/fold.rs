use crate::ssa::isa::*;
use crate::compiler::Flow;
use std::collections::BTreeMap;

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
                if let Some(eval) = kleft
                    .to_const()
                    .unwrap()
                    .$method(kright.to_const().unwrap())
                {
                    $ins.typed = InsType::load_const(eval);
                }
            }
            (None, None) => (),
        }
    }};
}

pub fn fold_constants(context: &mut Context) -> Flow {
    // dbg_println!("before folding: {}", context.print());
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
                _ => (),
            }
        }
    }
    // dbg_println!("after folding: {:#?}", context);
    Flow::Continue
}
