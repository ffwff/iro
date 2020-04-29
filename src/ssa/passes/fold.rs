use crate::compiler::Flow;
use crate::ssa::isa::*;
use std::collections::BTreeMap;

pub fn fold_constants(context: &mut Context) -> Flow {
    dbg_println!("before folding: {}", context.print());

    let mut var_to_const = BTreeMap::new();
    for block in &mut context.blocks {
        for ins in &mut block.ins {
            match &ins.typed {
                const_ins if const_ins.is_const() => {
                    var_to_const.insert(ins.retvar().unwrap(), const_ins.clone());
                }
                InsType::Copy(x) => {
                    if let Some(k) = var_to_const.get(&x).cloned() {
                        ins.typed = k.clone();
                        var_to_const.insert(ins.retvar().unwrap(), k);
                    }
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
            let (left, right, op_const) = match &ins.typed {
                InsType::Add((left, right)) => (*left, *right, OpConst::Add),
                InsType::Sub((left, right)) => (*left, *right, OpConst::Sub),
                InsType::Mul((left, right)) => (*left, *right, OpConst::Mul),
                InsType::Div((left, right)) => (*left, *right, OpConst::Div),
                InsType::Mod((left, right)) => (*left, *right, OpConst::Mod),
                InsType::Lt((left, right)) => (*left, *right, OpConst::Lt),
                InsType::Gt((left, right)) => (*left, *right, OpConst::Gt),
                InsType::Lte((left, right)) => (*left, *right, OpConst::Lte),
                InsType::Gte((left, right)) => (*left, *right, OpConst::Gte),
                InsType::Equ((left, right)) => (*left, *right, OpConst::Equ),
                InsType::Neq((left, right)) => (*left, *right, OpConst::Neq),
                const_ins if const_ins.is_const() => continue,
                _ => continue,
            };
            let function = match op_const {
                OpConst::Add => Constant::add,
                OpConst::Sub => Constant::sub,
                OpConst::Mul => Constant::mul,
                OpConst::Div => Constant::div,
                OpConst::Mod => Constant::imod,
                OpConst::Lt => Constant::lt,
                OpConst::Gt => Constant::gt,
                OpConst::Lte => Constant::lte,
                OpConst::Gte => Constant::gte,
                OpConst::Equ => Constant::equ,
                OpConst::Neq => Constant::neq,
            };
            match (var_to_const.get(&left), var_to_const.get(&right)) {
                (None, Some(k)) => {
                    ins.typed = InsType::OpConst {
                        register: left,
                        constant: k.to_const().unwrap(),
                        op: op_const,
                        reg_left: true,
                    };
                }
                (Some(k), None) => {
                    ins.typed = InsType::OpConst {
                        register: right,
                        constant: k.to_const().unwrap(),
                        op: op_const,
                        reg_left: false,
                    };
                }
                (Some(kleft), Some(kright)) => {
                    if let Some(eval) =
                        function(&kleft.to_const().unwrap(), kright.to_const().unwrap())
                    {
                        ins.typed = InsType::load_const(eval);
                    }
                }
                (None, None) => (),
            }
        }
    }

    dbg_println!("after folding: {}", context.print());
    Flow::Continue
}
