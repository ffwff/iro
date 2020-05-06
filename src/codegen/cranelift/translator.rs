use crate::ssa::isa;
use cranelift_codegen::ir::entities::Value;
use cranelift_codegen::ir::{types, InstBuilder};
use cranelift_frontend::{FunctionBuilder, Variable};

pub fn to_var(x: isa::Variable) -> Variable {
    Variable::with_u32(x.into())
}

pub fn const_to_value(builder: &mut FunctionBuilder, c: &isa::Constant) -> Value {
    match c {
        isa::Constant::Bool(x) => builder.ins().bconst(types::B1, *x),
        isa::Constant::I32(x) => builder.ins().iconst(types::I32, *x as i64),
        isa::Constant::I64(x) => builder.ins().iconst(types::I64, *x),
        isa::Constant::F64(x) => builder.ins().f64const(*x),
    }
}

pub fn const_to_type(c: &isa::Constant) -> types::Type {
    match c {
        isa::Constant::Bool(_) => types::B1,
        isa::Constant::I32(_) => types::I32,
        isa::Constant::I64(_) => types::I64,
        isa::Constant::F64(_) => types::F64,
    }
}
