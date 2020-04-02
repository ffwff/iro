use crate::runtime::Runtime;
use crate::ssa::isa;
use cranelift::prelude::*;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};
use cranelift_codegen::ir::entities::{Block, Value};
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{AbiParam, InstBuilder};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Backend, Linkage, Module, DataId, DataContext};
use cranelift_object::{ObjectBackend, ObjectBuilder};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use std::collections::HashMap;
use std::rc::Rc;

fn to_var(x: usize) -> Variable {
    Variable::with_u32(x as u32)
}

fn const_to_value(builder: &mut FunctionBuilder, c: &isa::Constant) -> Value {
    match c {
        isa::Constant::I32(x) => builder.ins().iconst(types::I32, *x as i64),
        isa::Constant::I64(x) => builder.ins().iconst(types::I64, *x),
        isa::Constant::F64(_x) => unimplemented!(),
    }
}

fn const_to_type(c: &isa::Constant) -> types::Type {
    match c {
        isa::Constant::I32(_) => types::I32,
        isa::Constant::I64(_) => types::I64,
        isa::Constant::F64(_) => types::F64,
    }
}

fn regconst_to_type(
    builder: &mut FunctionBuilder,
    rc: &isa::RegConst,
) -> (Value, Value, types::Type) {
    match rc {
        isa::RegConst::RegLeft((reg, value)) => (
            builder.use_var(to_var(*reg)),
            const_to_value(builder, value),
            const_to_type(value),
        ),
        isa::RegConst::RegRight((value, reg)) => (
            const_to_value(builder, value),
            builder.use_var(to_var(*reg)),
            const_to_type(value),
        ),
    }
}

macro_rules! generate_arithmetic {
    ($builder:expr, $ins:expr, $x:expr, $y:expr, $fn:tt) => {{
        let left = $builder.use_var(to_var(*$x));
        let right = $builder.use_var(to_var(*$y));
        let tmp = $builder.ins().$fn(left, right);
        $builder.def_var(to_var($ins.retvar().unwrap()), tmp);
    }};
}

/// Code generator
pub struct Codegen<B: Backend> {
    module: Module<B>,
    extern_mapping: HashMap<Rc<isa::FunctionName>, String>,
    string_mapping: HashMap<Rc<str>, DataId>,
}

impl<B> Codegen<B>
where
    B: Backend,
{
    pub fn from_builder(builder: B::Builder) -> Self {
        Codegen {
            module: Module::new(builder),
            extern_mapping: HashMap::new(),
            string_mapping: HashMap::new(),
        }
    }

    pub fn process(mut self, program: &isa::Program, build_standalone: bool) -> Module<B> {
        let mut builder_context = FunctionBuilderContext::new();
        if build_standalone {
            for (func_name, context) in &program.contexts {
                if let isa::IntrinsicType::Extern(external) = &context.intrinsic {
                    self.extern_mapping
                        .insert(func_name.clone(), external.clone());
                }
            }
        }
        for (func_name, context) in &program.contexts {
            if let isa::IntrinsicType::Extern(_) = &context.intrinsic {
                continue;
            }
            let mut fctx = self.module.make_context();
            self.visit_context(&context, &mut builder_context, &mut fctx);
            let id = self
                .module
                .declare_function(&func_name.to_string(), Linkage::Local, &fctx.func.signature)
                .unwrap();
            self.module
                .define_function(id, &mut fctx, &mut NullTrapSink {})
                .unwrap();
        }
        if build_standalone {
            self.generate_main(&mut builder_context);
        }
        self.module.finalize_definitions();
        self.module
    }

    fn pointer_type(&self) -> types::Type {
        self.module.isa().pointer_type()
    }

    fn ir_to_cranelift_type(&self, typed: &isa::Type) -> types::Type {
        match typed {
            isa::Type::Nil => types::I32,
            isa::Type::Bool => types::B1,
            isa::Type::I32 => types::I32,
            isa::Type::I64 => types::I64,
            isa::Type::F64 => types::F64,
            isa::Type::String => self.pointer_type(),
            _ => unreachable!(),
        }
    }

    fn generate_main(&mut self, builder_context: &mut FunctionBuilderContext) {
        let mut fctx = self.module.make_context();
        fctx.func.signature.params.push(AbiParam::new(types::I32));
        fctx.func
            .signature
            .params
            .push(AbiParam::new(self.module.isa().pointer_type()));
        fctx.func.signature.returns.push(AbiParam::new(types::I32));

        let mut builder = FunctionBuilder::new(&mut fctx.func, builder_context);
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);

        let sig = self.module.make_signature();
        let callee = self
            .module
            .declare_function("main()", Linkage::Import, &sig)
            .unwrap();
        let local_callee = self.module.declare_func_in_func(callee, &mut builder.func);
        let _call = builder.ins().call(local_callee, &[]);

        let tmp = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[tmp]);

        builder.seal_block(block);
        builder.finalize();
        std::mem::drop(builder);
        dbg_println!("{}", fctx.func.display(None));

        let id = self
            .module
            .declare_function(&"main".to_string(), Linkage::Export, &fctx.func.signature)
            .unwrap();
        self.module
            .define_function(id, &mut fctx, &mut NullTrapSink {})
            .unwrap();
    }

    fn visit_context(
        &mut self,
        context: &isa::Context,
        builder_context: &mut FunctionBuilderContext,
        fctx: &mut Context,
    ) {
        for arg in &context.args {
            fctx.func
                .signature
                .params
                .push(AbiParam::new(self.ir_to_cranelift_type(&arg)));
        }
        if context.rettype != isa::Type::NoReturn {
            fctx.func
                .signature
                .returns
                .push(AbiParam::new(self.ir_to_cranelift_type(&context.rettype)));
        }

        let mut builder = FunctionBuilder::new(&mut fctx.func, builder_context);
        let blocks: Vec<Block> = (0..context.blocks.len())
            .map(|_| builder.create_block())
            .collect();

        for (idx, arg) in context.args.iter().enumerate() {
            builder.declare_var(Variable::with_u32(idx as u32), self.ir_to_cranelift_type(&arg));
        }
        builder.append_block_params_for_function_params(blocks[0]);

        for (idx, typed) in context.variables.iter().enumerate() {
            if typed != &isa::Type::NeverUsed {
                builder.declare_var(Variable::with_u32(idx as u32), self.ir_to_cranelift_type(&typed));
            }
        }

        for (cblock, &bblock) in context.blocks.iter().zip(blocks.iter()) {
            builder.switch_to_block(bblock);
            for ins in &cblock.ins {
                self.visit_ins(&ins, &context, bblock, &blocks, &mut builder);
            }
        }
        builder.seal_all_blocks();
        builder.finalize();
        std::mem::drop(builder);

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&fctx.func, &flags);
        dbg_println!("{}", fctx.func.display(None));
        if let Err(errors) = res {
            panic!("{}", errors);
        }
    }

    fn visit_ins(
        &mut self,
        ins: &isa::Ins,
        context: &isa::Context,
        bblock: Block,
        bblocks: &Vec<Block>,
        builder: &mut FunctionBuilder,
    ) {
        match &ins.typed {
            isa::InsType::Nop => (),
            isa::InsType::LoadVar(arg) => {
                let tmp = builder.use_var(to_var(*arg));
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadArg(arg) => {
                let tmp = builder.block_params(bblock)[*arg];
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadI32(x) => {
                let tmp = builder.ins().iconst(types::I32, *x as i64);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadI64(x) => {
                let tmp = builder.ins().iconst(types::I64, *x as i64);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadString(x) => {
                let data_id = if let Some(data_id) = self.string_mapping.get(x) {
                    data_id.clone()
                } else {
                    let mut bytes_vec = x.as_bytes().to_vec();
                    bytes_vec.push(0u8);
                    let name = "__string_".to_owned() + &self.string_mapping.len().to_string();
                    let data_id = self.module
                        .declare_data(&name, Linkage::Export, false, false, None)
                        .expect("able to create data for string");
                    let mut data_ctx = DataContext::new();
                    data_ctx.define(bytes_vec.into_boxed_slice());
                    self.module.define_data(data_id, &data_ctx);
                    data_id
                };
                let value = self.module.declare_data_in_func(data_id, &mut builder.func);
                let tmp = builder.ins().symbol_value(self.pointer_type(), value);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadF64(x) => {
                let tmp = builder.ins().f64const(*x);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::Call { name, args } => {
                let mut sig = self.module.make_signature();
                let rettype = &context.variables[ins.retvar().unwrap()];
                {
                    let name: &isa::FunctionName = name;
                    for arg in &name.arg_types {
                        sig.params.push(AbiParam::new(self.ir_to_cranelift_type(&arg)));
                    }
                }
                if *rettype != isa::Type::NoReturn {
                    sig.returns
                        .push(AbiParam::new(self.ir_to_cranelift_type(&rettype)));
                }

                let callee = if let Some(mapping) = self.extern_mapping.get(&name.clone()) {
                    self.module
                        .declare_function(&mapping, Linkage::Import, &sig)
                } else {
                    self.module
                        .declare_function(&name.to_string(), Linkage::Import, &sig)
                }
                .unwrap();
                let local_callee = self.module.declare_func_in_func(callee, &mut builder.func);

                let mut arg_values = vec![];
                for arg in args {
                    arg_values.push(builder.use_var(to_var(*arg)));
                }
                let call = builder.ins().call(local_callee, &arg_values);
                if *rettype != isa::Type::NoReturn {
                    let tmp = builder.inst_results(call)[0];
                    builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                }
            }
            isa::InsType::Exit => {
                builder.ins().return_(&[]);
            }
            isa::InsType::Return(x) => {
                let arg = builder.use_var(to_var(*x));
                builder.ins().return_(&[arg]);
            }
            isa::InsType::IfJmp {
                condvar,
                iftrue,
                iffalse,
            } => {
                let var = builder.use_var(to_var(*condvar));
                builder.ins().brz(var, bblocks[*iffalse], &[]);
                builder.ins().jump(bblocks[*iftrue], &[]);
            }
            isa::InsType::Jmp(x) => {
                builder.ins().jump(bblocks[*x], &[]);
            }
            isa::InsType::Lt((x, y))
            | isa::InsType::Gt((x, y))
            | isa::InsType::Lte((x, y))
            | isa::InsType::Gte((x, y)) => {
                let left = builder.use_var(to_var(*x));
                let right = builder.use_var(to_var(*y));
                match &context.variables[*x] {
                    isa::Type::I32 | isa::Type::I64 => {
                        let tmp = builder.ins().icmp(
                            match &ins.typed {
                                isa::InsType::Lt(_) => IntCC::SignedLessThan,
                                isa::InsType::Gt(_) => IntCC::SignedGreaterThan,
                                isa::InsType::Lte(_) => IntCC::SignedLessThanOrEqual,
                                isa::InsType::Gte(_) => IntCC::SignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            },
                            left,
                            right,
                        );
                        builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                    }
                    isa::Type::F64 => {
                        let tmp = builder.ins().fcmp(
                            match &ins.typed {
                                isa::InsType::Lt(_)  => FloatCC::LessThan,
                                isa::InsType::Gt(_)  => FloatCC::GreaterThan,
                                isa::InsType::Lte(_) => FloatCC::LessThanOrEqual,
                                isa::InsType::Gte(_) => FloatCC::GreaterThanOrEqual,
                                _ => unreachable!(),
                            },
                            left,
                            right,
                        );
                        builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!(),
                }
            }
            isa::InsType::Add((x, y)) => match &context.variables[*x] {
                isa::Type::I32 | isa::Type::I64
                    => generate_arithmetic!(builder, ins, x, y, iadd),
                isa::Type::F64
                    => generate_arithmetic!(builder, ins, x, y, fadd),
                _ => unimplemented!(),
            },
            isa::InsType::Sub((x, y)) => match &context.variables[*x] {
                isa::Type::I32 | isa::Type::I64
                    => generate_arithmetic!(builder, ins, x, y, isub),
                isa::Type::F64
                    => generate_arithmetic!(builder, ins, x, y, fsub),
                _ => unimplemented!(),
            },
            isa::InsType::Mul((x, y)) => match &context.variables[*x] {
                isa::Type::I32 | isa::Type::I64
                    => generate_arithmetic!(builder, ins, x, y, imul),
                isa::Type::F64
                    => generate_arithmetic!(builder, ins, x, y, fmul),
                _ => unimplemented!(),
            },
            isa::InsType::Div((x, y)) => match &context.variables[*x] {
                isa::Type::I32 | isa::Type::I64
                    => generate_arithmetic!(builder, ins, x, y, sdiv),
                isa::Type::F64
                    => generate_arithmetic!(builder, ins, x, y, fdiv),
                _ => unimplemented!(),
            },
            isa::InsType::AddC(regconst)
            | isa::InsType::SubC(regconst)
            | isa::InsType::MulC(regconst)
            | isa::InsType::DivC(regconst) => {
                let (left, right, typed) = regconst_to_type(builder, &regconst);
                let tmp = match typed {
                    types::I32 | types::I64 => match &ins.typed {
                        isa::InsType::AddC(_) => builder.ins().iadd(left, right),
                        isa::InsType::SubC(_) => builder.ins().isub(left, right),
                        isa::InsType::MulC(_) => builder.ins().imul(left, right),
                        isa::InsType::DivC(_) => builder.ins().sdiv(left, right),
                        _ => unreachable!(),
                    },
                    types::F64 => match &ins.typed {
                        isa::InsType::AddC(_) => builder.ins().fadd(left, right),
                        isa::InsType::SubC(_) => builder.ins().fsub(left, right),
                        isa::InsType::MulC(_) => builder.ins().fmul(left, right),
                        isa::InsType::DivC(_) => builder.ins().fdiv(left, right),
                        _ => unreachable!(),
                    },
                    _ => unimplemented!(),
                };
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LtC(regconst)
            | isa::InsType::GtC(regconst)
            | isa::InsType::LteC(regconst)
            | isa::InsType::GteC(regconst) => {
                let (left, right, typed) = regconst_to_type(builder, &regconst);
                let tmp = match typed {
                    types::I32 | types::I64 => builder.ins().icmp(
                        match &ins.typed {
                            isa::InsType::LtC(_) => IntCC::SignedLessThan,
                            isa::InsType::GtC(_) => IntCC::SignedGreaterThan,
                            isa::InsType::LteC(_) => IntCC::SignedLessThanOrEqual,
                            isa::InsType::GteC(_) => IntCC::SignedGreaterThanOrEqual,
                            _ => unreachable!(),
                        },
                        left,
                        right,
                    ),
                    _ => unimplemented!(),
                };
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            x => unimplemented!("{:?}", x),
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum OptLevel {
    None,
    Speed,
    SpeedAndSize,
}

pub struct Settings {
    pub opt_level: OptLevel,
}

impl Settings {
    pub fn default() -> Self {
        Settings {
            opt_level: OptLevel::None,
        }
    }

    pub fn to_isa(&self) -> Box<dyn TargetIsa> {
        let mut flag_builder = settings::builder();
        match self.opt_level {
            OptLevel::None => flag_builder.set("opt_level", "none"),
            OptLevel::Speed => flag_builder.set("opt_level", "speed"),
            OptLevel::SpeedAndSize => flag_builder.set("opt_level", "speed_and_size"),
        }
        .unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        isa_builder.finish(settings::Flags::new(flag_builder))
    }
}

/// Simple JIT Backend
impl Codegen<SimpleJITBackend> {
    pub fn process_jit(
        _settings: Settings,
        program: &isa::Program,
        runtime: &Runtime,
    ) -> Module<SimpleJITBackend> {
        let mut builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        for (name, context) in &program.contexts {
            if let isa::IntrinsicType::Extern(symbol) = &context.intrinsic {
                builder.symbol(
                    name.to_string(),
                    runtime.funcs().get(symbol).unwrap().ptr() as _,
                );
            }
        }
        let codegen = Codegen::from_builder(builder);
        codegen.process(program, false)
    }
}

/// Object generation backend
impl Codegen<ObjectBackend> {
    pub fn process_object(settings: Settings, program: &isa::Program) -> Module<ObjectBackend> {
        let isa = settings.to_isa();
        let builder = ObjectBuilder::new(isa, "main.o", cranelift_module::default_libcall_names());
        let codegen = Codegen::from_builder(builder);
        codegen.process(program, true)
    }
}
