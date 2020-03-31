use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{AbiParam, InstBuilder};
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_codegen::ir::entities::Block;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_module::{Linkage, Module, Backend};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use cranelift_codegen::Context;
use cranelift_codegen::ir::entities::Value;
use std::rc::Rc;
use std::collections::HashMap;
use crate::ssa::isa;
use crate::runtime::Runtime;

fn to_var(x: usize) -> Variable {
    Variable::with_u32(x as u32)
}

fn ir_to_cranelift_type(typed: &isa::Type) -> types::Type {
    match typed {
        isa::Type::Nil => types::I32,
        isa::Type::Bool => types::B1,
        isa::Type::I32 => types::I32,
        isa::Type::I64 => types::I64,
        isa::Type::F64 => types::F64,
        _ => unreachable!(),
    }
}

fn const_to_value(builder: &mut FunctionBuilder, c: &isa::Constant) -> Value {
    match c {
        isa::Constant::I32(x) => builder.ins().iconst(types::I32, *x as i64),
        isa::Constant::I64(x) => builder.ins().iconst(types::I64, *x),
        isa::Constant::F64(x) => unimplemented!(),
    }
}

fn const_to_type(c: &isa::Constant) -> types::Type {
    match c {
        isa::Constant::I32(x) => types::I32,
        isa::Constant::I64(x) => types::I64,
        isa::Constant::F64(x) => types::F64,
    }
}

fn regconst_to_type(builder: &mut FunctionBuilder, rc: &isa::RegConst) -> (Value, Value, types::Type) {
    match rc {
        isa::RegConst::RegLeft((reg, value))
            => (builder.use_var(to_var(*reg)), const_to_value(builder, value), const_to_type(value)),
        isa::RegConst::RegRight((value, reg))
            => (const_to_value(builder, value), builder.use_var(to_var(*reg)), const_to_type(value)),
    }
}

/// Code generator
pub struct Codegen<B: Backend> {
    module: Module<B>,
}

impl<B> Codegen<B> where B: Backend {
    pub fn from_builder(builder: B::Builder) -> Self {
        Codegen {
            module: Module::new(builder),
        }
    }

    pub fn process(mut self, program: &isa::Program) -> Module<B> {
        let mut builder_context = FunctionBuilderContext::new();
        for (func_name, context) in &program.contexts {
            if let isa::IntrinsicType::Extern(_) = context.intrinsic {
                continue;
            }
            let mut fctx = self.module.make_context();
            self.visit_context(&context, &func_name, &mut builder_context, &mut fctx);
            let id = self.module.declare_function(
                &func_name.to_string(),
                Linkage::Export,
                &fctx.func.signature
            ).unwrap();
            self.module.define_function(
                id,
                &mut fctx,
                &mut NullTrapSink {},
            ).unwrap();
        }
        self.module.finalize_definitions();
        self.module
    }

    fn visit_context(
        &mut self,
        context: &isa::Context,
        name: &Rc<isa::FunctionName>,
        builder_context: &mut FunctionBuilderContext,
        fctx: &mut Context,
    ) {
        for arg in &context.args {
            fctx.func.signature.params.push(AbiParam::new(ir_to_cranelift_type(&arg)));
        }
        if context.rettype != isa::Type::NoReturn {
            fctx.func.signature.returns.push(AbiParam::new(ir_to_cranelift_type(&context.rettype)));
        }

        let mut builder = FunctionBuilder::new(&mut fctx.func, builder_context);
        let blocks: Vec<Block> = (0..context.blocks.len()).map(|_| builder.create_block()).collect();

        for (idx, arg) in context.args.iter().enumerate() {
            builder.declare_var(Variable::with_u32(idx as u32), ir_to_cranelift_type(&arg));
        }
        builder.append_block_params_for_function_params(blocks[0]);

        for (idx, typed) in context.variables.iter().enumerate() {
            if typed != &isa::Type::NeverUsed {
                builder.declare_var(Variable::with_u32(idx as u32), ir_to_cranelift_type(&typed));
            }
        }

        for (idx, (cblock, &bblock)) in context.blocks.iter().zip(blocks.iter()).enumerate() {
            builder.switch_to_block(bblock);
            for ins in &cblock.ins {
                self.visit_ins(&ins, &cblock, &context, bblock, &blocks, &mut builder);
            }
            if let Some(ins) = cblock.ins.last() {
                if !ins.typed.is_jmp() {
                    builder.ins().jump(blocks[idx + 1], &[]);
                }
            } else {
                builder.ins().jump(blocks[idx + 1], &[]);
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
        _cblock: &isa::Block,
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
            isa::InsType::Call { name, args } => {
                let mut sig = self.module.make_signature();
                let rettype = &context.variables[ins.retvar().unwrap()];
                {
                    let name: &isa::FunctionName = name;
                    for arg in &name.arg_types {
                        sig.params.push(AbiParam::new(ir_to_cranelift_type(&arg)));
                    }
                }
                if *rettype != isa::Type::NoReturn {
                    sig.returns.push(AbiParam::new(ir_to_cranelift_type(&rettype)));
                }

                let callee = self.module.declare_function(&name.to_string(), Linkage::Import, &sig).unwrap();
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
                                isa::InsType::Lt((_x, _y))  => IntCC::SignedLessThan,
                                isa::InsType::Gt((_x, _y))  => IntCC::SignedGreaterThan,
                                isa::InsType::Lte((_x, _y)) => IntCC::SignedLessThanOrEqual,
                                isa::InsType::Gte((_x, _y)) => IntCC::SignedGreaterThanOrEqual,
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
            isa::InsType::Add((x, y)) => {
                match &context.variables[*x] {
                    isa::Type::I32 | isa::Type::I64 => {
                        let left = builder.use_var(to_var(*x));
                        let right = builder.use_var(to_var(*y));
                        let tmp = builder.ins().iadd(left, right);
                        builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!()
                }
            }
            isa::InsType::Sub((x, y)) => {
                match &context.variables[*x] {
                    isa::Type::I32 | isa::Type::I64 => {
                        let left = builder.use_var(to_var(*x));
                        let right = builder.use_var(to_var(*y));
                        let tmp = builder.ins().isub(left, right);
                        builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!()
                }
            },
            isa::InsType::Mul((x, y)) => {
                match &context.variables[*x] {
                    isa::Type::I32 | isa::Type::I64 => {
                        let left = builder.use_var(to_var(*x));
                        let right = builder.use_var(to_var(*y));
                        let tmp = builder.ins().imul(left, right);
                        builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!()
                }
            },
            isa::InsType::Div((_x, _y)) => unimplemented!(),
            isa::InsType::AddC(regconst) |
            isa::InsType::SubC(regconst) |
            isa::InsType::MulC(regconst) |
            isa::InsType::DivC(regconst)
                => {
                let (left, right, typed) = regconst_to_type(builder, &regconst);
                let tmp = match typed {
                    types::I32 | types::I64 => {
                        match &ins.typed {
                            isa::InsType::AddC(_) => builder.ins().iadd(left, right),
                            isa::InsType::SubC(_) => builder.ins().isub(left, right),
                            isa::InsType::MulC(_) => builder.ins().imul(left, right),
                            isa::InsType::DivC(_) => unimplemented!(),
                            _ => unreachable!()
                        }
                    }
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
                    types::I32 | types::I64 => {
                        builder.ins().icmp(
                            match &ins.typed {
                                isa::InsType::LtC(_)  => IntCC::SignedLessThan,
                                isa::InsType::GtC(_)  => IntCC::SignedGreaterThan,
                                isa::InsType::LteC(_) => IntCC::SignedLessThanOrEqual,
                                isa::InsType::GteC(_) => IntCC::SignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            },
                            left,
                            right,
                        )
                    },
                    _ => unimplemented!(),
                };
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            x => unimplemented!("{:?}", x),
        }
    }
}

/// Simple JIT Backend
impl Codegen<SimpleJITBackend> {
    pub fn process_jit(program: &isa::Program, runtime: &Runtime) -> Module<SimpleJITBackend> {
        let mut builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        for (name, context) in &program.contexts {
            if let isa::IntrinsicType::Extern(symbol) = &context.intrinsic {
                builder.symbol(name.to_string(), runtime.funcs().get(symbol).unwrap().ptr() as _);
            }
        }
        let codegen = Codegen::from_builder(builder);
        codegen.process(program)
    }
}