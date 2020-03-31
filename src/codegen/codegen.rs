use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::ExtFuncData;
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, Signature};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_codegen::ir::entities::Block;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use cranelift_codegen::Context;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ssa::isa;

pub struct Codegen {
    module: Module<SimpleJITBackend>,
}

impl Codegen {
    pub fn new() -> Self {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = Module::new(builder);
        Codegen {
            module,
        }
    }

    pub fn process(&mut self, program: &isa::Program) {
        let mut builder_context = FunctionBuilderContext::new();
        for (func_name, context) in &program.contexts {
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
    }

    fn ir_to_cranelift_type(typed: &isa::Type) -> types::Type {
        println!("typed: {:?}", typed);
        match typed {
            isa::Type::Nil => types::I32,
            isa::Type::Bool => types::B1,
            isa::Type::I32 => types::I32,
            isa::Type::I64 => types::I64,
            isa::Type::F64 => types::F64,
            _ => unreachable!(),
        }
    }

    fn visit_intrinsic(
        &mut self,
        context: &isa::Context,
        name: &Rc<isa::FunctionName>,
    ) -> bool {
        match context.intrinsic {
            isa::IntrinsicType::None => false,
            isa::IntrinsicType::Extern(generic) => {
                // TODO
                true
            }
        }
    }

    fn visit_context(
        &mut self,
        context: &isa::Context,
        name: &Rc<isa::FunctionName>,
        builder_context: &mut FunctionBuilderContext,
        fctx: &mut Context,
    ) {
        if self.visit_intrinsic(context, name) {
            return;
        }
        dbg_println!("processing {:#?}", context);

        for arg in &context.args {
            fctx.func.signature.params.push(AbiParam::new(Codegen::ir_to_cranelift_type(&arg)));
        }
        if context.rettype != isa::Type::NoReturn {
            fctx.func.signature.returns.push(AbiParam::new(Codegen::ir_to_cranelift_type(&context.rettype)));
        }
        dbg_println!("sig: {:?}", fctx.func.signature);
        let mut builder = FunctionBuilder::new(&mut fctx.func, builder_context);

        let blocks: Vec<Block> = (0..context.blocks.len()).map(|_| builder.create_block()).collect();

        for (idx, arg) in context.args.iter().enumerate() {
            builder.declare_var(Variable::with_u32(idx as u32), Codegen::ir_to_cranelift_type(&arg));
        }
        builder.append_block_params_for_function_params(blocks[0]);

        for (idx, (cblock, &bblock)) in context.blocks.iter().zip(blocks.iter()).enumerate() {
            builder.switch_to_block(bblock);
            for ins in &cblock.ins {
                self.visit_ins(&ins, &cblock, &context, bblock, &blocks, &mut builder);
            }
            builder.seal_block(bblock);
        }

        builder.finalize();
        std::mem::drop(builder);

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&fctx.func, &flags);
        println!("{}", fctx.func.display(None));
        if let Err(errors) = res {
            panic!("{}", errors);
        }
    }

    fn to_var(x: usize) -> Variable {
        Variable::with_u32(x as u32)
    }

    fn visit_ins(
        &mut self,
        ins: &isa::Ins,
        cblock: &isa::Block,
        context: &isa::Context,
        bblock: Block,
        bblocks: &Vec<Block>,
        builder: &mut FunctionBuilder,
    ) {
        if let Some(retvar) = ins.retvar() {
            builder.declare_var(
                Codegen::to_var(retvar),
                Codegen::ir_to_cranelift_type(&context.variables[retvar])
            );
        }
        match &ins.typed {
            isa::InsType::Nop => (),
            isa::InsType::LoadVar(arg) => {
                let tmp = builder.use_var(Codegen::to_var(*arg));
                builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadArg(arg) => {
                let tmp = builder.block_params(bblock)[*arg];
                builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadI32(x) => {
                let tmp = builder.ins().iconst(types::I32, *x as i64);
                builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadI64(x) => {
                let tmp = builder.ins().iconst(types::I64, *x as i64);
                builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::Call { name, args } => {
                let mut sig = self.module.make_signature();
                let rettype = &context.variables[ins.retvar().unwrap()];
                {
                    let name: &isa::FunctionName = name;
                    for arg in &name.arg_types {
                        sig.params.push(AbiParam::new(Codegen::ir_to_cranelift_type(&arg)));
                    }
                }
                if *rettype != isa::Type::NoReturn {
                    sig.returns.push(AbiParam::new(Codegen::ir_to_cranelift_type(&rettype)));
                }
                dbg_println!("call sig: {:?}", sig);

                let callee = self.module.declare_function(&name.to_string(), Linkage::Import, &sig).unwrap();
                let local_callee = self.module.declare_func_in_func(callee, &mut builder.func);

                let mut arg_values = vec![];
                for arg in args {
                    arg_values.push(builder.use_var(Codegen::to_var(*arg)));
                }
                let call = builder.ins().call(local_callee, &arg_values);
                if *rettype != isa::Type::NoReturn {
                    let tmp = builder.inst_results(call)[0];
                    builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
                }
            }
            isa::InsType::Exit => {
                builder.ins().return_(&[]);
            }
            isa::InsType::Return(x) => {
                let arg = builder.use_var(Codegen::to_var(*x));
                builder.ins().return_(&[arg]);
            }
            isa::InsType::IfJmp {
                condvar,
                iftrue,
                iffalse,
            } => {
                let var = builder.use_var(Codegen::to_var(*condvar));
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
                let left = builder.use_var(Codegen::to_var(*x));
                let right = builder.use_var(Codegen::to_var(*y));
                match &context.variables[*x] {
                    isa::Type::I32 | isa::Type::I64 => {
                        let tmp = builder.ins().icmp(
                            match &ins.typed {
                                isa::InsType::Lt((x, y))  => IntCC::SignedLessThan,
                                isa::InsType::Gt((x, y))  => IntCC::SignedGreaterThan,
                                isa::InsType::Lte((x, y)) => IntCC::SignedLessThanOrEqual,
                                isa::InsType::Gte((x, y)) => IntCC::SignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            },
                            left,
                            right,
                        );
                        builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!(),
                }
            }
            isa::InsType::Add((x, y)) => {
                match &context.variables[*x] {
                    isa::Type::I32 => {
                        let left = builder.use_var(Codegen::to_var(*x));
                        let right = builder.use_var(Codegen::to_var(*y));
                        let tmp = builder.ins().iadd(left, right);
                        builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!()
                }
            }
            isa::InsType::Sub((x, y)) => {
                match &context.variables[*x] {
                    isa::Type::I32 => {
                        let left = builder.use_var(Codegen::to_var(*x));
                        let right = builder.use_var(Codegen::to_var(*y));
                        let tmp = builder.ins().isub(left, right);
                        builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!()
                }
            },
            isa::InsType::Mul((x, y)) => {
                match &context.variables[*x] {
                    isa::Type::I32 => {
                        let left = builder.use_var(Codegen::to_var(*x));
                        let right = builder.use_var(Codegen::to_var(*y));
                        let tmp = builder.ins().imul(left, right);
                        builder.def_var(Codegen::to_var(ins.retvar().unwrap()), tmp);
                    }
                    _ => unimplemented!()
                }
            },
            isa::InsType::Div((x, y)) => unimplemented!(),
            x => unimplemented!("{:?}", x),
        }
    }
}