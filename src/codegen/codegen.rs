use crate::codegen::structs::*;
use crate::runtime::Runtime;
use crate::ssa::isa;
use crate::ssa::visitor::TopLevelArch;
use cranelift::prelude::*;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::entities::{Block, Value};
use cranelift_codegen::ir::{types, AbiParam, ArgumentPurpose, InstBuilder, MemFlags, Signature};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Backend, DataContext, DataId, Linkage, Module};
use cranelift_object::{ObjectBackend, ObjectBuilder};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;
use std::borrow::Borrow;

fn to_var(x: usize) -> Variable {
    Variable::with_u32(x as u32)
}

fn const_to_value(builder: &mut FunctionBuilder, c: &isa::Constant) -> Value {
    match c {
        isa::Constant::I32(x) => builder.ins().iconst(types::I32, *x as i64),
        isa::Constant::I64(x) => builder.ins().iconst(types::I64, *x),
        isa::Constant::F64(x) => builder.ins().f64const(*x),
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

/// Struct data for an SSA variable
struct VarStructData {
    pub pointer: Value,
    pub struct_data: Rc<StructData>,
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
            self.visit_context(&context, &program, &mut builder_context, &mut fctx);
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

    fn ir_to_cranelift_type(&self, typed: &isa::Type) -> Option<types::Type> {
        match typed {
            isa::Type::Nil => Some(types::I32),
            isa::Type::Bool => Some(types::B1),
            isa::Type::I8 => Some(types::I8),
            isa::Type::I32 | isa::Type::I32Ptr(_) => Some(types::I32),
            isa::Type::I64 | isa::Type::I64Ptr(_) => Some(types::I64),
            isa::Type::F64 => Some(types::F64),
            _ => None,
        }
    }

    fn generate_call(
        &self,
        arg_types: &Vec<isa::Type>,
        sig: &mut Signature,
        args: &Vec<usize>,
        builder: &mut FunctionBuilder,
        var_to_var_struct_data: &BTreeMap<usize, VarStructData>,
    ) {
        // Cranelift currently doesn't have any abstractions for structured data,
        // So we'll have to hand roll our own D:
        // FIXME: this only generates calls for x86-64's system-v abi
        for (idx, arg) in arg_types.iter().enumerate() {
            if let Some(cranelift_type) = self.ir_to_cranelift_type(&arg) {
                sig.params.push(AbiParam::new(cranelift_type));
            } else if let isa::Type::Struct(isa::StructType(struct_data_rc)) = arg {
                let var = args[idx];
                let var_struct_data = &var_to_var_struct_data[&var];
                let struct_data: &StructData = struct_data_rc.borrow();
                if struct_data.size_of() <= 8 {
                    // For small structs, we pass the struct as a 64-bit integer parameter
                    let tmp = builder.ins().load(
                        types::I32,
                        MemFlags::trusted(),
                        var_struct_data.pointer,
                        0,
                    );
                    builder.def_var(to_var(var), tmp);
                    sig.params.push(AbiParam::new(types::I64));
                } else if struct_data.size_of() > 16 {
                    // For large structs, push a duplicate of it into the stack
                    unimplemented!()
                } else {
                    // For medium-sized structs, manually classify each eight bytes in the struct
                    #[derive(Debug, Clone, Copy)]
                    enum AbiClass {
                        None,
                        Memory,
                        Integer,
                        SSE,
                    }
                    fn classify_type(typed: PrimitiveType) -> AbiClass {
                        match typed {
                            PrimitiveType::I8
                            | PrimitiveType::I16
                            | PrimitiveType::I32
                            | PrimitiveType::I64 => AbiClass::Integer,
                            _ => unimplemented!(),
                        }
                    }
                    let mut eight_bytes = vec![AbiClass::None; (struct_data.size_of() + 7) / 8];
                    for prim_field in struct_data.flattened() {
                        let idx = prim_field.offset / 8;
                        match (classify_type(prim_field.typed), eight_bytes[idx]) {
                            // If one of the classes is INTEGER, the result is the INTEGER.
                            (AbiClass::Integer, AbiClass::None) => {
                                eight_bytes[idx] = AbiClass::Integer;
                            },
                            // Otherwise class SSE is used.
                            (_, AbiClass::None) => {
                                eight_bytes[idx] = AbiClass::SSE;
                            },
                            _ => unreachable!(),
                        }
                    }
                    for (idx, class) in eight_bytes.iter().enumerate() {
                        match class {
                            AbiClass::Integer => {
                                let tmp = builder.ins().load(
                                    types::I64,
                                    MemFlags::trusted(),
                                    var_struct_data.pointer,
                                    idx as i32 * 8,
                                );
                                builder.ins().x86_push(tmp);
                                sig.params.push(AbiParam::new(types::I64));
                            },
                            _ => unreachable!(),
                        }
                    }
                }
            }
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
        builder.ins().call(local_callee, &[]);

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
        program: &isa::Program,
        builder_context: &mut FunctionBuilderContext,
        fctx: &mut Context,
    ) {
        for arg in &context.args {
            fctx.func
                .signature
                .params
                .push(AbiParam::new(self.ir_to_cranelift_type(&arg).unwrap()));
        }
        if context.rettype != isa::Type::NoReturn {
            fctx.func.signature.returns.push(AbiParam::new(
                self.ir_to_cranelift_type(&context.rettype).unwrap(),
            ));
        }

        let mut builder = FunctionBuilder::new(&mut fctx.func, builder_context);
        let blocks: Vec<Block> = (0..context.blocks.len())
            .map(|_| builder.create_block())
            .collect();

        for (idx, arg) in context.args.iter().enumerate() {
            if let Some(typed) = self.ir_to_cranelift_type(&arg) {
                builder.declare_var(Variable::with_u32(idx as u32), typed);
            }
        }
        builder.append_block_params_for_function_params(blocks[0]);

        for (idx, typed) in context.variables.iter().enumerate() {
            if let Some(typed) = self.ir_to_cranelift_type(&typed) {
                builder.declare_var(Variable::with_u32(idx as u32), typed);
            } else {
                builder.declare_var(Variable::with_u32(idx as u32), types::I64);
            }
        }

        let mut var_to_var_struct_data = BTreeMap::new();
        for (cblock, &bblock) in context.blocks.iter().zip(blocks.iter()) {
            builder.switch_to_block(bblock);
            for ins in &cblock.ins {
                self.visit_ins(
                    &ins,
                    &context,
                    &program,
                    bblock,
                    &blocks,
                    &mut builder,
                    &mut var_to_var_struct_data,
                );
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
        program: &isa::Program,
        bblock: Block,
        bblocks: &Vec<Block>,
        builder: &mut FunctionBuilder,
        var_to_var_struct_data: &mut BTreeMap<usize, VarStructData>,
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
            isa::InsType::LoadBool(x) => {
                let tmp = builder.ins().bconst(types::B1, *x);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadSubstring(x) => {
                let data_id = if let Some(data_id) = self.string_mapping.get(x) {
                    data_id.clone()
                } else {
                    use std::fmt::Write;

                    let bytes_vec = x.as_bytes().to_vec();
                    let bytes_data_id = {
                        let mut name = String::new();
                        write!(name, "__string_{}_bytes", self.string_mapping.len()).unwrap();
                        let data_id = self
                            .module
                            .declare_data(&name, Linkage::Local, false, false, None)
                            .expect("able to create data for string bytes");
                        let mut data_ctx = DataContext::new();
                        data_ctx.define(bytes_vec.into_boxed_slice());
                        self.module
                            .define_data(data_id, &data_ctx)
                            .expect("able to define data for string bytes");
                        data_id
                    };

                    let mut name = String::new();
                    write!(name, "__string_{}", self.string_mapping.len()).unwrap();
                    let data_id = self
                        .module
                        .declare_data(&name, Linkage::Local, false, false, None)
                        .expect("able to create data for string");
                    let mut data_ctx = DataContext::new();
                    let bytes_value = self
                        .module
                        .declare_data_in_data(bytes_data_id, &mut data_ctx);
                    let mut struct_builder = StructBuilder::new(&program.substring_struct);
                    let ptr_offset = struct_builder.insert_zeroed("ptr").unwrap();
                    struct_builder
                        .insert("len", &(x.len() as u32).to_ne_bytes())
                        .unwrap();
                    data_ctx.define(struct_builder.into_vec().into_boxed_slice());
                    data_ctx.write_data_addr(ptr_offset as u32, bytes_value, 0);
                    self.module
                        .define_data(data_id, &data_ctx)
                        .expect("able to define data for string");
                    data_id
                };
                let value = self.module.declare_data_in_func(data_id, &mut builder.func);
                let pointer = builder.ins().symbol_value(self.pointer_type(), value);
                var_to_var_struct_data.insert(
                    ins.retvar().unwrap(),
                    VarStructData {
                        pointer,
                        struct_data: program.substring_struct.clone(),
                    },
                );
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
                    self.generate_call(
                        &name.arg_types,
                        &mut sig,
                        &args,
                        builder,
                        var_to_var_struct_data
                    );
                }
                if *rettype != isa::Type::NoReturn {
                    sig.returns
                        .push(AbiParam::new(self.ir_to_cranelift_type(&rettype).unwrap()));
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
            | isa::InsType::Gte((x, y))
            | isa::InsType::Equ((x, y)) => {
                let left = builder.use_var(to_var(*x));
                let right = builder.use_var(to_var(*y));
                match &context.variables[*x] {
                    maybe_int if maybe_int.is_int() => {
                        let tmp = builder.ins().icmp(
                            match &ins.typed {
                                isa::InsType::Lt(_) => IntCC::SignedLessThan,
                                isa::InsType::Gt(_) => IntCC::SignedGreaterThan,
                                isa::InsType::Lte(_) => IntCC::SignedLessThanOrEqual,
                                isa::InsType::Gte(_) => IntCC::SignedGreaterThanOrEqual,
                                isa::InsType::Equ(_) => IntCC::Equal,
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
                                isa::InsType::Lt(_) => FloatCC::LessThan,
                                isa::InsType::Gt(_) => FloatCC::GreaterThan,
                                isa::InsType::Lte(_) => FloatCC::LessThanOrEqual,
                                isa::InsType::Gte(_) => FloatCC::GreaterThanOrEqual,
                                isa::InsType::Equ(_) => FloatCC::Equal,
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
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, iadd),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fadd),
                _ => unimplemented!(),
            },
            isa::InsType::Sub((x, y)) => match &context.variables[*x] {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, isub),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fsub),
                _ => unimplemented!(),
            },
            isa::InsType::Mul((x, y)) => match &context.variables[*x] {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, imul),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fmul),
                _ => unimplemented!(),
            },
            isa::InsType::Div((x, y)) => match &context.variables[*x] {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, sdiv),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fdiv),
                _ => unimplemented!(),
            },
            isa::InsType::Mod((x, y)) => match &context.variables[*x] {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, srem),
                _ => unimplemented!(),
            },
            isa::InsType::AddC(regconst)
            | isa::InsType::SubC(regconst)
            | isa::InsType::MulC(regconst)
            | isa::InsType::DivC(regconst)
            | isa::InsType::ModC(regconst) => {
                let (left, right, typed) = regconst_to_type(builder, &regconst);
                let tmp = match typed {
                    maybe_int if maybe_int.is_int() => match &ins.typed {
                        isa::InsType::AddC(_) => builder.ins().iadd(left, right),
                        isa::InsType::SubC(_) => builder.ins().isub(left, right),
                        isa::InsType::MulC(_) => builder.ins().imul(left, right),
                        isa::InsType::DivC(_) => builder.ins().sdiv(left, right),
                        isa::InsType::ModC(_) => builder.ins().srem(left, right),
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
            isa::InsType::MemberReference { left, right } => {
                let struct_data_rc =
                    if let isa::Type::Struct(isa::StructType(data)) = &context.variables[*left] {
                        data.clone()
                    } else {
                        unimplemented!()
                    };
                let struct_data: &StructData = struct_data_rc.borrow();
                let field_data = struct_data.values().get(right).unwrap();
                let var_struct_data = var_to_var_struct_data.get(&left).unwrap();
                let tmp = builder.ins().load(
                    self.ir_to_cranelift_type(&field_data.typed).unwrap(),
                    MemFlags::trusted(),
                    var_struct_data.pointer,
                    field_data.offset as i32,
                );
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::Cast { var, typed } => {
                let tmp = match (&context.variables[*var], typed) {
                    (isa::Type::I64Ptr(_), isa::Type::I64) => {
                        builder.use_var(to_var(*var))
                    }
                    _ => unreachable!()
                };
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::PointerIndexC { var, offset } => {
                let retvar = ins.retvar().unwrap();
                let ptr = builder.use_var(to_var(*var));
                let tmp = builder.ins().load(
                    self.ir_to_cranelift_type(&context.variables[retvar]).unwrap(),
                    MemFlags::trusted(),
                    ptr,
                    *offset,
                );
                builder.def_var(to_var(retvar), tmp);
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

    pub fn generate_arch(&self) -> (TopLevelArch, Box<dyn TargetIsa>) {
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
        let isa = isa_builder.finish(settings::Flags::new(flag_builder));
        let arch = TopLevelArch {
            pointer_bits: isa.pointer_type().bits() as usize,
        };
        (arch, isa)
    }
}

/// Simple JIT Backend
impl Codegen<SimpleJITBackend> {
    pub fn process_jit(
        _isa: Box<dyn TargetIsa>,
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
    pub fn process_object(
        isa: Box<dyn TargetIsa>,
        program: &isa::Program,
    ) -> Module<ObjectBackend> {
        let builder = ObjectBuilder::new(isa, "main.o", cranelift_module::default_libcall_names());
        let codegen = Codegen::from_builder(builder);
        codegen.process(program, true)
    }
}
