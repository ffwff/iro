use crate::codegen::cranelift::abi;
use crate::codegen::cranelift::translator::*;
use crate::codegen::structs::*;
use crate::runtime::Runtime;
use crate::ssa::isa;
use crate::ssa::visitor::TopLevelArch;
use cranelift::prelude::*;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::entities::{Block, StackSlot, Value};
use cranelift_codegen::ir::immediates::Offset32;
use cranelift_codegen::ir::stackslot::{StackSlotData, StackSlotKind};
use cranelift_codegen::ir::TrapCode;
use cranelift_codegen::ir::{types, AbiParam, InstBuilder, MemFlags};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Backend, DataContext, DataId, Linkage, Module};
use cranelift_object::{ObjectBackend, ObjectBuilder};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

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
        dbg_println!("codegen: {:#?}", context);

        let mut stack_loads_by_var: Vec<Vec<Offset32>> = vec![vec![]; context.args.len()];
        abi::generate_function_signature(
            self.module.isa(),
            program,
            &context.args,
            &context.rettype,
            &mut fctx.func.signature,
            |idx, offset| {
                stack_loads_by_var[idx].push(offset);
            },
        );

        let mut builder = FunctionBuilder::new(&mut fctx.func, builder_context);
        let blocks: Vec<Block> = (0..context.blocks.len())
            .map(|_| builder.create_block())
            .collect();

        for (idx, typed) in context.variables.iter().enumerate() {
            if let Some(cranelift_type) = ir_to_cranelift_type(&typed) {
                builder.declare_var(Variable::with_u32(idx as u32), cranelift_type);
            }
        }

        // Generate stack loads for struct arguments
        let mut stack_loads_ins: BTreeMap<usize, (StackSlot, Vec<(Offset32, Value)>)> = btreemap![];
        for (idx, (typed, stack_loads)) in context
            .args
            .iter()
            .zip(stack_loads_by_var.iter())
            .enumerate()
        {
            if let Some(cranelift_type) = ir_to_cranelift_type(&typed) {
                builder.append_block_param(blocks[0], cranelift_type);
            } else {
                let aggregate_data: &dyn AggregateData = typed.as_aggregate_data(program).unwrap();
                let slot = builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    aggregate_data.size_of() as u32,
                ));
                let mut loads_for_struct = vec![];
                for offset in stack_loads {
                    let tmp = builder.append_block_param(blocks[0], types::I64);
                    loads_for_struct.push((*offset, tmp));
                }
                stack_loads_ins.insert(idx, (slot, loads_for_struct));
            }
        }

        for (cblock, &bblock) in context.blocks.iter().zip(blocks.iter()) {
            builder.switch_to_block(bblock);
            for ins in &cblock.ins {
                self.visit_ins(
                    &ins,
                    &context,
                    &program,
                    bblock,
                    &blocks,
                    &stack_loads_ins,
                    &mut builder,
                );
            }
        }
        builder.seal_all_blocks();
        builder.finalize();
        std::mem::drop(builder);

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&fctx.func, &flags);
        dbg_println!("{:#?}\n{}", context.name, fctx.func.display(None));
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
        stack_loads_ins: &BTreeMap<usize, (StackSlot, Vec<(Offset32, Value)>)>,
        builder: &mut FunctionBuilder,
    ) {
        match &ins.typed {
            isa::InsType::Nop => (),
            isa::InsType::LoadVar(arg) => {
                let tmp = builder.use_var(to_var(*arg));
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadArg(arg) => {
                if let Some((slot, struct_ins)) = stack_loads_ins.get(arg) {
                    for (offset, value) in struct_ins {
                        builder.ins().stack_store(*value, *slot, *offset);
                    }
                    let pointer = builder.ins().stack_addr(self.pointer_type(), *slot, 0);
                    let retvar = ins.retvar().unwrap();
                    builder.declare_var(to_var(retvar), self.pointer_type());
                    builder.def_var(to_var(retvar), pointer);
                } else {
                    let tmp = builder.block_params(bblock)[*arg];
                    builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                }
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
                    let mut struct_builder =
                        StructBuilder::new(&program.generic_fat_pointer_struct);
                    let ptr_offset = struct_builder.insert_zeroed("address").unwrap();
                    struct_builder
                        .insert("len", &x.len().to_ne_bytes())
                        .unwrap();
                    data_ctx.define(struct_builder.into_vec().into_boxed_slice());
                    data_ctx.write_data_addr(ptr_offset as u32, bytes_value, 0);
                    self.module
                        .define_data(data_id, &data_ctx)
                        .expect("able to define data for string");
                    self.string_mapping.insert(x.clone(), data_id);
                    data_id
                };
                let value = self.module.declare_data_in_func(data_id, &mut builder.func);
                let pointer = builder.ins().symbol_value(self.pointer_type(), value);
                let retvar = ins.retvar().unwrap();
                builder.declare_var(to_var(retvar), self.pointer_type());
                builder.def_var(to_var(retvar), pointer);
            }
            isa::InsType::LoadF64(x) => {
                let tmp = builder.ins().f64const(*x);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadSlice(x) => {
                let typed = &context.variables[ins.retvar().unwrap()];
                let (slice_bytes, instance_bytes) = {
                    let slice = typed.as_slice().unwrap();
                    (
                        slice.typed.bytes().unwrap() * (slice.len.unwrap() as usize),
                        slice.typed.bytes().unwrap(),
                    )
                };
                let slot = builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    slice_bytes as u32,
                ));
                let pointer = builder.ins().stack_addr(self.pointer_type(), slot, 0);
                let retvar = ins.retvar().unwrap();
                builder.declare_var(to_var(retvar), self.pointer_type());
                builder.def_var(to_var(retvar), pointer);
                for (idx, var) in x.iter().enumerate() {
                    let tmp = builder.use_var(to_var(*var));
                    builder
                        .ins()
                        .stack_store(tmp, slot, (idx * instance_bytes) as i32);
                }
            }
            isa::InsType::LoadNil => {
                let rettype = &context.variables[ins.retvar().unwrap()];
                if *rettype == isa::Type::Nil {
                    return;
                } else {
                    unimplemented!()
                }
            }
            isa::InsType::Call { name, args } => {
                let mut sig = self.module.make_signature();
                let rettype = &context.variables[ins.retvar().unwrap()];

                let mut arg_values = vec![];
                for arg in args {
                    if ir_to_cranelift_type(&context.variables[*arg]).is_some() {
                        arg_values.push(vec![builder.use_var(to_var(*arg))]);
                    } else {
                        arg_values.push(vec![]);
                    }
                }
                {
                    let name: &isa::FunctionName = name;
                    abi::generate_function_signature(
                        self.module.isa(),
                        program,
                        &name.arg_types,
                        rettype,
                        &mut sig,
                        |idx, offset| {
                            let var = args[idx];
                            let pointer = builder.use_var(to_var(var));
                            let tmp = builder.ins().load(
                                types::I64,
                                MemFlags::trusted(),
                                pointer,
                                offset,
                            );
                            arg_values[idx].push(tmp);
                        },
                    );
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

                let arg_values: Vec<Value> = arg_values.into_iter().flatten().collect();
                let call = builder.ins().call(local_callee, &arg_values);
                if let Some(tmp) = builder.inst_results(call).first().cloned() {
                    builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                }
            }
            isa::InsType::Exit => {
                builder.ins().return_(&[]);
            }
            isa::InsType::Return(x) => {
                let rettype = &context.variables[*x];
                if *rettype == isa::Type::Nil {
                    builder.ins().return_(&[]);
                } else {
                    let arg = builder.use_var(to_var(*x));
                    builder.ins().return_(&[arg]);
                }
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
            | isa::InsType::ModC(regconst)
                if regconst
                    .as_reg_left()
                    .map(|(_, k)| k.as_i64().is_some())
                    .is_some() =>
            {
                let (left, right) = regconst.as_reg_left().unwrap();
                let int = right.as_i64().unwrap();
                let var = builder.use_var(to_var(left));
                let tmp = match &ins.typed {
                    isa::InsType::AddC(_) => builder.ins().iadd_imm(var, int),
                    isa::InsType::SubC(_) => builder.ins().iadd_imm(var, -int),
                    isa::InsType::MulC(_) => builder.ins().imul_imm(var, int),
                    isa::InsType::DivC(_) => builder.ins().sdiv_imm(var, int),
                    isa::InsType::ModC(_) => builder.ins().srem_imm(var, int),
                    _ => unreachable!(),
                };
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::AddC(regconst)
            | isa::InsType::MulC(regconst)
            | isa::InsType::SubC(regconst)
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
                let struct_data: &StructData = match &context.variables[*left] {
                    isa::Type::Struct(isa::StructType(data)) => data.borrow(),
                    typed if typed.is_fat_pointer() => &program.generic_fat_pointer_struct,
                    _ => unreachable!(),
                };
                let field_data = struct_data.values().get(right).unwrap();
                let pointer = builder.use_var(to_var(*left));
                let tmp = builder.ins().load(
                    ir_to_cranelift_type(&field_data.typed).unwrap(),
                    MemFlags::trusted(),
                    pointer,
                    field_data.offset as i32,
                );
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::Cast { var, typed } => {
                let tmp = match (&context.variables[*var], typed) {
                    (isa::Type::I32Ptr(_), isa::Type::I32) => builder.use_var(to_var(*var)),
                    (isa::Type::I64Ptr(_), isa::Type::I64) => builder.use_var(to_var(*var)),
                    (left, right) if left.is_int() && right.is_int() => {
                        let (left_size, right_size) =
                            (left.bytes().unwrap(), right.bytes().unwrap());
                        let var = builder.use_var(to_var(*var));
                        if left_size < right_size {
                            builder
                                .ins()
                                .sextend(ir_to_cranelift_type(&right).unwrap(), var)
                        } else if left_size > right_size {
                            builder
                                .ins()
                                .ireduce(ir_to_cranelift_type(&right).unwrap(), var)
                        } else {
                            var
                        }
                    }
                    _ => unreachable!(),
                };
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::PointerIndex { var, index } => {
                let retvar = ins.retvar().unwrap();
                let ptr = builder.use_var(to_var(*var));
                let index_var = builder.use_var(to_var(*index));
                let multiplicand = builder
                    .ins()
                    .imul_imm(index_var, context.variables[retvar].bytes().unwrap() as i64);
                let indexed = builder.ins().iadd(ptr, multiplicand);
                let tmp = builder.ins().load(
                    ir_to_cranelift_type(&context.variables[retvar]).unwrap(),
                    MemFlags::trusted(),
                    indexed,
                    0,
                );
                builder.def_var(to_var(retvar), tmp);
            }
            isa::InsType::PointerIndexC { var, offset } => {
                let retvar = ins.retvar().unwrap();
                let ptr = builder.use_var(to_var(*var));
                let tmp = builder.ins().load(
                    ir_to_cranelift_type(&context.variables[retvar]).unwrap(),
                    MemFlags::trusted(),
                    ptr,
                    (*offset) * context.variables[retvar].bytes().unwrap() as i32,
                );
                builder.def_var(to_var(retvar), tmp);
            }
            isa::InsType::FatIndex { var, index } => {
                let retvar = ins.retvar().unwrap();
                let fat_ptr = builder.use_var(to_var(*var));
                let ptr = builder
                    .ins()
                    .load(self.pointer_type(), MemFlags::trusted(), fat_ptr, 0);
                let index_var = builder.use_var(to_var(*index));
                dbg_println!("{:#?}", context.variables[*var]);
                let return_instance = context.variables[*var].instance_type().unwrap();
                let multiplicand = builder
                    .ins()
                    .imul_imm(index_var, return_instance.bytes().unwrap() as i64);
                let indexed = builder.ins().iadd(ptr, multiplicand);
                let tmp = builder.ins().load(
                    ir_to_cranelift_type(return_instance).unwrap(),
                    MemFlags::trusted(),
                    indexed,
                    0,
                );
                builder.def_var(to_var(retvar), tmp);
            }
            isa::InsType::FatIndexC { var, offset } => {
                let retvar = ins.retvar().unwrap();
                let fat_ptr = builder.use_var(to_var(*var));
                let ptr = builder
                    .ins()
                    .load(self.pointer_type(), MemFlags::trusted(), fat_ptr, 0);
                let tmp = builder.ins().load(
                    ir_to_cranelift_type(&context.variables[retvar]).unwrap(),
                    MemFlags::trusted(),
                    ptr,
                    (*offset) * context.variables[retvar].bytes().unwrap() as i32,
                );
                builder.def_var(to_var(retvar), tmp);
            }
            isa::InsType::BoundsCheck { var, .. } | isa::InsType::BoundsCheckC { var, .. } => {
                let len = match &context.variables[*var] {
                    maybe_fat if maybe_fat.is_fat_pointer() => {
                        let fat_ptr = builder.use_var(to_var(*var));
                        builder.ins().load(
                            self.pointer_type(),
                            MemFlags::trusted(),
                            fat_ptr,
                            self.pointer_type().bytes() as i32,
                        )
                    }
                    isa::Type::Slice(slice_rc) => {
                        let slice_type: &isa::SliceType = &slice_rc.borrow();
                        builder
                            .ins()
                            .iconst(self.pointer_type(), slice_type.len.unwrap() as i64)
                    }
                    _ => unreachable!(),
                };
                let index_var = match &ins.typed {
                    isa::InsType::BoundsCheck { index, .. } => builder.use_var(to_var(*index)),
                    isa::InsType::BoundsCheckC { offset, .. } => {
                        builder.ins().iconst(self.pointer_type(), *offset as i64)
                    }
                    _ => unreachable!(),
                };
                let tmp = builder.ins().icmp(IntCC::UnsignedLessThan, index_var, len);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::Trap(cond) => {
                match cond {
                    isa::TrapType::BoundsCheck => {
                        // TODO: generate function call
                        builder.ins().trap(TrapCode::OutOfBounds);
                    }
                }
            }
            isa::InsType::PointerStore { var, index, right } => {
                let ptr = builder.use_var(to_var(*var));
                let index_var = builder.use_var(to_var(*index));
                let right_var = builder.use_var(to_var(*right));
                let multiplicand = builder
                    .ins()
                    .imul_imm(index_var, context.variables[*var].instance_bytes().unwrap() as i64);
                let indexed = builder.ins().iadd(ptr, multiplicand);
                builder.ins().store(
                    MemFlags::trusted(),
                    right_var,
                    indexed,
                    0,
                );
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
