use crate::codegen::backend;
use crate::codegen::cranelift::abi;
use crate::codegen::cranelift::translator::*;
use crate::codegen::settings::*;
use crate::codegen::structs::*;
use crate::compiler;
use crate::runtime::Runtime;
use crate::ssa::isa;

use cranelift::prelude::*;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::entities::{Block, StackSlot, Value};
use cranelift_codegen::ir::immediates::Offset32;
use cranelift_codegen::ir::stackslot::{StackSlotData, StackSlotKind};
use cranelift_codegen::ir::{types, AbiParam, InstBuilder, MemFlags};
use cranelift_codegen::isa::{TargetFrontendConfig, TargetIsa};
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Backend, DataContext, DataId, FuncOrDataId, Linkage, Module};
use cranelift_object::{ObjectBackend, ObjectBuilder};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

use std::collections::BTreeMap;
use std::rc::Rc;
use fnv::FnvHashMap;

macro_rules! generate_arithmetic {
    ($builder:expr, $ins:expr, $x:expr, $y:expr, $fn:tt) => {{
        let left = $builder.use_var(to_var(*$x));
        let right = $builder.use_var(to_var(*$y));
        let tmp = $builder.ins().$fn(left, right);
        $builder.def_var(to_var($ins.retvar().unwrap()), tmp);
    }};
}

struct InsContext<'a> {
    pub context: &'a isa::Context,
    pub program: &'a isa::Program,
    pub bblocks: &'a Vec<Block>,
    pub stack_loads_ins: &'a BTreeMap<usize, (StackSlot, Vec<(Offset32, Value)>)>,
    pub struct_return: Option<Value>,
}

/// Code generator
pub struct Codegen<B: Backend> {
    pub(super) module: Module<B>,
    extern_mapping: FnvHashMap<Rc<isa::FunctionName>, String>,
    string_mapping: FnvHashMap<Rc<str>, DataId>,
}

impl<B> Codegen<B>
where
    B: Backend,
{
    pub fn from_builder(builder: B::Builder) -> Self {
        Codegen {
            module: Module::new(builder),
            extern_mapping: fnv_hashmap![],
            string_mapping: fnv_hashmap![],
        }
    }

    pub fn ir_to_cranelift_type(&self, typed: &isa::Type) -> Option<types::Type> {
        match typed {
            isa::Type::Nil => Some(types::I32),
            isa::Type::Bool => Some(types::B1),
            isa::Type::I8 => Some(types::I8),
            isa::Type::I32 => Some(types::I32),
            isa::Type::I64 => Some(types::I64),
            isa::Type::ISize | isa::Type::Pointer(_) if !typed.is_fat_pointer() => {
                Some(self.pointer_type())
            }
            isa::Type::F64 => Some(types::F64),
            _ => None,
        }
    }

    pub fn process(mut self, program: &isa::Program, build_standalone: bool) -> Module<B> {
        self.build_struct_data_for_struct(&program, &program.builtins.generic_fat_pointer_struct);
        for (_, astruct) in &program.builtins.structs {
            self.build_struct_data_for_struct(&program, astruct);
        }
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

    fn frontend_config(&self) -> TargetFrontendConfig {
        self.module.isa().frontend_config()
    }

    fn type_to_struct_field_type(
        &self,
        program: &isa::Program,
        typed: &isa::Type,
    ) -> Option<StructFieldType> {
        match typed {
            isa::Type::I8 => Some(StructFieldType::I8),
            isa::Type::I16 => Some(StructFieldType::I16),
            isa::Type::I32 => Some(StructFieldType::I32),
            isa::Type::I64 => Some(StructFieldType::I64),
            isa::Type::ISize => StructFieldType::int_from_bits(self.pointer_type().bits()),
            isa::Type::F64 => Some(StructFieldType::F64),
            isa::Type::Pointer(_) => {
                if typed.is_fat_pointer() {
                    Some(StructFieldType::Struct(
                        program
                            .builtins
                            .generic_fat_pointer_struct
                            .data
                            .clone()
                            .into_inner()
                            .unwrap(),
                    ))
                } else {
                    StructFieldType::int_from_bits(self.pointer_type().bits())
                }
            }
            _ => None,
        }
    }

    pub(super) fn get_struct_data(
        &self,
        program: &isa::Program,
        typed: &isa::Type,
    ) -> Option<Rc<StructData>> {
        match typed {
            isa::Type::Struct(data) => Some(self.build_struct_data_for_struct(program, &data)),
            isa::Type::Slice(data) => Some(self.build_struct_data_for_slice(program, &data)),
            isa::Type::Union(data) => Some(self.build_struct_data_for_union(program, &data)),
            maybe_ptr if maybe_ptr.is_fat_pointer() => program
                .builtins
                .generic_fat_pointer_struct
                .data
                .clone()
                .into_inner(),
            _ => None,
        }
    }

    fn build_struct_data_for_struct(
        &self,
        program: &isa::Program,
        typed: &isa::StructType,
    ) -> Rc<StructData> {
        if let Some(data) = typed.data.borrow() {
            return data.clone();
        }
        let mut data = StructData::new();
        for field in typed.fields() {
            if let Some(typed) = self.type_to_struct_field_type(program, &field.typed) {
                data.append_typed(typed);
            } else if let Some(substruct) = self.get_struct_data(program, &field.typed) {
                data.append_struct(substruct);
            } else {
                unimplemented!()
            }
        }
        let rc = Rc::new(data);
        typed.data.replace(rc.clone());
        rc
    }

    fn build_struct_data_for_slice(
        &self,
        program: &isa::Program,
        typed: &isa::SliceType,
    ) -> Rc<StructData> {
        if let Some(data) = typed.data.borrow() {
            return data.clone();
        }
        let mut data = StructData::new();
        if let Some(field) = self.type_to_struct_field_type(program, &typed.typed) {
            data.append_array(field, typed.len.unwrap());
        } else if let Some(substruct) = self.get_struct_data(program, &typed.typed) {
            data.append_struct_array(substruct, typed.len.unwrap());
        } else {
            unimplemented!()
        }
        let rc = Rc::new(data);
        typed.data.replace(rc.clone());
        rc
    }

    fn build_struct_data_for_union(
        &self,
        program: &isa::Program,
        typed: &isa::UnionType,
    ) -> Rc<StructData> {
        if let Some(data) = typed.data.borrow() {
            return data.clone();
        }
        let mut builder = UnionBuilder::new();
        for field_typed in typed.types() {
            if let Some(field_typed) = self.type_to_struct_field_type(program, &field_typed) {
                builder.insert_typed(field_typed);
            } else if let Some(substruct) = self.get_struct_data(program, &field_typed) {
                builder.insert_struct(substruct);
            } else {
                unimplemented!()
            }
        }
        let rc = Rc::new(builder.into_struct_data());
        typed.data.replace(rc.clone());
        rc
    }

    fn size_of(&self, program: &isa::Program, typed: &isa::Type) -> u32 {
        if let Some(struct_data) = self.get_struct_data(&program, typed) {
            struct_data.size_of()
        } else {
            self.ir_to_cranelift_type(typed).unwrap().bytes()
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
        dbg_println!("codegen: {}", context.print());

        let mut stack_loads_by_var: Vec<Vec<Offset32>> = vec![vec![]; context.args.len()];
        let mut has_struct_return = false;
        self.generate_function_signature(
            program,
            &context.args,
            &context.rettype,
            &mut fctx.func.signature,
            |arg| match arg {
                abi::LoadFunctionArg::Return(_) => {
                    has_struct_return = true;
                }
                abi::LoadFunctionArg::PrimitiveArg(_) => (),
                abi::LoadFunctionArg::StructArg { idx, offset, .. } => {
                    stack_loads_by_var[idx].push(offset);
                }
            },
        );

        let mut builder = FunctionBuilder::new(&mut fctx.func, builder_context);
        let blocks: Vec<Block> = (0..context.blocks.len())
            .map(|_| builder.create_block())
            .collect();

        let struct_return = if has_struct_return {
            Some(builder.append_block_param(blocks[0], self.pointer_type()))
        } else {
            None
        };

        for (idx, typed) in context.variables.iter().enumerate() {
            if let Some(cranelift_type) = self.ir_to_cranelift_type(&typed) {
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
            if let Some(cranelift_type) = self.ir_to_cranelift_type(&typed) {
                builder.append_block_param(blocks[0], cranelift_type);
            } else {
                let struct_data = self.get_struct_data(&program, typed).unwrap();
                let slot = builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    struct_data.size_of(),
                ));
                let mut loads_for_struct = vec![];
                for offset in stack_loads {
                    let tmp = builder.append_block_param(blocks[0], types::I64);
                    loads_for_struct.push((*offset, tmp));
                }
                stack_loads_ins.insert(idx, (slot, loads_for_struct));
            }
        }

        let ins_context = InsContext {
            context,
            program,
            bblocks: &blocks,
            stack_loads_ins: &stack_loads_ins,
            struct_return,
        };
        for (cblock, &bblock) in context.blocks.iter().zip(blocks.iter()) {
            builder.switch_to_block(bblock);
            for ins in &cblock.ins {
                self.visit_ins(&ins, bblock, &mut builder, &ins_context);
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

    fn visit_member_ref(
        &mut self,
        left: isa::Variable,
        indices: &Box<[isa::MemberExprIndex]>,
        builder: &mut FunctionBuilder,
        ins_context: &InsContext,
    ) -> (Value, isa::Type) {
        let mut ptr = builder.use_var(to_var(left));
        let mut current_offset = 0u32;

        #[inline]
        fn flush_ptr(builder: &mut FunctionBuilder, ptr: &mut Value, current_offset: &mut u32) {
            if *current_offset != 0u32 {
                *ptr = builder.ins().iadd_imm(*ptr, *current_offset as i64);
            }
            *current_offset = 0u32;
        }

        let mut last_typed = ins_context.context.variable(left);
        for (idx, index) in indices.iter().enumerate() {
            let do_dereference = idx != indices.len() - 1;
            match &index.var {
                isa::MemberExprIndexVar::StructIndex(x) => {
                    let struct_data = self
                        .get_struct_data(&ins_context.program, last_typed)
                        .unwrap();
                    current_offset += struct_data.fields()[*x].offset;
                }
                isa::MemberExprIndexVar::Variable(x) => {
                    flush_ptr(builder, &mut ptr, &mut current_offset);
                    let instance_type = last_typed.instance_type().unwrap();
                    let instance_bytes = self.size_of(&ins_context.program, instance_type);
                    match last_typed {
                        maybe_ptr if maybe_ptr.is_fat_pointer() => {
                            // NOTE: the raw pointer contained in a fat pointer
                            // is always at offset 0
                            let raw_ptr = builder.ins().load(
                                self.pointer_type(),
                                MemFlags::trusted(),
                                ptr,
                                0,
                            );
                            let idx = builder.use_var(to_var(*x));
                            // Bounds check
                            let len = builder.ins().load(
                                self.pointer_type(),
                                MemFlags::trusted(),
                                ptr,
                                self.pointer_type().bytes() as i32,
                            );
                            let cmp = builder.ins().ifcmp(idx, len);
                            builder.ins().trapif(
                                IntCC::UnsignedGreaterThanOrEqual,
                                cmp,
                                TrapCode::OutOfBounds,
                            );
                            // Dereference
                            let idx_scaled = builder.ins().imul_imm(idx, instance_bytes as i64);
                            ptr = builder.ins().iadd(raw_ptr, idx_scaled);
                            if do_dereference {
                                ptr = builder.ins().load(
                                    self.ir_to_cranelift_type(instance_type).unwrap(),
                                    MemFlags::trusted(),
                                    ptr,
                                    0,
                                );
                            }
                        }
                        isa::Type::Slice(slice) => {
                            let idx = builder.use_var(to_var(*x));
                            // Bounds check
                            let cmp = builder.ins().ifcmp_imm(idx, slice.len.unwrap() as i64);
                            builder.ins().trapif(
                                IntCC::UnsignedGreaterThanOrEqual,
                                cmp,
                                TrapCode::OutOfBounds,
                            );
                            // Dereference
                            let idx_scaled = builder.ins().imul_imm(idx, instance_bytes as i64);
                            ptr = builder.ins().iadd(ptr, idx_scaled);
                            if do_dereference {
                                ptr = builder.ins().load(
                                    self.ir_to_cranelift_type(instance_type).unwrap(),
                                    MemFlags::trusted(),
                                    ptr,
                                    0,
                                );
                            }
                        }
                        isa::Type::Pointer(_) => {
                            let raw_ptr = builder.ins().load(
                                self.pointer_type(),
                                MemFlags::trusted(),
                                ptr,
                                0,
                            );
                            let idx = builder.use_var(to_var(*x));
                            let idx_scaled = builder.ins().imul_imm(idx, instance_bytes as i64);
                            ptr = builder.ins().iadd(raw_ptr, idx_scaled);
                            if do_dereference {
                                ptr = builder.ins().load(
                                    self.ir_to_cranelift_type(instance_type).unwrap(),
                                    MemFlags::trusted(),
                                    ptr,
                                    0,
                                );
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
            last_typed = &index.typed;
        }
        flush_ptr(builder, &mut ptr, &mut current_offset);
        (ptr, last_typed.clone())
    }

    fn visit_ins(
        &mut self,
        ins: &isa::Ins,
        bblock: Block,
        builder: &mut FunctionBuilder,
        ins_context: &InsContext,
    ) {
        dbg_println!("codegen: {}", ins.print());
        let context = ins_context.context;
        match &ins.typed {
            isa::InsType::MarkMoved(_) => (),
            isa::InsType::Drop(_arg) => {
                // TODO: implement me
            }
            isa::InsType::Copy(arg) | isa::InsType::Move(arg) => {
                let tmp = builder.use_var(to_var(*arg));
                let typed = builder.func.dfg.value_type(tmp);
                builder.declare_var(to_var(ins.retvar().unwrap()), typed);
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::Alloca => {
                let retvar = ins.retvar().unwrap();
                let typed = self.ir_to_cranelift_type(context.variable(retvar)).unwrap();
                let slot = builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    typed.bytes() as u32,
                ));
                let pointer = builder.ins().stack_addr(self.pointer_type(), slot, 0);
                builder.declare_var(to_var(retvar), self.pointer_type());
                builder.def_var(to_var(retvar), pointer);
            }
            isa::InsType::Load(x) => {
                let retvar = ins.retvar().unwrap();
                let typed = self.ir_to_cranelift_type(context.variable(retvar)).unwrap();
                let src_var = builder.use_var(to_var(*x));
                let tmp = builder.ins().load(typed, MemFlags::trusted(), src_var, 0);
                builder.declare_var(to_var(retvar), typed);
                builder.def_var(to_var(retvar), tmp);
            }
            isa::InsType::Store { source, dest } => {
                let dest_typed = context.variable(*dest);
                let source_typed = context.variable(*source);
                if let Some(union_type) = dest_typed.as_union() {
                    let struct_data =
                        self.build_struct_data_for_union(&ins_context.program, &union_type);
                    if let Some(idx) = union_type.index(&source_typed) {
                        // FIXME: check discriminant type
                        let field = &struct_data.fields()[idx + 1];
                        if self.ir_to_cranelift_type(&source_typed).is_some() {
                            let src_var = builder.use_var(to_var(*source));
                            let dest_var = builder.use_var(to_var(*dest));
                            builder.ins().store(
                                MemFlags::trusted(),
                                src_var,
                                dest_var,
                                field.offset as i32,
                            );
                        } else {
                            unimplemented!()
                        }
                    } else {
                        unreachable!()
                    }
                } else {
                    let src_var = builder.use_var(to_var(*source));
                    let dest_var = builder.use_var(to_var(*dest));
                    builder
                        .ins()
                        .store(MemFlags::trusted(), src_var, dest_var, 0);
                }
            }
            isa::InsType::Borrow { var, .. } => {
                let tmp = builder.use_var(to_var(*var));
                builder.declare_var(to_var(ins.retvar().unwrap()), self.pointer_type());
                builder.def_var(to_var(ins.retvar().unwrap()), tmp);
            }
            isa::InsType::LoadArg(arg) => {
                let arg = if ins_context.struct_return.is_some() {
                    *arg + 1
                } else {
                    *arg
                };
                if let Some((slot, struct_ins)) = ins_context.stack_loads_ins.get(&arg) {
                    for (offset, value) in struct_ins {
                        builder.ins().stack_store(*value, *slot, *offset);
                    }
                    let pointer = builder.ins().stack_addr(self.pointer_type(), *slot, 0);
                    let retvar = ins.retvar().unwrap();
                    builder.declare_var(to_var(retvar), self.pointer_type());
                    builder.def_var(to_var(retvar), pointer);
                } else {
                    let tmp = builder.block_params(bblock)[arg];
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
                    let struct_data = ins_context
                        .program
                        .builtins
                        .generic_fat_pointer_struct
                        .data
                        .borrow()
                        .unwrap();
                    let mut struct_builder = StructBuilder::new(&struct_data);
                    struct_builder.append_zeroed(); // address
                    struct_builder.append(&x.len().to_ne_bytes()); // len
                    data_ctx.define(struct_builder.into_vec().into_boxed_slice());
                    data_ctx.write_data_addr(0u32, bytes_value, 0);
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
                let typed = context.variable(ins.retvar().unwrap());
                let (slice_bytes, instance_bytes) = {
                    let slice = typed.as_slice().unwrap();
                    let instance_bytes = self.size_of(&ins_context.program, &slice.typed);
                    (instance_bytes * slice.len.unwrap(), instance_bytes)
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
                        .stack_store(tmp, slot, (idx as i32) * (instance_bytes as i32));
                }
            }
            isa::InsType::LoadStruct => {
                let typed = context.variable(ins.retvar().unwrap());
                let struct_data = self.get_struct_data(&ins_context.program, typed).unwrap();
                let slot = builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    struct_data.size_of() as u32,
                ));
                let pointer = builder.ins().stack_addr(self.pointer_type(), slot, 0);
                let retvar = ins.retvar().unwrap();
                builder.declare_var(to_var(retvar), self.pointer_type());
                builder.def_var(to_var(retvar), pointer);
            }
            isa::InsType::LoadNil => {
                let rettype = context.variable(ins.retvar().unwrap());
                if *rettype == isa::Type::Nil {
                    return;
                } else {
                    unimplemented!()
                }
            }
            isa::InsType::Call { name, args } => {
                let mut sig = self.module.make_signature();
                let rettype = context.variable(ins.retvar().unwrap());

                let mut arg_values = vec![];
                let name: &isa::FunctionName = &ins_context.context.call_names[*name as usize];
                {
                    self.generate_function_signature(
                        ins_context.program,
                        &name.arg_types,
                        rettype,
                        &mut sig,
                        |arg| match arg {
                            abi::LoadFunctionArg::Return(struct_data) => {
                                let slot = builder.create_stack_slot(StackSlotData::new(
                                    StackSlotKind::ExplicitSlot,
                                    struct_data.size_of() as u32,
                                ));
                                let pointer =
                                    builder.ins().stack_addr(self.pointer_type(), slot, 0);
                                arg_values.push(pointer);
                                builder.declare_var(
                                    to_var(ins.retvar().unwrap()),
                                    self.pointer_type(),
                                );
                            }
                            abi::LoadFunctionArg::PrimitiveArg(idx) => {
                                arg_values.push(builder.use_var(to_var(args[idx])));
                            }
                            abi::LoadFunctionArg::StructArg { idx, offset, typed } => {
                                let pointer = builder.use_var(to_var(args[idx]));
                                let tmp =
                                    builder
                                        .ins()
                                        .load(typed, MemFlags::trusted(), pointer, offset);
                                arg_values.push(tmp);
                            }
                        },
                    );
                }
                dbg_println!("call {:#?}", rettype);

                let callee = if let Some(mapping) = self.extern_mapping.get(name) {
                    if let Some(FuncOrDataId::Func(func_id)) = self.module.get_name(&mapping) {
                        Ok(func_id)
                    } else {
                        self.module
                            .declare_function(&mapping, Linkage::Import, &sig)
                    }
                } else {
                    self.module
                        .declare_function(&name.to_string(), Linkage::Import, &sig)
                }
                .unwrap();
                let local_callee = self.module.declare_func_in_func(callee, &mut builder.func);

                let call = builder.ins().call(local_callee, &arg_values);
                if let Some(tmp) = builder.inst_results(call).first().cloned() {
                    builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                }
            }
            isa::InsType::Exit => {
                builder.ins().return_(&[]);
            }
            isa::InsType::Return(x) => {
                let rettype = context.variable(*x);
                if *rettype == isa::Type::Nil {
                    builder.ins().return_(&[]);
                } else if let Some(struct_data) =
                    self.get_struct_data(&ins_context.program, rettype)
                {
                    let return_value = ins_context.struct_return.unwrap();
                    let arg = builder.use_var(to_var(*x));
                    builder.emit_small_memory_copy(
                        self.frontend_config(),
                        return_value,
                        arg,
                        struct_data.size_of() as u64,
                        0,
                        0,
                        true,
                    );
                    builder.ins().return_(&[return_value]);
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
                builder.ins().brz(var, ins_context.bblocks[*iffalse], &[]);
                builder.ins().jump(ins_context.bblocks[*iftrue], &[]);
            }
            isa::InsType::Jmp(x) => {
                builder.ins().jump(ins_context.bblocks[*x], &[]);
            }
            isa::InsType::Lt((x, y))
            | isa::InsType::Gt((x, y))
            | isa::InsType::Lte((x, y))
            | isa::InsType::Gte((x, y))
            | isa::InsType::Equ((x, y))
            | isa::InsType::Neq((x, y)) => {
                let left = builder.use_var(to_var(*x));
                let right = builder.use_var(to_var(*y));
                match context.variable(*x) {
                    maybe_int if maybe_int.is_int() => {
                        let tmp = builder.ins().icmp(
                            match &ins.typed {
                                isa::InsType::Lt(_) => IntCC::SignedLessThan,
                                isa::InsType::Gt(_) => IntCC::SignedGreaterThan,
                                isa::InsType::Lte(_) => IntCC::SignedLessThanOrEqual,
                                isa::InsType::Gte(_) => IntCC::SignedGreaterThanOrEqual,
                                isa::InsType::Equ(_) => IntCC::Equal,
                                isa::InsType::Neq(_) => IntCC::NotEqual,
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
                                isa::InsType::Neq(_) => FloatCC::NotEqual,
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
            isa::InsType::Add((x, y)) => match context.variable(*x) {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, iadd),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fadd),
                _ => unimplemented!(),
            },
            isa::InsType::Sub((x, y)) => match context.variable(*x) {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, isub),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fsub),
                _ => unimplemented!(),
            },
            isa::InsType::Mul((x, y)) => match context.variable(*x) {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, imul),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fmul),
                _ => unimplemented!(),
            },
            isa::InsType::Div((x, y)) => match context.variable(*x) {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, sdiv),
                isa::Type::F64 => generate_arithmetic!(builder, ins, x, y, fdiv),
                _ => unimplemented!(),
            },
            isa::InsType::Mod((x, y)) => match context.variable(*x) {
                maybe_int if maybe_int.is_int() => generate_arithmetic!(builder, ins, x, y, srem),
                _ => unimplemented!(),
            },
            isa::InsType::OpConst {
                register,
                constant,
                op,
                reg_left,
            } => {
                if *reg_left && constant.as_int().is_some() {
                    let var = builder.use_var(to_var(*register));
                    let int = constant.as_int().unwrap();
                    let tmp = match op {
                        isa::OpConst::Add => builder.ins().iadd_imm(var, int),
                        isa::OpConst::Sub => builder.ins().iadd_imm(var, -int),
                        isa::OpConst::Mul => builder.ins().imul_imm(var, int),
                        isa::OpConst::Div => builder.ins().sdiv_imm(var, int),
                        isa::OpConst::Mod => builder.ins().srem_imm(var, int),
                        _ => builder.ins().icmp_imm(
                            match op {
                                isa::OpConst::Lt => IntCC::SignedLessThan,
                                isa::OpConst::Gt => IntCC::SignedGreaterThan,
                                isa::OpConst::Lte => IntCC::SignedLessThanOrEqual,
                                isa::OpConst::Gte => IntCC::SignedGreaterThanOrEqual,
                                isa::OpConst::Equ => IntCC::Equal,
                                isa::OpConst::Neq => IntCC::NotEqual,
                                _ => unreachable!(),
                            },
                            var,
                            int,
                        ),
                    };
                    builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                } else {
                    let (left, right) = match *reg_left {
                        true => (
                            builder.use_var(to_var(*register)),
                            const_to_value(builder, constant),
                        ),
                        false => (
                            const_to_value(builder, constant),
                            builder.use_var(to_var(*register)),
                        ),
                    };
                    let typed = const_to_type(constant);
                    match typed {
                        maybe_int if maybe_int.is_int() => match op {
                            isa::OpConst::Add => builder.ins().iadd(left, right),
                            isa::OpConst::Sub => builder.ins().isub(left, right),
                            isa::OpConst::Mul => builder.ins().imul(left, right),
                            isa::OpConst::Div => builder.ins().sdiv(left, right),
                            isa::OpConst::Mod => builder.ins().srem(left, right),
                            _ => builder.ins().icmp(
                                match op {
                                    isa::OpConst::Lt => IntCC::SignedLessThan,
                                    isa::OpConst::Gt => IntCC::SignedGreaterThan,
                                    isa::OpConst::Lte => IntCC::SignedLessThanOrEqual,
                                    isa::OpConst::Gte => IntCC::SignedGreaterThanOrEqual,
                                    isa::OpConst::Equ => IntCC::Equal,
                                    isa::OpConst::Neq => IntCC::NotEqual,
                                    _ => unreachable!(),
                                },
                                left,
                                right,
                            ),
                        },
                        types::F64 => match op {
                            isa::OpConst::Add => builder.ins().fadd(left, right),
                            isa::OpConst::Sub => builder.ins().fsub(left, right),
                            isa::OpConst::Mul => builder.ins().fmul(left, right),
                            isa::OpConst::Div => builder.ins().fdiv(left, right),
                            _ => builder.ins().fcmp(
                                match op {
                                    isa::OpConst::Lt => FloatCC::LessThan,
                                    isa::OpConst::Gt => FloatCC::GreaterThan,
                                    isa::OpConst::Lte => FloatCC::LessThanOrEqual,
                                    isa::OpConst::Gte => FloatCC::GreaterThanOrEqual,
                                    isa::OpConst::Equ => FloatCC::Equal,
                                    isa::OpConst::Neq => FloatCC::NotEqual,
                                    _ => unreachable!(),
                                },
                                left,
                                right,
                            ),
                        },
                        _ => unimplemented!(),
                    };
                }
            }
            isa::InsType::MemberReference { left, indices, .. } => {
                let retvar = ins.retvar().unwrap();
                let (member_ptr, typed) =
                    self.visit_member_ref(*left, indices, builder, ins_context);
                if let Some(prim) = self.ir_to_cranelift_type(&typed) {
                    let tmp = builder.ins().load(prim, MemFlags::trusted(), member_ptr, 0);
                    builder.def_var(to_var(retvar), tmp);
                } else {
                    let struct_data = self.get_struct_data(&ins_context.program, &typed).unwrap();
                    let slot = builder.create_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        struct_data.size_of(),
                    ));
                    let pointer = builder.ins().stack_addr(self.pointer_type(), slot, 0);
                    builder.emit_small_memory_copy(
                        self.frontend_config(),
                        pointer,
                        member_ptr,
                        struct_data.size_of() as u64,
                        0,
                        0,
                        true,
                    );
                    builder.declare_var(to_var(retvar), self.pointer_type());
                    builder.def_var(to_var(retvar), pointer);
                }
            }
            isa::InsType::MemberReferenceStore { indices, right, .. } => {
                let left = ins.memref_store_left().unwrap();
                let right_var = builder.use_var(to_var(*right));
                let (member_ptr, typed) =
                    self.visit_member_ref(*left, indices, builder, ins_context);
                if let Some(struct_data) = self.get_struct_data(&ins_context.program, &typed) {
                    builder.emit_small_memory_copy(
                        self.frontend_config(),
                        member_ptr,
                        right_var,
                        struct_data.size_of() as u64,
                        0,
                        0,
                        true,
                    );
                } else {
                    builder
                        .ins()
                        .store(MemFlags::trusted(), right_var, member_ptr, 0);
                }
            }
            isa::InsType::Cast { var, typed } => {
                match (context.variable(*var), typed) {
                    (left, right) if left.is_int_repr() && right.is_int_repr() => {
                        let left_typed = self.ir_to_cranelift_type(left).unwrap();
                        let right_typed = self.ir_to_cranelift_type(right).unwrap();
                        let (left_size, right_size) = (left_typed.bits(), right_typed.bits());
                        let var = builder.use_var(to_var(*var));
                        let tmp = if left_size < right_size {
                            builder
                                .ins()
                                .sextend(self.ir_to_cranelift_type(&right).unwrap(), var)
                        } else if left_size > right_size {
                            builder
                                .ins()
                                .ireduce(self.ir_to_cranelift_type(&right).unwrap(), var)
                        } else {
                            var
                        };
                        builder.def_var(to_var(ins.retvar().unwrap()), tmp);
                    }
                    (left, right) if left.is_union() => {
                        let retvar = ins.retvar().unwrap();
                        let left_var = builder.use_var(to_var(*var));
                        let union_type = left.as_union().unwrap();
                        let struct_data =
                            self.build_struct_data_for_union(&ins_context.program, &union_type);
                        if let Some(idx) = union_type.index(&right) {
                            // FIXME: check discriminant type
                            let field = &struct_data.fields()[idx + 1];
                            if let Some(prim) = self.ir_to_cranelift_type(&right) {
                                let tmp = builder.ins().load(
                                    prim,
                                    MemFlags::trusted(),
                                    left_var,
                                    field.offset as i32,
                                );
                                builder.def_var(to_var(retvar), tmp);
                            } else {
                                unimplemented!()
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unimplemented!("{}", ins.print()),
        }
    }
}

#[derive(Clone)]
pub struct CraneliftBackend {}

impl CraneliftBackend {
    fn generate_isa(settings: &Settings) -> Box<dyn TargetIsa> {
        let mut flag_builder = settings::builder();
        match settings.opt_level {
            OptLevel::None => flag_builder.set("opt_level", "none"),
            OptLevel::Speed => flag_builder.set("opt_level", "speed"),
            OptLevel::SpeedAndSize => flag_builder.set("opt_level", "speed_and_size"),
        }
        .unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder.finish(settings::Flags::new(flag_builder));
        isa
    }

    pub fn backend() -> backend::Backend {
        backend::Backend::with_jit_and_object(Self {})
    }
}

impl backend::JitBackend for CraneliftBackend {
    unsafe fn run(
        &self,
        program: &isa::Program,
        _settings: &Settings,
        runtime: &Runtime,
    ) -> Result<(), compiler::Error> {
        let mut builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        for (name, context) in &program.contexts {
            if let isa::IntrinsicType::Extern(symbol) = &context.intrinsic {
                builder.symbol(
                    name.to_string(),
                    runtime.funcs().get(symbol).unwrap().ptr() as _,
                );
            }
        }
        let mut module: Module<SimpleJITBackend> =
            Codegen::from_builder(builder).process(program, false);
        if let Some(main) = module.get_name("main()") {
            if let FuncOrDataId::Func(func_id) = main {
                let function = module.get_finalized_function(func_id);
                let main_fn = std::mem::transmute::<_, extern "C" fn()>(function);
                main_fn();
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
        Ok(())
    }
}

impl backend::ObjectBackend for CraneliftBackend {
    fn generate_object(
        &self,
        program: &isa::Program,
        settings: &Settings,
    ) -> Result<Vec<u8>, compiler::Error> {
        let builder = ObjectBuilder::new(
            CraneliftBackend::generate_isa(settings),
            "main.o",
            cranelift_module::default_libcall_names(),
        );
        let module: Module<ObjectBackend> = Codegen::from_builder(builder).process(program, true);
        Ok(module.finish().emit().unwrap())
    }
}
