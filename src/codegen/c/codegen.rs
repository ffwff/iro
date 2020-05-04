use crate::codegen::backend;
use crate::codegen::c::mangler;
use crate::codegen::settings::*;
use crate::codegen::structs::*;
use crate::compiler;
use crate::ssa::isa;
use crate::runtime;
use fnv::FnvHashMap;
use std::fmt::Write;
use std::rc::Rc;

struct MemberRefData {
    pub accessor: String,
    pub bounds_checks: Vec<String>,
}

struct InsContext<'a> {
    context: &'a isa::Context,
}

struct Codegen<'a> {
    program: &'a isa::Program,
    mangled_cache: FnvHashMap<Rc<isa::FunctionName>, String>,
    function_decls: Vec<String>,
    function_bodies: Vec<String>,
    typedefs: FnvHashMap<isa::Type, String>,
}

impl<'a> Codegen<'a> {
    fn new(program: &'a isa::Program) -> Self {
        Self {
            program,
            mangled_cache: fnv_hashmap![],
            function_decls: vec![],
            function_bodies: vec![],
            typedefs: fnv_hashmap![],
        }
    }

    fn get_struct_data(&self, typed: &isa::Type) -> Option<Rc<StructData>> {
        match typed {
            isa::Type::Struct(data) => Some(self.build_struct_data_for_struct(&data)),
            isa::Type::Slice(data) => Some(self.build_struct_data_for_slice(&data)),
            isa::Type::Union(data) => Some(self.build_struct_data_for_union(&data)),
            maybe_ptr if maybe_ptr.is_fat_pointer() => self
                .program
                .builtins
                .generic_fat_pointer_struct
                .data
                .clone()
                .into_inner(),
            _ => None,
        }
    }

    fn build_struct_data_for_struct(&self, typed: &isa::StructType) -> Rc<StructData> {
        if let Some(data) = typed.data.borrow() {
            return data.clone();
        }
        let mut data = StructData::new();
        for field in typed.fields() {
            data.append_typed(Some(field.name.clone()), StructFieldType::Void);
        }
        let rc = Rc::new(data);
        typed.data.replace(rc.clone());
        rc
    }

    fn build_struct_data_for_slice(&self, typed: &isa::SliceType) -> Rc<StructData> {
        if let Some(data) = typed.data.borrow() {
            return data.clone();
        }
        let mut data = StructData::new();
        data.append_array(None, StructFieldType::Void, typed.len.unwrap());
        let rc = Rc::new(data);
        typed.data.replace(rc.clone());
        rc
    }

    fn build_struct_data_for_union(&self, typed: &isa::UnionType) -> Rc<StructData> {
        unimplemented!()
    }

    fn process(&mut self) -> String {
        self.build_struct_data_for_struct(&self.program.builtins.generic_fat_pointer_struct);
        for astruct in self.program.builtins.structs.values() {
            self.build_struct_data_for_struct(astruct);
        }

        let mut to_generate = vec![];
        for (func_name, context) in &self.program.contexts {
            if let isa::IntrinsicType::Extern(named) = &context.intrinsic {
                let mut s = String::new();
                write!(
                    s,
                    "extern {} {}({})",
                    self.c_type(&context.rettype),
                    named,
                    self.c_args(&context.args),
                )
                .unwrap();
                self.function_decls.push(s);
                self.mangled_cache
                    .insert(func_name.clone(), named.to_string());
            } else {
                to_generate.push((func_name, context));
            }
        }
        for (func_name, context) in to_generate {
            self.visit_context(func_name, context)
                .expect("able to visit_context");
        }
        self.generate_main().expect("able to generate_main");

        let mut source = self.generate_prelude().unwrap();
        for decl in self.typedefs.values() {
            writeln!(source, "typedef {};", decl).unwrap();
        }
        for decl in &self.function_decls {
            source.push_str(decl);
            source.push_str(";\n");
        }
        for body in &self.function_bodies {
            source.push_str(body);
        }
        source
    }

    fn generate_prelude(&mut self) -> Result<String, std::fmt::Error> {
        let mut prelude = String::new();
        writeln!(
            prelude,
            "\
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdalign.h>
#include <assert.h>"
        )?;
        Ok(prelude)
    }

    fn generate_main(&mut self) -> Result<(), std::fmt::Error> {
        let mut source = String::new();
        writeln!(source, "int main(int argc, char **argv) {{")?;
        writeln!(source, "\t{}();", self.mangle(&self.program.entry))?;
        writeln!(source, "\treturn 0;")?;
        writeln!(source, "}}")?;
        self.function_bodies.push(source);
        Ok(())
    }

    fn mangle(&mut self, unmangled: &Rc<isa::FunctionName>) -> &str {
        self.mangled_cache
            .entry(unmangled.clone())
            .or_insert_with(|| mangler::mangle(unmangled))
    }

    fn c_type(&mut self, typed: &isa::Type) -> String {
        match typed {
            isa::Type::Bool => "bool".to_string(),
            isa::Type::I8 => "int8_t".to_string(),
            isa::Type::I16 => "int16_t".to_string(),
            isa::Type::I32 => "int32_t".to_string(),
            isa::Type::I64 => "int64_t".to_string(),
            isa::Type::ISize => "intptr_t".to_string(),
            isa::Type::F64 => "double".to_string(),
            isa::Type::Pointer(x) => {
                if typed.is_fat_pointer() {
                    let mut s = String::new();
                    mangler::mangle_type(typed, &mut s);
                    if let Some(slice) = x.typed.as_slice() {
                        assert!(slice.is_dyn());
                        let inner = self.c_type(&slice.typed);
                        self.typedefs.insert(typed.clone(), {
                            let mut typedef_source = String::new();
                            write!(
                                &mut typedef_source,
                                "\
struct {{
\t{}* data;
\tintptr_t len;
}} {}",
                                inner, s
                            )
                            .unwrap();
                            typedef_source
                        });
                    } else {
                        unreachable!()
                    }
                    s
                } else {
                    let mut s = String::new();
                    write!(&mut s, "{}*", self.c_type(&x.typed)).unwrap();
                    s
                }
            }
            isa::Type::Struct(x) => {
                if !self.typedefs.contains_key(typed) {
                    let typedef_source = {
                        let mut typedef_source = String::new();
                        writeln!(typedef_source, "struct {{").unwrap();
                        let mut fields = x.vars().iter().collect::<Vec<_>>();
                        fields.sort_by_key(|(_, field)| field.idx);
                        for (key, field) in fields {
                            writeln!(typedef_source, "\t{} {};", self.c_type(&field.typed), key)
                                .unwrap();
                        }
                        write!(&mut typedef_source, "}} {}", x.name()).unwrap();
                        typedef_source
                    };
                    self.typedefs.insert(typed.clone(), typedef_source);
                }
                x.name().to_string()
            }
            isa::Type::Slice(x) => {
                let mut s = String::new();
                mangler::mangle_type(typed, &mut s);
                if !self.typedefs.contains_key(typed) {
                    if let Some(len) = x.len {
                        let inner = self.c_type(&x.typed);
                        self.typedefs.insert(typed.clone(), {
                            let mut typedef_source = String::new();
                            write!(&mut typedef_source, "{} {}[{}]", inner, s, len).unwrap();
                            typedef_source
                        });
                    } else {
                        unreachable!()
                    }
                }
                s
            }
            isa::Type::Union(x) => {
                unimplemented!();
            }
            _ => "void".to_string(),
        }
    }

    fn c_args(&mut self, args: &[isa::Type]) -> String {
        args.iter()
            .enumerate()
            .map(|(idx, typed)| {
                let mut s = String::new();
                write!(s, "{} arg{}", self.c_type(&typed), idx).unwrap();
                s
            })
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn visit_member_ref(
        &mut self,
        left: isa::Variable,
        indices: &[isa::MemberExprIndex],
        ins_context: &InsContext,
    ) -> Result<MemberRefData, std::fmt::Error> {
        let mut last_typed = ins_context.context.variable(left);
        let mut f = String::new();
        let mut bounds_checks = vec![];
        match ins_context.context.variable(left) {
            maybe_ptr if maybe_ptr.is_fat_pointer() => write!(f, "(&v{})", left)?,
            isa::Type::Struct(_) => write!(f, "(&v{})", left)?,
            isa::Type::Pointer(_) | isa::Type::Slice(_) => write!(f, "v{}", left)?,
            _ => unreachable!(),
        }
        for index in indices.iter() {
            match &index.var {
                isa::MemberExprIndexVar::StructIndex(idx) => {
                    let struct_data = self.get_struct_data(&last_typed).unwrap();
                    let field = &struct_data.fields()[*idx];
                    if let Some(name) = &struct_data.names()[*idx] {
                        write!(f, "->{}", name)?;
                    } else {
                        write!(f, "->b{}", idx)?;
                    }
                }
                isa::MemberExprIndexVar::Variable(var) => match last_typed {
                    maybe_ptr if maybe_ptr.is_fat_pointer() => {
                        let mut bounds_check_stmt = String::new();
                        write!(bounds_check_stmt, "assert(v{} < {}->len);", var, f)?;
                        bounds_checks.push(bounds_check_stmt);
                        write!(f, "->data[v{}]", var)?;
                    }
                    _ => {
                        write!(f, "[v{}]", var)?;
                    }
                },
            }
            last_typed = &index.typed;
        }
        Ok(MemberRefData {
            accessor: f,
            bounds_checks,
        })
    }

    fn visit_context(
        &mut self,
        func_name: &Rc<isa::FunctionName>,
        context: &isa::Context,
    ) -> Result<(), std::fmt::Error> {
        dbg_println!("codegen: {}", context.print());

        let mut function_decl = String::new();
        let args = self.c_args(&context.args);
        write!(
            function_decl,
            "static {} {}({})",
            self.c_type(&context.rettype),
            self.mangle(func_name),
            args,
        )?;

        let mut function_body = String::new();
        writeln!(function_body, "{} {{", function_decl)?;
        for (idx, typed) in context.variables.iter().enumerate() {
            if !typed.is_empty() {
                writeln!(function_body, "\t{} v{};", self.c_type(typed), idx)?;
            }
        }

        let ins_context = InsContext { context };
        for (idx, block) in context.blocks.iter().enumerate() {
            writeln!(function_body, "b{}:;", idx)?;
            for ins in &block.ins {
                self.visit_ins(ins, &ins_context, &mut function_body)?;
            }
        }
        writeln!(function_body, "}}")?;

        self.function_decls.push(function_decl);
        self.function_bodies.push(function_body);

        Ok(())
    }

    fn visit_ins(
        &mut self,
        ins: &isa::Ins,
        ins_context: &InsContext,
        f: &mut String,
    ) -> Result<(), std::fmt::Error> {
        dbg_println!("processing {}", ins.print());
        let context = ins_context.context;
        match &ins.typed {
            isa::InsType::DeallocHeap(arg) => {
                writeln!(f, "\t{}(v{});", runtime::DEALLOC_NAME_MANGLED, arg)?;
            }
            isa::InsType::Copy(arg) | isa::InsType::Move(arg) => {
                writeln!(f, "\tv{} = v{};", ins.retvar().unwrap(), arg)?;
            }
            isa::InsType::AllocHeap => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = (void*)({}(sizeof(*v{}), alignof(*v{})));", retvar, runtime::MALLOC_NAME_MANGLED, retvar, retvar)?;
            }
            isa::InsType::Alloca | isa::InsType::LoadStruct => (),
            isa::InsType::Load(x) => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = *v{};", retvar, x)?;
            }
            isa::InsType::Store { source, dest } => {
                writeln!(f, "\t*v{} = v{};", dest, source)?;
            }
            isa::InsType::Borrow { var, .. } => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = &v{};", retvar, var)?;
            }
            isa::InsType::LoadArg(arg) => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = arg{};", retvar, arg)?;
            }
            isa::InsType::LoadI32(x) => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = {};", retvar, x)?;
            }
            isa::InsType::LoadI64(x) => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = {}LL;", retvar, x)?;
            }
            isa::InsType::LoadBool(x) => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = {};", retvar, if *x { "true" } else { "false" })?;
            }
            isa::InsType::LoadSubstring(x) => {
                let retvar = ins.retvar().unwrap();
                writeln!(
                    f,
                    "\tv{}.data = \"{}\";",
                    retvar,
                    x.escape_debug().to_string()
                )?;
                writeln!(f, "\tv{}.len = {};", retvar, x.len())?;
            }
            isa::InsType::LoadSlice(x) => {
                let retvar = ins.retvar().unwrap();
                for (idx, var) in x.iter().enumerate() {
                    writeln!(f, "\tv{}[{}] = v{};", retvar, idx, var)?;
                }
            }
            isa::InsType::LoadF64(x) => {
                let retvar = ins.retvar().unwrap();
                writeln!(f, "\tv{} = {};", retvar, x)?;
            }
            isa::InsType::LoadNil => {
                let rettype = context.variable(ins.retvar().unwrap());
                if *rettype == isa::Type::Nil {
                    // purposely left blank
                } else {
                    unimplemented!()
                }
            }
            isa::InsType::Call { name, args } => {
                // dbg_println!("{:?}", self.mangled_cache);
                let sig = self.mangle(&context.call_names[*name as usize]);
                let retvar = ins.retvar().unwrap();
                let call_args = args
                    .iter()
                    .map(|var| "v".to_string() + &var.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                if context.variable(retvar).is_empty() {
                    writeln!(f, "\t{}({});", sig, call_args)?;
                } else {
                    writeln!(f, "\tv{} = {}({});", retvar, sig, call_args)?;
                }
            }
            isa::InsType::Exit => {
                writeln!(f, "\treturn;")?;
            }
            isa::InsType::Return(x) => {
                if context.variable(*x).is_empty() {
                    writeln!(f, "\treturn;")?;
                } else {
                    writeln!(f, "\treturn v{};", x)?;
                }
            }
            isa::InsType::IfJmp {
                condvar,
                iftrue,
                iffalse,
            } => {
                writeln!(
                    f,
                    "\tif(v{}) goto b{}; else goto b{};",
                    condvar, iftrue, iffalse
                )?;
            }
            isa::InsType::Jmp(x) => {
                writeln!(f, "\tgoto b{};", x)?;
            }
            isa::InsType::Lt((x, y)) => {
                writeln!(f, "\tv{} = v{}<v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Gt((x, y)) => {
                writeln!(f, "\tv{} = v{}>v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Lte((x, y)) => {
                writeln!(f, "\tv{} = v{}<=v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Gte((x, y)) => {
                writeln!(f, "\tv{} = v{}>=v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Equ((x, y)) => {
                writeln!(f, "\tv{} = v{}==v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Neq((x, y)) => {
                writeln!(f, "\tv{} = v{}!=v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Add((x, y)) => {
                writeln!(f, "\tv{} = v{}+v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Sub((x, y)) => {
                writeln!(f, "\tv{} = v{}-v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Mul((x, y)) => {
                writeln!(f, "\tv{} = v{}*v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Div((x, y)) => {
                writeln!(f, "\tv{} = v{}/v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::Mod((x, y)) => {
                writeln!(f, "\tv{} = v{}%v{};", ins.retvar().unwrap(), x, y)?
            }
            isa::InsType::OpConst {
                register,
                constant,
                op,
                reg_left,
            } => {
                let op_str = match op {
                    isa::OpConst::Add => "+",
                    isa::OpConst::Sub => "-",
                    isa::OpConst::Mul => "*",
                    isa::OpConst::Div => "/",
                    isa::OpConst::Mod => "%",
                    isa::OpConst::Lt => "<",
                    isa::OpConst::Gt => ">",
                    isa::OpConst::Lte => "<=",
                    isa::OpConst::Gte => ">=",
                    isa::OpConst::Equ => "==",
                    isa::OpConst::Neq => "!=",
                };
                fn write_constant(
                    f: &mut String,
                    constant: &isa::Constant,
                ) -> Result<(), std::fmt::Error> {
                    match constant {
                        isa::Constant::Bool(x) => f.write_str(if *x { "true" } else { "false" }),
                        isa::Constant::I32(n) => write!(f, "{}", *n),
                        isa::Constant::I64(n) => write!(f, "{}LL", *n),
                        isa::Constant::F64(n) => write!(f, "{}", *n),
                    }
                }
                if *reg_left {
                    write!(f, "\tv{} = v{}{}", ins.retvar().unwrap(), register, op_str)?;
                    write_constant(f, constant)?;
                    writeln!(f, ";")?;
                } else {
                    write!(f, "\tv{} = ", ins.retvar().unwrap())?;
                    write_constant(f, constant)?;
                    writeln!(f, "{}v{};", op_str, register)?;
                }
            }
            isa::InsType::MemberReference { left, indices, .. } => {
                let data = self.visit_member_ref(*left, indices, ins_context)?;
                for stmt in &data.bounds_checks {
                    writeln!(f, "\t{}", stmt)?;
                }
                writeln!(f, "\tv{} = {};", ins.retvar().unwrap(), data.accessor)?;
            }
            isa::InsType::MemberReferenceStore { right, indices, .. } => {
                let left = ins.memref_store_left().unwrap();
                let data = self.visit_member_ref(*left, indices, ins_context)?;
                for stmt in &data.bounds_checks {
                    writeln!(f, "\t{}", stmt)?;
                }
                writeln!(f, "\t{} = v{};", data.accessor, right)?;
            }
            isa::InsType::Cast { var, typed } => match (context.variable(*var), typed) {
                (left, right) if left.is_int_repr() && right.is_int_repr() => {
                    let left_type = self.c_type(left);
                    writeln!(f, "\tv{} = ({})v{};", ins.retvar().unwrap(), left_type, var)?;
                }
                (left, right) if left.is_union() => {
                    unimplemented!();
                }
                _ => unreachable!(),
            },
            _ => unimplemented!("{}", ins.print()),
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct CBackend {}

impl CBackend {
    pub fn backend() -> backend::Backend {
        backend::Backend::with_object(Self {})
    }
}

impl backend::ObjectBackend for CBackend {
    fn generate_object(
        &self,
        program: &isa::Program,
        settings: &Settings,
    ) -> Result<Vec<u8>, compiler::Error> {
        let source = Codegen::new(program).process();
        dbg_println!("output:\n{}", source);

        use std::process::Command;
        use tempfile::Builder;

        let source_name = Builder::new()
            .rand_bytes(5)
            .suffix(".c")
            .tempfile()
            .map_err(|error| compiler::Error::io_error(error))?;
        std::fs::write(&source_name, source).map_err(|error| compiler::Error::io_error(error))?;

        let source_path = source_name.into_temp_path();
        let object_path = Builder::new()
            .rand_bytes(5)
            .suffix(".o")
            .tempfile()
            .map_err(|error| compiler::Error::io_error(error))?
            .into_temp_path();

        let args = vec![
            "-c".to_string(),
            "-g".to_string(),
            match settings.opt_level {
                OptLevel::None => "-O0",
                OptLevel::Speed => "-O3",
                OptLevel::SpeedAndSize => "-O2",
            }
            .to_string(),
            "-o".to_string(),
            object_path.to_string_lossy().to_string(),
            source_path.to_string_lossy().to_string(),
        ];
        let output = Command::new("gcc").args(&args).output().unwrap();
        if !output.status.success() {
            unimplemented!("\n{}", String::from_utf8_lossy(&output.stderr));
        }
        std::fs::read(object_path).map_err(|error| compiler::Error::io_error(error))
    }
}
