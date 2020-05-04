use crate::ssa::isa;
use std::fmt::Write;
use std::rc::Rc;

pub fn mangle_string(source: &str, dest: &mut String) {
    for ch in source.chars() {
        assert!((0x20..0x7e).contains(&(ch as _)));
        dest.push(ch);
    }
}

pub fn mangle_type(typed: &isa::Type, dest: &mut String) {
    match typed {
        isa::Type::Nil => dest.push('N'),
        isa::Type::Bool => dest.push('O'),
        isa::Type::I8 => dest.push('B'),
        isa::Type::I16 => dest.push('H'),
        isa::Type::I32 => dest.push('W'),
        isa::Type::I64 => dest.push('Q'),
        isa::Type::ISize => dest.push('S'),
        isa::Type::F64 => dest.push('D'),
        isa::Type::Pointer(x) => {
            dest.push('P');
            dest.push(match x.tag {
                isa::BorrowModifier::Immutable => 'i',
                isa::BorrowModifier::Mutable => 'm',
                isa::BorrowModifier::Unique => 'u',
            });
            mangle_type(&x.typed, dest);
        }
        isa::Type::Struct(x) => {
            dest.push('S');
            mangle_string(&x.name(), dest);
        }
        isa::Type::Slice(x) => {
            dest.push('Z');
            if let Some(len) = x.len {
                write!(dest, "{}", len).unwrap();
            } else {
                dest.push('0');
            }
            mangle_type(&x.typed, dest);
        }
        isa::Type::Union(x) => {
            dest.push_str("U$");
            for typed in x.types() {
                mangle_type(&typed, dest);
                dest.push('_');
            }
            dest.push('$');
        }
        _ => unreachable!(),
    }
}

pub fn mangle(unmangled: &Rc<isa::FunctionName>) -> String {
    let mut name = "_I".to_string();
    mangle_string(&unmangled.name, &mut name);
    name.push('_');
    for typed in &unmangled.arg_types {
        mangle_type(typed, &mut name);
        name.push('_');
    }
    name
}
