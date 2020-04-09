use crate::codegen::cranelift::translator::*;
use crate::codegen::structs::*;
use crate::ssa::isa;
use cranelift_codegen::ir::immediates::Offset32;
use cranelift_codegen::ir::{types, AbiParam, Signature};
use cranelift_codegen::isa::TargetIsa;
use std::borrow::Borrow;
use target_lexicon::{Architecture, CallingConvention, Triple};

fn generate_function_arguments_x86_64_sysv<F>(
    program: &isa::Program,
    arg_types: &Vec<isa::Type>,
    sig: &mut Signature,
    mut load_function: F,
) where
    F: FnMut(usize, Offset32),
{
    // Cranelift currently doesn't have any abstractions for structured data,
    // So we'll have to hand roll our own D:
    // FIXME: this only generates calls for x86-64's system-v abi
    for (idx, arg) in arg_types.iter().enumerate() {
        if let Some(cranelift_type) = ir_to_cranelift_type(&arg) {
            sig.params.push(AbiParam::new(cranelift_type));
        } else {
            let struct_data: &StructData = match arg {
                isa::Type::Struct(isa::StructType(struct_data_rc)) => struct_data_rc.borrow(),
                maybe_ptr if maybe_ptr.is_fat_pointer() => &program.generic_fat_pointer_struct,
                _ => unreachable!(),
            };
            if struct_data.size_of() <= 8 {
                // For small structs, we pass the struct as a 64-bit integer parameter
                load_function(idx, Offset32::new(0));
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
                        }
                        // Otherwise class SSE is used.
                        (_, AbiClass::None) => {
                            eight_bytes[idx] = AbiClass::SSE;
                        }
                        _ => unreachable!(),
                    }
                }
                for (struct_idx, class) in eight_bytes.iter().enumerate() {
                    match class {
                        AbiClass::Integer => {
                            load_function(idx, Offset32::new(struct_idx as i32 * 8));
                            sig.params.push(AbiParam::new(types::I64));
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}

pub fn generate_function_arguments<F>(
    target_isa: &dyn TargetIsa,
    program: &isa::Program,
    arg_types: &Vec<isa::Type>,
    sig: &mut Signature,
    load_function: F,
) where
    F: FnMut(usize, Offset32),
{
    match target_isa.triple() {
        triple
        @ Triple {
            architecture: Architecture::X86_64,
            ..
        } if triple.default_calling_convention().unwrap() == CallingConvention::SystemV => {
            generate_function_arguments_x86_64_sysv(program, arg_types, sig, load_function);
        }
        triple => unimplemented!("{:?}", triple),
    }
}
