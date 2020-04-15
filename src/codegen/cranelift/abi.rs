use crate::codegen::cranelift::translator::*;
use crate::codegen::structs::*;
use crate::ssa::isa;
use cranelift_codegen::ir::immediates::Offset32;
use cranelift_codegen::ir::{types, AbiParam, ArgumentPurpose, Signature};
use cranelift_codegen::isa::TargetIsa;
use target_lexicon::{Architecture, CallingConvention, Triple};

pub enum LoadFunctionArg<'a> {
    Return(&'a dyn AggregateData),
    PrimitiveArg(usize),
    StructArg {
        idx: usize,
        offset: Offset32,
        typed: types::Type,
    },
}
pub trait LoadFunction<'a> = FnMut(LoadFunctionArg<'a>);

fn generate_function_signature_x86_64_sysv<'a, F>(
    program: &'a isa::Program,
    arg_types: &Vec<isa::Type>,
    return_type: &'a isa::Type,
    sig: &mut Signature,
    mut load_function: F,
) where
    F: LoadFunction<'a>,
{
    match *return_type {
        isa::Type::NoReturn => (),
        isa::Type::Nil => (),
        _ => {
            if let Some(aggregate_data) = return_type.as_aggregate_data(&program.builtins) {
                load_function(LoadFunctionArg::Return(aggregate_data));
                sig.params
                    .push(AbiParam::special(types::I64, ArgumentPurpose::StructReturn));
                sig.returns
                    .push(AbiParam::special(types::I64, ArgumentPurpose::StructReturn));
            } else {
                sig.returns
                    .push(AbiParam::new(ir_to_cranelift_type(&return_type).unwrap()));
            }
        }
    }
    for (idx, arg) in arg_types.iter().enumerate() {
        if let Some(cranelift_type) = ir_to_cranelift_type(&arg) {
            load_function(LoadFunctionArg::PrimitiveArg(idx));
            sig.params.push(AbiParam::new(cranelift_type));
        } else {
            let aggregate_data: &dyn AggregateData = arg.as_aggregate_data(&program.builtins).unwrap();
            if aggregate_data.size_of() <= 8 {
                // For small structs, we pass the struct as a 64-bit integer parameter
                load_function(LoadFunctionArg::StructArg {
                    idx: 0,
                    offset: Offset32::new(0),
                    typed: types::I64,
                });
                sig.params.push(AbiParam::new(types::I64));
            } else if aggregate_data.size_of() > 16 {
                // For large structs, push a duplicate of it into the stack
                unimplemented!()
            } else {
                // For medium-sized structs, manually classify each eight bytes in the struct
                #[derive(Debug, Clone, Copy, PartialEq)]
                enum AbiClass {
                    None,
                    Memory,
                    Integer,
                    SSE,
                }
                fn classify_type(typed: &PrimitiveType) -> AbiClass {
                    match typed {
                        PrimitiveType::I8
                        | PrimitiveType::I16
                        | PrimitiveType::I32
                        | PrimitiveType::I64 => AbiClass::Integer,
                        PrimitiveType::F64 => AbiClass::SSE,
                    }
                }
                let mut eight_bytes = vec![AbiClass::None; (aggregate_data.size_of() + 7) / 8];
                for prim_field in aggregate_data.flattened_fields() {
                    for array_idx in 0..prim_field.multiplier {
                        let idx = (prim_field.offset + array_idx * prim_field.typed.bytes()) / 8;
                        match (classify_type(&prim_field.typed), eight_bytes[idx]) {
                            // If both classes are equal, this is the resulting class.
                            (x, y) if x == y => (),
                            // If one of the classes is NO_CLASS, the resulting class is the otherclass.
                            (other, AbiClass::None) => {
                                eight_bytes[idx] = other;
                            }
                            // If one of the classes is MEMORY, the result is the MEMORY.
                            (AbiClass::Memory, _) | (_, AbiClass::Memory) => {
                                eight_bytes[idx] = AbiClass::Memory;
                            }
                            // If one of the classes is INTEGER, the result is the INTEGER.
                            (AbiClass::Integer, _) | (_, AbiClass::Integer) => {
                                eight_bytes[idx] = AbiClass::Integer;
                            }
                            // Otherwise class SSE is used.
                            (_, _) => {
                                eight_bytes[idx] = AbiClass::SSE;
                            }
                        }
                    }
                }
                for (struct_idx, class) in eight_bytes.iter().enumerate() {
                    match class {
                        AbiClass::Integer => {
                            load_function(LoadFunctionArg::StructArg {
                                idx,
                                offset: Offset32::new(struct_idx as i32 * 8),
                                typed: types::I64,
                            });
                            sig.params.push(AbiParam::new(types::I64));
                        }
                        AbiClass::SSE => {
                            load_function(LoadFunctionArg::StructArg {
                                idx,
                                offset: Offset32::new(struct_idx as i32 * 8),
                                typed: types::F64,
                            });
                            sig.params.push(AbiParam::new(types::F64));
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}

pub fn generate_function_signature<'a, F>(
    target_isa: &dyn TargetIsa,
    program: &'a isa::Program,
    arg_types: &Vec<isa::Type>,
    return_type: &'a isa::Type,
    sig: &mut Signature,
    load_function: F,
) where
    F: LoadFunction<'a>,
{
    // Cranelift currently doesn't have any abstractions for structured data,
    // So we'll have to hand roll our own D:
    match target_isa.triple() {
        triple
        @ Triple {
            architecture: Architecture::X86_64,
            ..
        } if triple.default_calling_convention().unwrap() == CallingConvention::SystemV => {
            generate_function_signature_x86_64_sysv(
                program,
                arg_types,
                return_type,
                sig,
                load_function,
            );
        }
        triple => unimplemented!("{:?}", triple),
    }
}
