use crate::codegen::cranelift::codegen::Codegen;
use crate::codegen::structs::*;
use crate::ssa::isa;
use cranelift_codegen::ir::immediates::Offset32;
use cranelift_codegen::ir::{types, AbiParam, ArgumentPurpose, Signature};
use cranelift_module::Backend;
use std::rc::Rc;
use target_lexicon::{Architecture, CallingConvention, Triple};

pub enum LoadFunctionArg {
    Return(Rc<StructData>),
    PrimitiveArg(usize),
    StructArg {
        idx: usize,
        offset: Offset32,
        typed: types::Type,
    },
}
pub trait LoadFunction = FnMut(LoadFunctionArg);

impl<B> Codegen<B>
where
    B: Backend,
{
    fn generate_function_signature_x86_64_sysv<F>(
        &self,
        program: &isa::Program,
        arg_types: &Vec<isa::Type>,
        return_type: &isa::Type,
        sig: &mut Signature,
        mut load_function: F,
    ) where
        F: LoadFunction,
    {
        match *return_type {
            isa::Type::NoReturn => (),
            isa::Type::Nil => (),
            _ => {
                if let Some(struct_data) = self.get_struct_data(&program, return_type) {
                    load_function(LoadFunctionArg::Return(struct_data));
                    sig.params
                        .push(AbiParam::special(types::I64, ArgumentPurpose::StructReturn));
                    sig.returns
                        .push(AbiParam::special(types::I64, ArgumentPurpose::StructReturn));
                } else {
                    sig.returns.push(AbiParam::new(
                        self.ir_to_cranelift_type(&return_type)
                            .expect("must be a primitive type"),
                    ));
                }
            }
        }
        for (idx, arg) in arg_types.iter().enumerate() {
            if let Some(cranelift_type) = self.ir_to_cranelift_type(&arg) {
                load_function(LoadFunctionArg::PrimitiveArg(idx));
                sig.params.push(AbiParam::new(cranelift_type));
            } else {
                let struct_data = self.get_struct_data(&program, arg).unwrap();
                if struct_data.size_of() <= 8 {
                    // FIXME: support SSE vars
                    // For small structs, we pass the struct as a 64-bit integer parameter
                    load_function(LoadFunctionArg::StructArg {
                        idx,
                        offset: Offset32::new(0),
                        typed: types::I64,
                    });
                    sig.params.push(AbiParam::new(types::I64));
                } else if struct_data.size_of() > 16 {
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
                    fn classify_type(typed: &StructFieldType) -> AbiClass {
                        match typed {
                            StructFieldType::I8
                            | StructFieldType::I16
                            | StructFieldType::I32
                            | StructFieldType::I64 => AbiClass::Integer,
                            StructFieldType::F64 => AbiClass::SSE,
                            _ => unreachable!(),
                        }
                    }
                    let mut eight_bytes =
                        vec![AbiClass::None; ((struct_data.size_of() + 7) / 8) as usize];
                    for prim_field in struct_data.flattened_fields().iter() {
                        for array_idx in 0..prim_field.multiplier {
                            let idx = ((prim_field.offset + array_idx * prim_field.typed.size_of())
                                / 8) as usize;
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

    pub(super) fn generate_function_signature<F>(
        &self,
        program: &isa::Program,
        arg_types: &Vec<isa::Type>,
        return_type: &isa::Type,
        sig: &mut Signature,
        load_function: F,
    ) where
        F: LoadFunction,
    {
        // Cranelift currently doesn't have any abstractions for structured data,
        // So we'll have to hand roll our own D:
        let target_isa = self.module.isa();
        match target_isa.triple() {
            triple
            @
            Triple {
                architecture: Architecture::X86_64,
                ..
            } if triple.default_calling_convention().unwrap() == CallingConvention::SystemV => {
                self.generate_function_signature_x86_64_sysv(
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
}
