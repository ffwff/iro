use crate::compiler::Flow;
use crate::ssa::isa::*;

pub fn cleanup_high_level_instructions(context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        let old_len = block.ins.len();
        let old_ins = std::mem::replace(&mut block.ins, Vec::with_capacity(old_len));

        for ins in old_ins {
            match ins.typed {
                InsType::Drop(x) => {
                    match &context.variables[usize::from(x)] {
                        Type::Pointer(data) => {
                            if data.tag == BorrowModifier::Unique {
                                block.ins.push(Ins::empty_ret(
                                    InsType::DeallocHeap(x),
                                    ins.source_location(),
                                ));
                            }
                        }
                        Type::Struct(data) => {
                            // TODO
                        }
                        Type::Slice(data) => {
                            // TODO
                        }
                        Type::Union(data) => {
                            // TODO
                        }
                        _ => (),
                    }
                }
                InsType::MarkMoved(_) => (),
                _ => block.ins.push(ins),
            }
        }
    }
    Flow::Continue
}
