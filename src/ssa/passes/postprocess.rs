use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::ContextLocalData;

pub fn cleanup_high_level_instructions(_: &mut ContextLocalData, context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        let old_len = block.ins.len();
        let old_ins = std::mem::replace(&mut block.ins, Vec::with_capacity(old_len));

        for ins in old_ins {
            match ins.typed {
                InsType::Drop(x) => {
                    let source_location = ins.source_location();
                    fn insert_destructor(
                        x: Variable,
                        variables: &mut [Type],
                        block: &mut Block,
                        source_location: u32,
                    ) {
                        match &variables[usize::from(x)] {
                            Type::Pointer(data) => {
                                if data.tag == BorrowModifier::Unique {
                                    block.ins.push(Ins::empty_ret(
                                        InsType::DeallocHeap(x),
                                        source_location,
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
                    insert_destructor(x, &mut context.variables, block, source_location);
                }
                InsType::MarkMoved(_) => (),
                _ => block.ins.push(ins),
            }
        }
    }
    Flow::Continue
}
