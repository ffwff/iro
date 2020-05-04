use crate::compiler::Flow;
use crate::ssa::isa::*;
use crate::ssa::passes::ContextLocalData;

pub fn separate_postlude(_: &mut ContextLocalData, context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        if let Some(ins) = block.ins.last() {
            if ins.typed.is_jmp() {
                block.postlude = block.ins.pop();
            } else {
                unreachable!()
            }
        }
    }
    Flow::Continue
}

pub fn fuse_postlude(_: &mut ContextLocalData, context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        block.ins.push(block.postlude.take().unwrap());
    }
    Flow::Continue
}
