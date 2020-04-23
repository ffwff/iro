use crate::ssa::isa::*;
use crate::utils::pipeline::Flow;

pub fn separate_postlude(context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        if let Some(ins) = block.ins.last().clone() {
            if ins.typed.is_jmp() {
                block.postlude = ins.clone();
                block.ins.pop();
            } else {
                unreachable!()
            }
        }
    }
    Flow::Continue
}

pub fn fuse_postlude(context: &mut Context) -> Flow {
    for block in &mut context.blocks {
        let postlude = std::mem::replace(&mut block.postlude, Ins::new(0, InsType::Nop));
        block.ins.push(postlude);
    }
    Flow::Continue
}
