use crate::ssa::isa::Context;
use crate::ssa::opt;
use crate::utils::pipeline::*;

pub fn ssa_pipeline() -> Pipeline<Context, fn(&mut Context) -> Flow> {
    Pipeline::new(
        [
            opt::build_graph_and_rename_vars,
            opt::fold_constants,
            opt::collect_garbage_vars,
            opt::data_flow_analysis,
        ]
        .to_vec(),
    )
}
