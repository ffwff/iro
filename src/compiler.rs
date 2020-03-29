use crate::ssa::opt;
use crate::ssa::isa::Context;
use crate::utils::pipeline::*;

pub fn ssa_pipeline() -> Pipeline<Context, fn(&mut Context) -> Flow> {
    Pipeline::new(
        [
            opt::build_graph_and_rename_vars,
            // opt::remove_defined_never_used,
            // opt::fold_constants,
            opt::eliminate_phi,
            opt::data_flow_analysis,
            // opt::remove_no_flow,
        ]
        .to_vec(),
    )
}