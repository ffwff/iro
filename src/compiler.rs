use crate::ssa::opt;
use crate::ssa::isa::Context;
use crate::utils::Pipeline;

pub fn ssa_pipeline() -> Pipeline<Context, fn(&mut Context)> {
    Pipeline::new(
        [
            opt::build_graph_and_rename_vars,
            opt::fold_constants,
            opt::remove_defined_never_used,
            opt::eliminate_phi,
            opt::data_flow_analysis,
        ]
        .to_vec(),
    )
}