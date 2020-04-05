use crate::ssa::isa::Context;
use crate::ssa::opt;
use crate::utils::pipeline::*;

pub fn ssa_pipeline() -> Pipeline<Context, fn(&mut Context) -> Flow> {
    Pipeline::new(
        [
            opt::insert_jmps,
            opt::build_graph_and_rename_vars,
            opt::fold_constants,
            opt::collect_garbage_vars,
            opt::separate_postlude,
            opt::cleanup_blocks,
            opt::fuse_postlude,
            opt::data_flow_analysis,
            opt::eliminate_phi,
        ]
        .to_vec(),
    )
}
