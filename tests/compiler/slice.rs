use iro::codegen::cranelift::Settings;
use iro::runtime::Runtime;
use iro::ssa::isa::{FunctionName, Type};
use iro::ssa::visitor::TopLevelArch;
use iro::utils;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

#[cfg(test)]
#[test]
fn index() {
    extern "C" fn record_i32(i: i32, n: i32) {
        assert_eq!(i + 1, n);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32, i32));
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_i32\"(i: I32, n: I32): Nil

    X := [1, 2, 3]
    record(0, X[0])
    record(1, X[1])
    record(2, X[2])
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}
