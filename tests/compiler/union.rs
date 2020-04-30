use crate::utils;
use iro::runtime::pointer::FatPointer;
use iro::runtime::Runtime;
use std::sync::atomic::{AtomicBool, Ordering};

#[test]
fn union_i32_f32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 1);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 2.0);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        "\
    extern def record=\"record_i32\"(n: I32): Nil
    extern def record=\"record_f64\"(n: F64): Nil

    def f(x) =>
        if x == 0 =>
            1
        else =>
            2.0
    record(f(0) as I32)
    record(f(1) as F64)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
