use crate::utils;
use iro::runtime::Runtime;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn fib() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 89);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        "\
    extern def record=\"record_i32\"(n: I32): Nil

    def fib(n): I32 =>
        if n <= 1 =>
            return 1i32
        fib(n-1) + fib(n-2)
    record(fib(10))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
