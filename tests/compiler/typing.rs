use crate::utils;
use iro::runtime::Runtime;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn cast_on_argument() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 10);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        "\
    extern def record=\"record_i64\"(n: I64): Nil

    def f(n: I64) =>
        record(n)
    f(10)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn cast_on_binop() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 10);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        "\
    extern def record=\"record_i64\"(n: I64): Nil

    def f(n: I64) =>
        record(n-5)
    f(15)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn explicit_cast() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 10);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        "\
    extern def record=\"record_i64\"(n: I64): Nil

    record(10 as I64)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn return_nil() {
    let runtime = Runtime::new();
    utils::parse_and_run(
        "\
    def f(): Nil =>
        pass

    f()
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}
