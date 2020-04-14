use iro::codegen::cranelift::Settings;
use iro::runtime::Runtime;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn index() {
    extern "C" fn record_i32(i: i32, n: i32) {
        assert_eq!(i + 1, n);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32, i32));
    utils::parse_and_run(
        Settings::default(),
        "\
    extern def record=\"record_i32\"(i: I32, n: I32): Nil

    x := [1, 2, 3]
    record(0, x[0])
    record(1, x[1])
    record(2, x[2])
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn ffi() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_substr(slice: [i32; 4]) {
        assert_eq!(slice[0], 10);
        assert_eq!(slice[1], 20);
        assert_eq!(slice[2], 30);
        assert_eq!(slice[3], 40);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_slice", record_substr as extern "C" fn([i32; 4]));
    utils::parse_and_run(
        Settings::default(),
        "\
    extern def record=\"record_slice\"(n: [I32; 4]): Nil

    x := [10,20,30,40]
    record(x)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn index_assign() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 100);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "\
    extern def record=\"record_i32\"(i: I32): Nil

    x:=[1,2,3]
    x[0] = 100
    record(x[0])
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn slice_return() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_substr(slice: [i32; 4]) {
        assert_eq!(slice[0], 10);
        assert_eq!(slice[1], 20);
        assert_eq!(slice[2], 30);
        assert_eq!(slice[3], 40);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_slice", record_substr as extern "C" fn([i32; 4]));
    utils::parse_and_run(
        Settings::default(),
        "\
    extern def record=\"record_slice\"(n: [I32; 4]): Nil
    
    def f() =>
        [10,20,30,40]
    record(f())
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
