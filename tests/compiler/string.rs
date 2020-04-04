use iro::codegen::codegen::Settings;
use iro::runtime::Runtime;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn len() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 3);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_i32\"(n: I32): Nil

    record(\"ABC\".len)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
