use iro::codegen::codegen::Settings;
use iro::runtime::Runtime;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn less_than() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 1);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    def f(x, y)
        if x < y
            record(1)
        end
        0
    end
    f(10, 15)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn greater_than() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 1);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    def f(x, y)
        if x > y
            record(1)
        end
        0
    end
    f(15, 10)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn greater_than_eq() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 1);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    def f(x, y)
        if x >= y
            record(1)
        else
            record(1)
        end
        0
    end
    f(15, 10)
    f(15, 15)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn less_than_eq() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 1);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    def f(x, y)
        if x <= y
            record(1)
        else
            record(1)
        end
        0
    end
    f(10, 15)
    f(15, 15)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
