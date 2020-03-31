use iro::runtime::Runtime;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn fib() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 89);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        "
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    def fib(n): I32
        if n <= 1
            return 1i32
        end
        fib(n-1) + fib(n-2)
    end
    record(fib(10))
    ",
        runtime,
    );
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn fib64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 89);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def fib(n): I64
        if n <= 1
            return 1i64
        end
        fib(n-1) + fib(n-2)
    end
    record(fib(10i64))
    ",
        runtime,
    );
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
