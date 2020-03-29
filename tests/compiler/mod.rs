use iro::runtime::Runtime;
use crate::utils;
pub mod arithmetic;

#[cfg(test)]
#[test]
fn fib() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 89);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run("
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
    ", runtime);
}

#[cfg(test)]
#[test]
fn fib64() {
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 89);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run("
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
    ", runtime);
}

#[cfg(test)]
#[test]
fn while_loop() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 10);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    let i = 0
    while i < 10
        i += 1
    end
    record(i)
    ", runtime);
}

#[cfg(test)]
#[test]
fn while_loop_nested() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 50);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    let i = 0
    let x = 0
    while i < 10
        let j = 0
        while j < 5
            x += 1
            j += 1
        end
        i += 1
    end
    record(x)
    ", runtime);
}