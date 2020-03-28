use iro::runtime::Runtime;
use crate::utils;

#[cfg(test)]
#[test]
fn constant_i32() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 10);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    record(10)
    ", runtime);
}

#[cfg(test)]
#[test]
fn constant_i64() {
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 10);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run("
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    record(10i64)
    ", runtime);
}

#[cfg(test)]
#[test]
fn add_i32() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 25);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    def f(x, y)
        return x + y
    end
    record(f(10, 15))
    ", runtime);
}

#[cfg(test)]
#[test]
fn sub_i32() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, -5);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    def f(x, y)
        return x - y
    end
    record(f(10, 15))
    ", runtime);
}

#[cfg(test)]
#[test]
fn add_i64() {
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 25);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run("
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x + y
    end
    record(f(10i64, 15i64))
    ", runtime);
}

#[cfg(test)]
#[test]
fn sub_i64() {
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, -5);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run("
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x - y
    end
    record(f(10i64, 15i64))
    ", runtime);
}

#[cfg(test)]
#[test]
fn imul_i64() {
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 150);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run("
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x * y
    end
    record(f(10i64, 15i64))
    ", runtime);
}

#[cfg(test)]
#[test]
fn idiv_i64() {
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 20);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run("
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x / y
    end
    record(f(40i64, 2i64))
    ", runtime);
}
