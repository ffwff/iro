use iro::codegen::codegen::Settings;
use iro::runtime::Runtime;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn constant_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 10);
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

    record(10)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn constant_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 10);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    record(10i64)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn constant_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 10.5);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    record(10.5)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn add_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 25);
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
        return x + y
    end
    record(f(10, 15))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn sub_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, -5);
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
        return x - y
    end
    record(f(10, 15))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn mul_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 80);
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
        return x * y
    end
    record(f(40, 2))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn div_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 20);
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
        return x / y
    end
    record(f(40, 2))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn add_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 25);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x + y
    end
    record(f(10i64, 15i64))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn sub_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, -5);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x - y
    end
    record(f(10i64, 15i64))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn mul_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 150);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x * y
    end
    record(f(10i64, 15i64))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn div_i64() {
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 20);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    def f(x, y)
        return x / y
    end
    record(f(40i64, 2i64))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn add_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 1.0);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    def f(x, y)
        return x + y
    end
    record(f(0.5, 0.5))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn sub_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 0.0);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    def f(x, y)
        return x - y
    end
    record(f(0.5, 0.5))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn mul_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 0.25);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    def f(x, y)
        return x * y
    end
    record(f(0.5, 0.5))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn div_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 1.0);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    def f(x, y)
        return x / y
    end
    record(f(0.5, 0.5))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn add_const_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 25);
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

    record(10+15)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn sub_const_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, -5);
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

    record(10-15)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn mul_const_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 150);
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

    record(10*15)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn div_const_i32() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 10);
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

    record(100/10)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn add_const_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 15);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    record(10i64 + 5i64)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn sub_const_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 5);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    record(10i64 - 5i64)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn mul_const_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 150);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    record(10i64 * 15i64)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn div_const_i64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 10);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_i64)]
    def record(n: I64): Nil
    end

    record(100i64 / 10i64)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn add_const_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 1.0);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    record(0.5 + 0.5)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn sub_const_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 1.0);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    record(1.5 - 0.5)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn mul_const_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 0.25);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    record(0.5 * 0.5)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn div_const_f64() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_f64(n: f64) {
        assert_eq!(n, 1.0);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_f64", record_f64 as extern "C" fn(f64));
    utils::parse_and_run(
        Settings::default(),
        "
    @[Static(record_f64)]
    def record(n: F64): Nil
    end

    record(0.5 / 0.5)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}