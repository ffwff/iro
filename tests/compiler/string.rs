use iro::codegen::cranelift::Settings;
use iro::runtime::Runtime;
use iro::runtime::pointer::FatPointer;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn len() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i64(n: i64) {
        assert_eq!(n, 3);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i64", record_i64 as extern "C" fn(i64));
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_i64\"(n: I64): Nil

    record(\"ABC\".len)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn ptr_access() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i8(n: i8) {
        assert_eq!(n, 'A' as i8);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i8", record_i8 as extern "C" fn(i8));
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_i8\"(n: I8): Nil

    record(\"ABC\"[0])
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn substring_ffi() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_substr(substring: FatPointer<u8>) {
        assert_eq!(substring.len(), 3);
        unsafe {
            assert_eq!(std::str::from_utf8(substring.slice()).unwrap(), "ABC");
        }
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func(
        "record_substr",
        record_substr as extern "C" fn(FatPointer<u8>),
    );
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_substr\"(n: &Substring): Nil

    record(\"ABC\")
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn substring_passing() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_substr(substring: FatPointer<u8>) {
        assert_eq!(substring.len(), 3);
        unsafe {
            assert_eq!(std::str::from_utf8(substring.slice()).unwrap(), "ABC");
        }
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func(
        "record_substr",
        record_substr as extern "C" fn(FatPointer<u8>),
    );
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_substr\"(n: &Substring): Nil

    def f(x)
        record(x)
    end

    f(\"ABC\")
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn string_dereference() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 0x41);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_i32\"(n: I32): Nil

    record(\"A\"[0])
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
