use crate::utils;
use iro::runtime::pointer::FatPointer;
use iro::runtime::Runtime;
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
        "\
    extern def record=\"record_i64\"(n: ISize): Nil

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
        "\
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
        "\
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
        "\
    extern def record=\"record_substr\"(n: &Substring): Nil

    def f(x) =>
        record(x)

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
        "\
    extern def record=\"record_i32\"(n: I32): Nil

    record(\"A\"[0])
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn substring_return() {
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
        "\
    extern def record=\"record_substr\"(n: &Substring): Nil
    
    def f(): &Substring =>
        \"ABC\"
    record(f())
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn substring_return_with_args() {
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
        "\
    extern def record=\"record_substr\"(n: &Substring): Nil
    
    def f(x: I32, y: I32, z: I32): &Substring =>
        \"ABC\"
    record(f(1,2,3))
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn substring_escape_seq() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_substr(substring: FatPointer<u8>) {
        assert_eq!(substring.len(), 5);
        unsafe {
            assert_eq!(std::str::from_utf8(substring.slice()).unwrap(), "\n\r\ta\\");
        }
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func(
        "record_substr",
        record_substr as extern "C" fn(FatPointer<u8>),
    );
    utils::parse_and_run(
        "\
    extern def record=\"record_substr\"(n: &Substring): Nil

    record(\"\\n\\r\\t\\a\\\\\")
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
