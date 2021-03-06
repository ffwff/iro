use crate::utils;
use iro::compiler::error;
use iro::runtime::Runtime;
use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn int_borrow() {
    extern "C" fn record_i32(i: i32) {
        assert_eq!(i, 10);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        "\
    extern def record=\"record_i32\"(i: I32): Nil

    i := 10
    x := &i
    record(*x)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn borrow_after_borrow_mut() {
    extern "C" fn record_i32(i: i32) {
        assert_eq!(i, 10);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    let err = utils::parse_and_run(
        "\
    extern def record=\"record_i32\"(i: I32): Nil

    i := 10
    x := &i
    y := &mut i
    record(*x)
    ",
        runtime,
    )
    .expect_err("error out on parse_and_run");
    match err.error {
        error::Code::MemoryError { typed, .. } => assert_eq!(typed, error::MemoryErrorType::Borrow),
        other => panic!("error isn't memory error: {}", other),
    }
}

#[test]
fn uni_alloc() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    static mut MALLOC_I32: UnsafeCell<i32> = UnsafeCell::new(0);
    extern "C" fn fake_malloc(_size: i32, _align: i32) -> *mut i32 {
        unsafe { UnsafeCell::get(&MALLOC_I32) }
    }
    extern "C" fn fake_dealloc(address: isize) {
        unsafe {
            assert_eq!(address, MALLOC_I32.get() as isize);
        }
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    extern "C" fn record_i32(i: i32) {
        assert_eq!(i, 10);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func(
        "fake_malloc",
        fake_malloc as extern "C" fn(i32, i32) -> *mut i32,
    );
    runtime.insert_func("fake_dealloc", fake_dealloc as extern "C" fn(isize));
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        "\
    extern def fake_malloc(): ISize
    extern def fake_dealloc(address: ISize): Nil
    extern def record=\"record_i32\"(i: I32): Nil

    @[Public]
    def __iro_malloc__(size: I32, align: I32): ISize =>
        fake_malloc()

    @[Public]
    def __iro_dealloc__(address: ISize) =>
        fake_dealloc(address)

    i := uni 10
    record(*i)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn borrow_asg() {
    extern "C" fn record_i32(i: i32) {
        assert_eq!(i, 1000);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        "\
    extern def record=\"record_i32\"(i: I32): Nil

    mut i := 10
    y := &mut i
    *y = 1000
    record(i)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}
