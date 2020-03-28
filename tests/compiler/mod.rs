use std::sync::atomic::{AtomicI32, Ordering};
use iro::runtime::Runtime;
use iro::runtime::ToGenericFunction;
use crate::utils;

#[cfg(test)]
#[test]
fn while_loop() {
    static atomic: AtomicI32 = AtomicI32::new(0);
    extern "C" fn record_i32(n: i32) {
        atomic.swap(n, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    // println!("{:p} {:#?}", record_i32 as *const libc::c_void, runtime);
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
    assert_eq!(atomic.load(Ordering::Relaxed), 10);
}