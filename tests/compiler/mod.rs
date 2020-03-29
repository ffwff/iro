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

#[test]
fn while_loop_nested() {
    use std::sync::Mutex;
    use std::cell::RefCell;
    lazy_static! {
        static ref OUTPUT: Mutex<RefCell<Vec<(i32, i32)>>> = Mutex::new(RefCell::new(vec![]));
    }
    extern "C" fn record_i32(i: i32, j: i32) {
        OUTPUT.lock().unwrap().borrow_mut().push((i, j));
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32,i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(i: I32, j: I32): Nil
    end

    let i = 0
    while i < 10
        let j = 0
        while j < 5
            record(i, j)
            j += 1
        end
        i += 1
    end
    ", runtime);
    let mut expected = vec![];
    for i in 0..10 {
        for j in 0..5 {
            expected.push((i as i32, j as i32));
        }
    }
    let locked = OUTPUT.lock().unwrap().borrow().clone();
    assert_eq!(locked, expected);
}

#[test]
fn while_loop_nested_x() {
    use std::sync::Mutex;
    use std::cell::RefCell;
    lazy_static! {
        static ref OUTPUT: Mutex<RefCell<Vec<(i32, i32, i32)>>> = Mutex::new(RefCell::new(vec![]));
    }
    extern "C" fn record_i32(i: i32, j: i32, x: i32) {
        OUTPUT.lock().unwrap().borrow_mut().push((i, j, x));
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32,i32,i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(i: I32, j: I32, x: I32): Nil
    end

    let i = 0
    let x = 0
    while i < 10
        let j = 0
        while j < 5
            x += 1
            record(i, j, x)
            j += 1
        end
        i += 1
    end
    ", runtime);
    let mut expected = vec![];
    let mut x = 0;
    for i in 0..10 {
        for j in 0..5 {
            x += 1;
            expected.push((i as i32, j as i32, x as i32));
        }
    }
    let locked = OUTPUT.lock().unwrap().borrow().clone();
    assert_eq!(locked, expected);
}

#[test]
fn while_loop_nested_post_x() {
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 50);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run("
    @[Static(record_i32)]
    def record(x: I32): Nil
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