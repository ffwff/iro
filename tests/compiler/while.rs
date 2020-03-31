use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use iro::runtime::Runtime;
use iro::utils;

#[cfg(test)]

#[test]
fn while_loop() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 10);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        "
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    let i = 0
    while i < 10
        i += 1
    end
    record(i)
    ",
        runtime,
    );
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn while_loop_set() {
    static RUN_IT: AtomicI32 = AtomicI32::new(0);
    extern "C" fn record_i32(n: i32) {
        let prev = RUN_IT.fetch_add(1, Ordering::Relaxed);
        assert_eq!(prev, n);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        "
    @[Static(record_i32)]
    def record(n: I32): Nil
    end

    let i = 0
    while i < 10
        record(i)
        i += 1
    end
    ",
        runtime,
    );
    assert_eq!(RUN_IT.load(Ordering::Relaxed), 10);
}

#[test]
fn while_loop_nested() {
    use std::cell::RefCell;
    use std::sync::Mutex;
    lazy_static! {
        static ref OUTPUT: Mutex<RefCell<Vec<(i32, i32)>>> = Mutex::new(RefCell::new(vec![]));
    }
    extern "C" fn record_i32(i: i32, j: i32) {
        OUTPUT.lock().unwrap().borrow_mut().push((i, j));
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32, i32));
    utils::parse_and_run(
        "
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
    ",
        runtime,
    );
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
    use std::cell::RefCell;
    use std::sync::Mutex;
    lazy_static! {
        static ref OUTPUT: Mutex<RefCell<Vec<(i32, i32, i32)>>> = Mutex::new(RefCell::new(vec![]));
    }
    extern "C" fn record_i32(i: i32, j: i32, x: i32) {
        OUTPUT.lock().unwrap().borrow_mut().push((i, j, x));
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32, i32, i32));
    utils::parse_and_run(
        "
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
    ",
        runtime,
    );
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
    utils::parse_and_run(
        "
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
    ",
        runtime,
    ).unwrap();
}
