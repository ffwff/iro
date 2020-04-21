use crate::utils;
use iro::runtime::pointer::FatPointer;
use iro::runtime::Runtime;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]
#[test]
fn class_init() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_substr(substring: FatPointer<u8>, age: i32) {
        assert_eq!(substring.len(), 3);
        unsafe {
            assert_eq!(std::str::from_utf8(substring.slice()).unwrap(), "ABC");
        }
        assert_eq!(age, 20);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func(
        "record_substr",
        record_substr as extern "C" fn(FatPointer<u8>, i32),
    );
    utils::parse_and_run(
        "\
    extern def record=\"record_substr\"(name: &Substring, age: I32): Nil

    class Person =>
        name: &Substring
        age: I32

    person := (
        Person =>
            name: \"ABC\"
            age: 20
    )
    record(person.name, person.age)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn nested_class_init() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_substr(first_name: FatPointer<u8>, last_name: FatPointer<u8>, age: i32) {
        unsafe {
            assert_eq!(std::str::from_utf8(first_name.slice()).unwrap(), "Abc");
            assert_eq!(std::str::from_utf8(last_name.slice()).unwrap(), "Def");
        }
        assert_eq!(age, 20);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func(
        "record_substr",
        record_substr as extern "C" fn(FatPointer<u8>, FatPointer<u8>, i32),
    );
    utils::parse_and_run(
        "\
    extern def record=\"record_substr\"(first_name: &Substring, last_name: &Substring, age: I32): Nil

    class Name =>
        first: &Substring
        last: &Substring

    class Person =>
        name: Name
        age: I32

    person := (
        Person =>
            name: (
                Name => 
                    first: \"Abc\"
                    last: \"Def\"
            )
            age: 20
    )
    record(person.name.first, person.name.last, person.age)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn nested_slice_class_init() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record(n: i32, slice: [i32; 2]) {
        assert_eq!(n, 1337);
        assert_eq!(slice[0], 100);
        assert_eq!(slice[1], 200);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record", record as extern "C" fn(i32, [i32; 2]));
    utils::parse_and_run(
        "\
    extern def record(n: I32, slice: [I32; 2]): Nil

    class Thing =>
        a_int: I32
        a_slice: [I32; 2]
    
    thing := (
        Thing =>
            a_int: 1337
            a_slice: [100, 200]
    )
    record(thing.a_int, thing.a_slice)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}
