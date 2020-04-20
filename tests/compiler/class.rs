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
