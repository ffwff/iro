use crate::utils;
use iro::runtime::Runtime;

#[cfg(test)]
#[test]
fn nested_class() {
    extern "C" fn noop(_: i32) {}
    let mut runtime = Runtime::new();
    runtime.insert_func("noop", noop as extern "C" fn(i32));
    assert!(utils::parse_and_run(
        "\
    extern def noop(n: I32): Nil

    class Other =>
        a_int: I32

    class Thing =>
        other: Other
    
    thing := (
        Thing =>
            other: (
                Other =>
                    a_int: 10
            )
    )
    j := thing.other
    noop(j.a_int)
    noop(thing.other.a_int)
    ",
        runtime,
    )
    .is_err());
}

#[test]
fn class() {
    extern "C" fn noop(_: i32) {}
    let mut runtime = Runtime::new();
    runtime.insert_func("noop", noop as extern "C" fn(i32));
    runtime.insert_func("noop_i32", noop as extern "C" fn(i32));
    assert!(utils::parse_and_run(
        "\
    class Other =>
        a_int: I32
    
    extern def noop(n: Other): Nil
    extern def noop_i32(n: I32): Nil

    x := (
        Other =>
            a_int: 10
    )
    noop(x)
    noop_i32(x.a_int)
    ",
        runtime,
    )
    .is_err());
}

#[test]
fn if_expr() {
    let mut runtime = Runtime::new();
    assert!(utils::parse_and_run(
        "\
    mut i := 0
    if i < 10 =>
        i += 1
    else =>
        i += 2
    ",
        runtime,
    )
    .is_ok());
}

#[test]
fn while_loop() {
    let mut runtime = Runtime::new();
    assert!(utils::parse_and_run(
        "\
    extern def noop(n: I32): Nil

    mut i := 0
    while i < 10 =>
        i += 1
    ",
        runtime,
    )
    .is_ok());
}
