use crate::utils;
use iro::compiler::error;
use iro::runtime::Runtime;

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
