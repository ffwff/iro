use iro::codegen::cranelift::Settings;
use iro::runtime::Runtime;
use iro::utils;

#[cfg(test)]
#[test]
fn and_expr() {
    extern "C" fn record_i32(i: i32, n: i32) {
        assert_eq!(i, n);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32, i32));
    utils::parse_and_run(
        Settings::default(),
        "\
    extern def record=\"record_i32\"(i: I32, n: I32): Nil

    def f(i, x, y) =>
        if x and y =>
            record(i, 1)
        else =>
            record(i, 0)
    f(0, true, false)
    f(0, false, true)
    f(0, false, false)
    f(1, true, true)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}

#[test]
fn or_expr() {
    extern "C" fn record_i32(i: i32, n: i32) {
        assert_eq!(i, n);
    }
    let mut runtime = Runtime::new();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32, i32));
    utils::parse_and_run(
        Settings::default(),
        "\
    extern def record=\"record_i32\"(i: I32, n: I32): Nil

    def f(i, x, y) =>
        if x or y =>
            record(i, 1)
        else =>
            record(i, 0)
    f(1, true, false)
    f(1, false, true)
    f(1, true, true)
    f(0, false, false)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
}
