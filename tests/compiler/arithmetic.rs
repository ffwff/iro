use iro::codegen::codegen::Settings;
use iro::runtime::Runtime;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]

macro_rules! load_const {
    ($name:tt, $type:tt, $irotype:expr, $left:expr, $right:expr) => {
        #[test]
        fn $name() {
            static RUN_FLAG: AtomicBool = AtomicBool::new(false);
            extern "C" fn record(n: $type) {
                assert_eq!(n, $right);
                RUN_FLAG.store(true, Ordering::Relaxed);
            }
            let mut runtime = Runtime::empty();
            runtime.insert_func("record", record as extern "C" fn($type));
            let source = format!(
                "
            extern def record=\"record\"(n: {}): Nil

            record({})
            ",
                $irotype, $left
            );
            utils::parse_and_run(Settings::default(), &source, runtime)
                .expect("able to parse_and_run");
            assert!(RUN_FLAG.load(Ordering::Relaxed));
        }
    };
}

macro_rules! arithmetic {
    ($name:tt, $op:expr, $type:tt, $irotype:expr, $a:expr, $b:expr, $c:expr) => {
        #[test]
        fn $name() {
            static RUN_FLAG: AtomicBool = AtomicBool::new(false);
            extern "C" fn record(n: $type) {
                assert_eq!(n, $c);
                RUN_FLAG.store(true, Ordering::Relaxed);
            }
            let mut runtime = Runtime::empty();
            runtime.insert_func("record", record as extern "C" fn($type));
            let source = format!(
                "
            extern def record=\"record\"(n: {}): Nil

            def f(x, y)
                return x {} y
            end
            record(f({}, {}))
            ",
                $irotype, $op, $a, $b
            );
            utils::parse_and_run(Settings::default(), &source, runtime)
                .expect("able to parse_and_run");
            assert!(RUN_FLAG.load(Ordering::Relaxed));
        }
    };
}

macro_rules! arithmetic_const {
    ($name:tt, $op:expr, $type:tt, $irotype:expr, $a:expr, $b:expr, $c:expr) => {
        #[test]
        fn $name() {
            static RUN_FLAG: AtomicBool = AtomicBool::new(false);
            extern "C" fn record(n: $type) {
                assert_eq!(n, $c);
                RUN_FLAG.store(true, Ordering::Relaxed);
            }
            let mut runtime = Runtime::empty();
            runtime.insert_func("record", record as extern "C" fn($type));
            let source = format!(
                "
            extern def record=\"record\"(n: {}): Nil

            record({} {} {})
            ",
                $irotype, $a, $op, $b
            );
            utils::parse_and_run(Settings::default(), &source, runtime)
                .expect("able to parse_and_run");
            assert!(RUN_FLAG.load(Ordering::Relaxed));
        }
    };
}

load_const!(const_i32, i32, "I32", 10, 10);
load_const!(const_i64, i64, "I64", "10i64", 10);
load_const!(const_f64, f64, "F64", "10.0", 10.0);

arithmetic!(add_i32, "+", i32, "I32", 10, 15, 25);
arithmetic!(sub_i32, "-", i32, "I32", 10, 15, -5);
arithmetic!(mul_i32, "*", i32, "I32", 10, 15, 150);
arithmetic!(div_i32, "/", i32, "I32", 40, 2, 20);
arithmetic!(mod_i32, "%", i32, "I32", 30, 7, 2);

arithmetic!(add_i64, "+", i64, "I64", "10i64", "15i64", 25);
arithmetic!(sub_i64, "-", i64, "I64", "10i64", "15i64", -5);
arithmetic!(mul_i64, "*", i64, "I64", "10i64", "15i64", 150);
arithmetic!(div_i64, "/", i64, "I64", "40i64", "2i64", 20);
arithmetic!(mod_i64, "%", i64, "I64", "30i64", "7i64", 2);

arithmetic!(add_f64, "+", f64, "F64", "0.5", "0.5", 1.0);
arithmetic!(sub_f64, "-", f64, "F64", "0.5", "0.5", 0.0);
arithmetic!(mul_f64, "*", f64, "F64", "0.5", "0.5", 0.25);
arithmetic!(div_f64, "/", f64, "F64", "0.5", "0.5", 1.0);

arithmetic_const!(add_i32_const, "+", i32, "I32", 10, 15, 25);
arithmetic_const!(sub_i32_const, "-", i32, "I32", 10, 15, -5);
arithmetic_const!(mul_i32_const, "*", i32, "I32", 10, 15, 150);
arithmetic_const!(div_i32_const, "/", i32, "I32", 40, 2, 20);
arithmetic_const!(mod_i32_const, "%", i32, "I32", 30, 7, 2);

arithmetic_const!(add_i64_const, "+", i64, "I64", "10i64", "15i64", 25);
arithmetic_const!(sub_i64_const, "-", i64, "I64", "10i64", "15i64", -5);
arithmetic_const!(mul_i64_const, "*", i64, "I64", "10i64", "15i64", 150);
arithmetic_const!(div_i64_const, "/", i64, "I64", "40i64", "2i64", 20);
arithmetic_const!(mod_i64_const, "%", i64, "I64", "30i64", "7i64", 2);

arithmetic_const!(add_f64_const, "+", f64, "F64", "0.5", "0.5", 1.0);
arithmetic_const!(sub_f64_const, "-", f64, "F64", "0.5", "0.5", 0.0);
arithmetic_const!(mul_f64_const, "*", f64, "F64", "0.5", "0.5", 0.25);
arithmetic_const!(div_f64_const, "/", f64, "F64", "0.5", "0.5", 1.0);
