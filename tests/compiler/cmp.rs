use iro::codegen::codegen::Settings;
use iro::runtime::Runtime;
use iro::utils;
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(test)]

macro_rules! cmp {
    ($name:tt, $op:expr, $a:expr, $b:expr) => {
        #[test]
        fn $name() {
            static RUN_FLAG: AtomicBool = AtomicBool::new(false);
            extern "C" fn record_i32(n: i32) {
                assert_eq!(n, 1);
                RUN_FLAG.store(true, Ordering::Relaxed);
            }
            let mut runtime = Runtime::empty();
            runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
            let source = format!("
            extern def record=\"record_i32\"(n: I32): Nil

            def f(x, y)
                if x {} y
                    record(1)
                end
                0
            end
            f({}, {})
            ", $op, $a, $b);
            utils::parse_and_run(
                Settings::default(),
                &source,
                runtime,
            )
            .expect("able to parse_and_run");
            assert!(RUN_FLAG.load(Ordering::Relaxed));
        }
    };
}

cmp!(lt_than_i32, "<", 10, 15);
cmp!(gt_than_i32, ">", 15, 10);
cmp!(le_than_i32, "<=", 10, 15);
cmp!(ge_than_i32, ">=", 15, 10);
cmp!(eq_than_i32, "==", 15, 15);

cmp!(lt_than_f64, "<", 10.0, 15.0);
cmp!(gt_than_f64, ">", 15.0, 10.0);
cmp!(le_than_f64, "<=", 10.0, 15.0);
cmp!(ge_than_f64, ">=", 15.0, 10.0);
cmp!(eq_than_f64, "==", 15.0, 15.0);