use iro::codegen::codegen::Settings;
use iro::runtime::Runtime;
use iro::ssa::isa::{FunctionName, Type};
use iro::utils;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

#[cfg(test)]
#[test]
fn if_expr() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(n: i32) {
        assert_eq!(n, 1);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32));
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_i32\"(n: I32): Nil

    def f(x)
        if x > 10
            record(1)
        else
            record(0)
        end
        0
    end
    f(20)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn if_expr_elsif() {
    static RUN_FLAG: AtomicBool = AtomicBool::new(false);
    extern "C" fn record_i32(i: i32, n: i32) {
        assert_eq!(i, n);
        RUN_FLAG.store(true, Ordering::Relaxed);
    }
    let mut runtime = Runtime::empty();
    runtime.insert_func("record_i32", record_i32 as extern "C" fn(i32, i32));
    utils::parse_and_run(
        Settings::default(),
        "
    extern def record=\"record_i32\"(i: I32, n: I32): Nil

    def f(i, x)
        if x > 10
            record(i, 0)
        elsif x > 5
            record(i, 1)
        else
            record(i, 2)
        end
        0
    end
    f(0, 20)
    f(1, 6)
    f(2, 0)
    ",
        runtime,
    )
    .expect("able to parse_and_run");
    assert!(RUN_FLAG.load(Ordering::Relaxed));
}

#[test]
fn if_expr_unify() {
    let program = utils::parse_to_ssa(
        "
    def f(x)
        return if x > 10
            0
        else
            \"ABC\"
        end
    end
    f(10)
    ",
    )
    .expect("able to parse_to_ssa");
    println!("{:#?}", program.contexts);
    let function = program
        .contexts
        .get(&FunctionName {
            name: Rc::from("f"),
            arg_types: vec![Type::I32],
        })
        .expect("f(I32) exists");
    let set_rc = function.rettype.as_union().unwrap();
    let set: &BTreeSet<Type> = &set_rc;
    assert_eq!(set, &btreeset![Type::I32, Type::String]);
}

#[test]
fn if_expr_unify_true_branch() {
    let program = utils::parse_to_ssa(
        "
    def f(x)
        return if x > 10
            0
        end
    end
    f(10)
    ",
    )
    .expect("able to parse_to_ssa");
    println!("{:#?}", program.contexts);
    let function = program
        .contexts
        .get(&FunctionName {
            name: Rc::from("f"),
            arg_types: vec![Type::I32],
        })
        .expect("f(I32) exists");
    let set_rc = function.rettype.as_union().unwrap();
    let set: &BTreeSet<Type> = &set_rc;
    assert_eq!(set, &btreeset![Type::I32, Type::Nil]);
}

#[test]
fn if_expr_unify_false_branch() {
    let program = utils::parse_to_ssa(
        "
    def f(x)
        return if x > 10
        else
            0
        end
    end
    f(10)
    ",
    )
    .expect("able to parse_to_ssa");
    println!("{:#?}", program.contexts);
    let function = program
        .contexts
        .get(&FunctionName {
            name: Rc::from("f"),
            arg_types: vec![Type::I32],
        })
        .expect("f(I32) exists");
    let set_rc = function.rettype.as_union().unwrap();
    let set: &BTreeSet<Type> = &set_rc;
    assert_eq!(set, &btreeset![Type::I32, Type::Nil]);
}
