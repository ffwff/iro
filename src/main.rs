#[macro_use] extern crate maplit;
use iro::utils;

fn main() {
    /* let ast = utils::parse_input("
    def f(a,b)
        return g(a, b)
    end
    def g(x : Int, y)
        return f(x, y)
    end
    ").unwrap(); */
    /* let ast = utils::parse_input("
    def f(a,b)
        return g(a, 10)
    end
    def g(x, y)
        return x + y
    end
    ").unwrap(); */
    /* let ast = utils::parse_input("
    x = 10
    x
    ").unwrap(); */
    /* let ast = utils::parse_input("
    def f(x, y)
        return x + 10
    end
    ").unwrap(); */
    let ast = utils::parse_input("
    def f(x,y)
        return x+y
    end
    def g(x,y)
        z = f(x, y) + 10
        return z
    end
    g(1, 2)
    ").unwrap();
    println!("{:#?}", ast);
    if let Err(e) = utils::type_visitor(&ast) {
        println!("error: {:#?}", e);
    }
    println!("{:#?}", ast);
    // utils::ssa_visitor(&ast).unwrap();
}

#[cfg(test)]
mod type_tests {
    use crate::utils::*;
    use std::cell::RefCell;
    use std::borrow::Borrow;
    use iro::types::types::*;

    #[test]
    fn simple_ifs() {
        let ast = parse_input("
        if 10 == 10
            a = 1
            a + 2
        else
            if 10 == 10
                a = \"a\"
                a + \"a\"
            end
            b = 1
        end
        a
        b
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        let a = Type::Union(hashset![Type::Int, Type::String, Type::Nil]);
        let b = Type::Union(hashset![Type::Int, Type::Nil]);
        assert_eq!(ast.exprs[0].type_info().borrow().typed(), &Type::Int);
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &a);
        assert_eq!(ast.exprs[2].type_info().borrow().typed(), &b);
    }

    #[test]
    fn if_with_two_empty_branch() {
        let ast = parse_input("
        if 10 == 10
        else
        end
        if 10 == 10
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        assert_eq!(ast.exprs[0].type_info().borrow().typed(), &Type::Nil);
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &Type::Nil);
    }

    #[test]
    fn if_with_one_empty_branch() {
        let ast = parse_input("
        if 10 == 10
            10
        else
        end
        if 10 == 10
        else
            10
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        let typed = Type::Union(hashset![Type::Int, Type::Nil]);
        assert_eq!(ast.exprs[0].type_info().borrow().typed(), &typed);
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &typed);
    }
    
    #[test]
    fn simple_def() {
        let ast = parse_input("
        def x(y)
            return 1 + y
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Int);
        });
    }
    
    #[test]
    fn simple_def_return_one_branch() {
        let ast = parse_input("
        def x(y)
            if 1 == 2
                return 1 + y
            end
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Union(hashset![Type::Int, Type::Nil]));
        });
    }
    
    #[test]
    fn simple_def_return_both_branch() {
        let ast = parse_input("
        def x(y)
            if 1 == 2
                return 1 + y
            else
                return 10
            end
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Int);
        });
    }
    
    #[test]
    fn simple_def_return_one_branch_nested() {
        let ast = parse_input("
        def x(y)
            if 1 == 2
                if 1 == 2
                    return 1 + y
                end
            else
                return 10
            end
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Union(hashset![Type::Int, Type::Nil]));
        });
    }
    
    #[test]
    fn simple_def_typed_arguments() {
        let ast = parse_input("
        def x(y : Int)
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            {
                let vdata_rc : &RefCell<VariableData> = &data.args[0].1.borrow();
                let vdata = &vdata_rc.borrow();
                assert_eq!(vdata.type_info.typed(), &Type::Int);
            }
        });
    }
    
    #[test]
    fn simple_def_typed_arguments_check() {
        let ast = parse_input("
        def x(y : Integer)
            y + \"A\"
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Err(iro::ast::Error::IncompatibleType));
    }
    
    #[test]
    fn simple_binexpr_infer() {
        let ast = parse_input("
        def x(y)
            return 1 + y
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Int);
        });
    }
    
    #[test]
    fn ident_infer() {
        let ast = parse_input("
        def x(y)
            z = y + 1
            return z
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Int);
        });
    }
    
    #[test]
    fn call_simple_args() {
        let ast = parse_input("
        def x(y)
            return y + 1
        end
        x(10)
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &Type::Int);
    }
    
    #[test]
    fn call_infer_caller_args() {
        let ast = parse_input("
        def f(y)
            return y + 1
        end
        def g(y)
            return f(y)
        end
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| { 
            {
                let vdata_rc : &RefCell<VariableData> = &data.args[0].1.borrow();
                let vdata = &vdata_rc.borrow();
                assert_eq!(vdata.type_info.typed(), &Type::Int);
            }
            assert_eq!(data.returntype.typed(), &Type::Int);
        });
        check_function_data(&ast.exprs[1], |data| { 
            {
                let vdata_rc : &RefCell<VariableData> = &data.args[0].1.borrow();
                let vdata = &vdata_rc.borrow();
                assert_eq!(vdata.type_info.typed(), &Type::Int);
            }
            assert_eq!(data.returntype.typed(), &Type::Int);
        });
    }
    
    #[test]
    fn call_infer_caller_args2() {
        let ast = parse_input("
        def f(x,y)
            return x+y
        end
        f(10,10)
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        check_function_data(&ast.exprs[0], |data| {
            assert!(data.overloads.is_some());
            let overloads = data.overloads.as_ref().unwrap();
            assert!(!overloads.is_empty());
            let overload = overloads.iter().next().unwrap();
            assert_eq!(overload.args[0].typed(), &Type::Int);
            assert_eq!(overload.args[1].typed(), &Type::Int);
            assert_eq!(overload.returntype.typed(), &Type::Int);
        });
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &Type::Int);
    }
    
    #[test]
    fn call_infer_indirect_arguments_both_const() {
        let ast = parse_input("
        def f(x,y)
            return g(x, y)
        end
        def g(x, y)
            return x + y
        end
        f(1,1)
        ").unwrap();
        assert_eq!(type_visitor(&ast), Ok(()));
        println!("{:#?}", ast);
        check_function_data(&ast.exprs[0], |data| {
            assert!(data.overloads.is_some());
            let overloads = data.overloads.as_ref().unwrap();
            assert!(!overloads.is_empty());
            let overload = overloads.iter().next().unwrap();
            assert_eq!(overload.args[0].typed(), &Type::Int);
            assert_eq!(overload.args[1].typed(), &Type::Int);
            assert_eq!(overload.returntype.typed(), &Type::Int);
        });
        assert_eq!(ast.exprs[2].type_info().borrow().typed(), &Type::Int);
    }
    
    #[test]
    fn call_infer_indirect_arguments_one_const() {
        let ast = parse_input("
        def f(x,y)
            return g(x, y)
        end
        def g(a, b)
            return a + 10
        end
        f(1,1)
        ").unwrap();
        println!("{:#?}", type_visitor(&ast));
        println!("{:#?}", ast);
        //assert_eq!(type_visitor(&ast), Ok(()));
    }
}