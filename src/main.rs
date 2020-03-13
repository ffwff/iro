#[macro_use] extern crate maplit;

fn main() {
    let ast = utils::parse_input("
    def f(x,y)
        if 1 == 2
            return x
        else
            if 2 == 3
                return y
            end
        end
    end
    ").unwrap();
    utils::type_visitor(&ast).unwrap();
    println!("{}", ast.exprs[0].type_info().borrow());
}

mod utils {
    use iro::ast;
    use iro::lexer;
    use iro::parser;
    use iro::ast::Visitor;
    use iro::types::visitor::TypeVisitor;
    use iro::types::types::{TypeInfo, FunctionData};

    pub type ParseError = lalrpop_util::ParseError<usize, lexer::Tok, lexer::Error>;
    pub type ParseResult = Result<ast::Program, ParseError>;

    pub fn parse_input(input: &str) -> ParseResult {
        let tokenizer = lexer::Lexer::new(input);
        parser::TopParser::new().parse(tokenizer)
    }

    pub fn type_visitor(ast : &ast::Program) -> ast::VisitorResult {
        let mut visitor = TypeVisitor::new();
        visitor.visit_program(&ast)
    }

    #[cfg(test)]
    pub fn check_function_data<T>(boxed : &iro::ast::NodeBox, callback : T) where T : Fn(&FunctionData) {
        use std::cell::RefCell;
        use std::borrow::Borrow;
        let type_info : &TypeInfo = &boxed.type_info().borrow();
        let function = type_info.as_function().unwrap();
        let data_rc : &RefCell<FunctionData> = function.borrow();
        let data : &FunctionData = &data_rc.borrow();
        callback(data);
    }
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_err());
    }
    
    #[test]
    fn simple_binexpr_infer() {
        let ast = parse_input("
        def x(y)
            return 1 + y
        end
        ").unwrap();
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
        assert!(type_visitor(&ast).is_ok());
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
}