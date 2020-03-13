use iro::ast;
use iro::lexer;
use iro::parser;
use iro::ast::Visitor;
use iro::types::visitor::TypeVisitor;

fn main() {
}

#[cfg(test)]
#[macro_use] extern crate maplit;

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::borrow::Borrow;
    use iro::types::types::*;

    fn parse_input(input: &str) -> ast::Program {
        let tokenizer = lexer::Lexer::new(input);
        parser::TopParser::new().parse(tokenizer).unwrap()
    }

    fn check_function_data<T>(boxed : &iro::ast::NodeBox, callback : T) where T : Fn(&FunctionData) {
        let type_info : &TypeInfo = &boxed.type_info().borrow();
        let function = type_info.as_function().unwrap();
        let data_rc : &RefCell<FunctionData> = function.borrow();
        let data : &FunctionData = &data_rc.borrow();
        callback(data);
    }

    fn type_visitor(ast : &ast::Program) -> ast::VisitorResult {
        let mut visitor = TypeVisitor::new();
        visitor.visit_program(&ast)
    }

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
        ");
        assert!(type_visitor(&ast).is_ok());
        let a = Type::Union(hashset![Type::Integer, Type::String, Type::Nil]);
        let b = Type::Union(hashset![Type::Integer, Type::Nil]);
        assert_eq!(ast.exprs[0].type_info().borrow().typed(), &Type::Integer);
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &a);
        assert_eq!(ast.exprs[2].type_info().borrow().typed(), &b);
    }

    #[test]
    fn empty_if() {
        let ast = parse_input("
        if 10 == 10
        else
        end
        if 10 == 10
        end
        ");
        assert!(type_visitor(&ast).is_ok());
        assert_eq!(ast.exprs[0].type_info().borrow().typed(), &Type::Nil);
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &Type::Nil);
    }
    
    #[test]
    fn simple_def() {
        let ast = parse_input("
        def x(y)
            return 1 + y
        end
        ");
        assert!(type_visitor(&ast).is_ok());
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Integer);
        });
    }
    
    #[test]
    fn simple_def_if() {
        let ast = parse_input("
        def x(y)
            if 1 == 2
                return 1 + y
            end
        end
        ");
        assert!(type_visitor(&ast).is_ok());
        println!("{:#?}", ast);
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Integer);
        });
    }
    
    #[test]
    fn simple_binexpr_infer() {
        let ast = parse_input("
        def x(y)
            return 1 + y
        end
        ");
        assert!(type_visitor(&ast).is_ok());
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Integer);
        });
    }
    
    #[test]
    fn ident_infer() {
        let ast = parse_input("
        def x(y)
            z = y + 1
            return z
        end
        ");
        assert!(type_visitor(&ast).is_ok());
        check_function_data(&ast.exprs[0], |data| {
            assert_eq!(data.returntype.typed(), &Type::Integer);
        });
    }
    
    #[test]
    fn call_simple_args() {
        let ast = parse_input("
        def x(y)
            return y + 1
        end
        x(10)
        ");
        assert!(type_visitor(&ast).is_ok());
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &Type::Integer);
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
        ");
        assert!(type_visitor(&ast).is_ok());
        check_function_data(&ast.exprs[0], |data| { 
            {
                let vdata_rc : &RefCell<VariableData> = &data.args[0].1.borrow();
                let vdata = &vdata_rc.borrow();
                assert_eq!(vdata.type_info.typed(), &Type::Integer);
            }
            assert_eq!(data.returntype.typed(), &Type::Integer);
        });
        check_function_data(&ast.exprs[1], |data| { 
            {
                let vdata_rc : &RefCell<VariableData> = &data.args[0].1.borrow();
                let vdata = &vdata_rc.borrow();
                assert_eq!(vdata.type_info.typed(), &Type::Integer);
            }
            assert_eq!(data.returntype.typed(), &Type::Integer);
        });
    }
    
    #[test]
    fn call_infer_caller_args2() {
        let ast = parse_input("
        def f(x,y)
            return x+y
        end
        f(10,10)
        ");
        assert!(type_visitor(&ast).is_ok());
        check_function_data(&ast.exprs[0], |data| {
            assert!(data.overloads.is_some());
            let overloads = data.overloads.as_ref().unwrap();
            assert!(!overloads.is_empty());
            let overload = overloads.iter().next().unwrap();
            assert_eq!(overload.args[0].typed(), &Type::Integer);
            assert_eq!(overload.args[1].typed(), &Type::Integer);
            assert_eq!(overload.returntype.typed(), &Type::Integer);
        });
        assert_eq!(ast.exprs[1].type_info().borrow().typed(), &Type::Integer);
    }
}