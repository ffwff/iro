use iro::ast;
use iro::lexer;
use iro::parser;
use iro::ast::Visitor;
use iro::types::visitor::TypeVisitor;

fn main() {
    /* let input = "
    def x(y)
        z = y + 1
        return z
    end
    "; */
    /* let input = "
    def x(y,z)
        return y + z
    end
    ";  */
    /* let input = "
    def x(y)
        return 1 + y
    end
    "; */
    let input = "
    def x(y,z)
        if 10
            return y
        else
            x = y + z
            return x
        end
    end
    ";
    /* let input = "
    if 10 == 10
        a = 1
        a + 2
    else
        a
        if 10 == 10
            a = \"a\"
            a + \"a\"
        end
        b = 1
    end
    x
    "; */
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer).unwrap();
    println!("{:#?}", ast.exprs);
    let mut visitor = TypeVisitor::new();
    if let Err(err) = visitor.visit_program(&ast) {
        println!("error: {:#?}", err);
    }
    println!("{:#?}", ast.exprs);
}