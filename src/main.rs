mod ast;
mod lexer;
mod parser;
mod util;

use lexer::*;
use parser::*;

const CODE: &str = r#"
fn main() {
    let x = "Hello, world!";
    print(x);
}
"#;

fn main() {
    let code = "1 + 2 * (7 + 1)";
    let mut lex = Lexer::new(code);
    let tokens = lex.lex();

    println!("{:?}\n", tokens);

    let mut parser = Parser::new(tokens);
    println!("{:#?}", parser.parse());

    // let mut parser = Parser::new(lex.lex());

    //println!("{:?}", parser.parse());
}
