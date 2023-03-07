mod lexer;
mod parser;
mod util;

use lexer::*;
//use parser::*;

const CODE: &str = r#"
fn main() {
    let x = "Hello, world!";
    print(x);
}
"#;

fn main() {
    let mut lex = Lexer::new(CODE);

    println!("{:?}", lex.lex());

    // let mut parser = Parser::new(lex.lex());

    //println!("{:?}", parser.parse());
}
