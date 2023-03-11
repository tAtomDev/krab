mod common;
mod core;
mod runtime;
mod util;

use std::io::{self, BufRead, Write};

use crate::{core::*, runtime::Interpreter};

use lexer::*;
use parser::*;

const CODE: &str = r#"
fn main() {
    let x = "Hello, world!";
    print(x);
}
"#;

fn main() {
    print!("\x1B[2J\x1B[1;1H");
    println!("\x1b[32m- Krab 0.1 REPL\nType 'exit' to leave or 'clear' to clear terminal.\x1b[0m");

    let interpreter = Interpreter;

    let mut buffer = String::with_capacity(2048);
    let mut stdin = io::stdin().lock();
    loop {
        buffer.clear();
        print!("> \x1b[33m");
        io::stdout().flush().unwrap();

        stdin.read_line(&mut buffer).unwrap();
        buffer = buffer.trim().into();
        if buffer == "exit" {
            drop(stdin);
            break;
        }

        if buffer == "clear" {
            print!("\x1b[0m\x1B[2J\x1B[1;1H");
            continue;
        }

        if buffer.starts_with("lex") {
            buffer = buffer.split_off(4);
            let lex = Lexer::new(&buffer).lex();
            println!("\x1b[0m{:?}", lex);
            continue;
        }

        if buffer.starts_with("parse") {
            buffer = buffer.split_off(6);
            let program = Parser::new(Lexer::new(&buffer).lex()).parse();
            println!("\x1b[0m{:?}", program);
            continue;
        }

        let value = interpreter.evaluate(&buffer);

        if let Some(value) = value {
            println!("\x1b[0m{:?}", value);
        } else {
            println!("\x1b[0mundefined");
        }
    }

    println!("\x1b[0mREPL EXITED");
}
