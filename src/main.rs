mod common;
mod core;
mod runtime;
mod util;

use std::io::{self, BufRead, Write};

use crate::{common::Value, core::*, runtime::Interpreter};

use lexer::*;
use parser::*;

fn main() {
    print!("\x1B[2J\x1B[1;1H");
    println!(
        "\x1b[32m- Krab 0.1 REPL\nType 'exit' to leave or 'clear' to clear the terminal.\x1b[0m"
    );

    let mut interpreter = Interpreter::new();

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

        print!("\x1b[0m");
        io::stdout().flush().unwrap();

        if buffer == "clear" {
            print!("\x1B[2J\x1B[1;1H");
            continue;
        }

        if buffer.starts_with("/lex") {
            buffer = buffer.split_off(4);
            let lex = Lexer::new(&buffer).lex();
            println!("{lex:?}");
            continue;
        }

        if buffer.starts_with("/parse") {
            buffer = buffer.split_off(6);
            let program = Parser::new(Lexer::new(&buffer).lex()).parse();
            println!("{program:?}");
            continue;
        }

        if buffer.starts_with("/vars") {
            if interpreter.environment.variables.is_empty() {
                println!("No variables declared");
                continue;
            }

            let variables = interpreter.environment.variables.iter().fold(
                String::new(),
                |acc, (name, variable)| {
                    format!(
                        "{acc}\n{} \"{name}\" = {:?}",
                        if variable.is_const { "const" } else { "let" },
                        variable.value
                    )
                },
            );

            println!("\x1b[36m{}\x1b[0m", variables.trim());
            continue;
        }

        let value = interpreter.evaluate(&buffer);

        if value != Value::Nothing {
            println!("{}", value);
        }
    }

    println!("\x1b[0mREPL EXITED");
}
