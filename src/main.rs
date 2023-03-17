mod common;
mod core;
mod runtime;
mod util;
use colored::Colorize;

use std::{
    fs::File,
    io::{self, BufReader, Read, Write},
};

use crate::{common::Value, core::*, runtime::Interpreter};

use lexer::*;
use parser::*;

fn main() {
    print!("{}", "".normal().clear());
    println!(
        "{}\n{}",
        "- Krab 0.1 REPL".green().bold(),
        "Type 'exit' to leave or 'clear' to clear the terminal".green()
    );

    let mut interpreter = Interpreter::new();

    interpreter
        .environment
        .register_native_function("print", |args| {
            let content = args
                .into_iter()
                .map(|a| a.stringify())
                .collect::<Vec<_>>()
                .join(" ");
            println!("{content}");

            None
        })
        .unwrap();

    let mut buffer = String::with_capacity(2048);
    loop {
        buffer.clear();
        print!("{} ", ">".bright_white().bold());
        io::stdout().flush().unwrap();

        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {}
            Err(error) => {
                eprintln!("{error}");
                continue;
            }
        }
        buffer = buffer.trim().into();
        if buffer == "exit" {
            break;
        }

        io::stdout().flush().unwrap();

        if buffer == "clear" {
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
            let program = Parser::new(Lexer::new(&buffer).lex().unwrap()).parse();
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
                        "{acc}\n{} {name}: {} = {:?}",
                        if variable.is_const { "const" } else { "let" },
                        variable.ty,
                        variable.value
                    )
                },
            );

            println!("{}", variables.trim().cyan());
            continue;
        }

        if buffer.ends_with(".krab") {
            let Ok(file) = File::open(&buffer) else {
                println!("File not found");
                continue;
            };

            let mut buf_reader = BufReader::new(file);
            let mut content = String::with_capacity(1024);

            buf_reader.read_to_string(&mut content).unwrap();
            buffer = content;
        }

        let value = match interpreter.evaluate_source(&buffer) {
            Ok(v) => v.parse_value(),
            Err(e) => {
                eprintln!("{}", e.to_string().red());
                continue;
            }
        };

        let value = match value {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", e.to_string().red());
                continue;
            }
        };

        if value != Value::Nothing {
            println!("{}", value.to_string().yellow());
        }
    }

    println!("REPL EXITED");
}
