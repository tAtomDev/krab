use std::io::{self, Write};

use krab::prelude::*;

fn main() {
    println!("- Krab 0.2 REPL\nType 'exit' to leave");

    let mut engine = Engine::new();
    let mut buffer = String::with_capacity(2048);

    loop {
        buffer.clear();
        print!("> ");
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

        if buffer.starts_with("/lex") {
            buffer = buffer.split_off(4);

            let lex = Lexer::new(&buffer).lex();
            println!("{lex:?}");
            continue;
        }

        if buffer.starts_with("/parse") {
            buffer = buffer.split_off(6);
            let program =
                Parser::new(Lexer::new(&buffer).lex().unwrap(), TypeCache::new_empty()).parse_ast();
            println!("{program:?}");
            continue;
        }

        if buffer.ends_with(".krab") || buffer.starts_with("./") {
            let success = match engine.load_file(&buffer) {
                Ok(success) => success,
                Err(err) => {
                    eprintln!("{err}");
                    continue;
                }
            };

            if !success {
                println!("File not found");
                continue;
            }
        } else if let Err(e) = engine.load_source(&buffer) {
            eprintln!("{}", e);
            continue;
        };

        let eval = engine.eval_ast();

        match eval {
            Err(e) => {
                eprintln!("RuntimeError: {e}");
                continue;
            }
            Ok(value) => {
                if value != Value::Unit {
                    println!("{:?}", value)
                }
            }
        };
    }

    println!("REPL EXITED");
}
