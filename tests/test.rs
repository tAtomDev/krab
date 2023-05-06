use krab::prelude::*;

fn eval_file(file: &str) -> Value {
    let mut engine = Engine::new();
    let success = match engine.load_file(file) {
        Ok(s) => s,
        Err(e) => panic!("{e}"),
    };

    if !success {
        panic!("file {file} not found");
    }

    engine.eval_ast().unwrap()
}

#[test]
fn eval_expressions() {
    assert_eq!(
        eval_file("./tests/test_expressions.krab"),
        Value::Integer(50)
    );
}

#[test]
fn eval_logical() {
    assert_eq!(eval_file("./tests/test_logical.krab"), Value::Boolean(true));
}

#[test]
fn eval_functions() {
    assert_eq!(eval_file("./tests/test_functions.krab"), Value::Integer(21));
}

#[test]
fn eval_factorial() {
    assert_eq!(eval_file("./tests/factorial.krab"), Value::Integer(120));
}
