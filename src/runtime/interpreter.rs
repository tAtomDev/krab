use std::fmt::Display;

use crate::{
    common::{tokens::Operator, Value},
    core::{
        ast::*,
        lexer::{Lexer, LexicalError},
        parser::{Parser, ParserError},
    },
};

use super::Environment;

use thiserror::Error;

#[derive(Debug)]
pub enum RuntimeError {
    LexicalError(LexicalError),
    ParserError(ParserError),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexicalError(e) => write!(f, "Lexical Error: {}", e),
            Self::ParserError(e) => write!(f, "Parser Error: {}", e),
        }
    }
}

pub struct Interpreter {
    pub environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None),
        }
    }

    pub fn evaluate_source(&mut self, src: &str) -> Result<Value, RuntimeError> {
        let lex = match Lexer::new(src).lex() {
            Ok(lex) => lex,
            Err(e) => return Err(RuntimeError::LexicalError(e)),
        };

        let program = match Parser::new(lex).parse() {
            Ok(program) => program,
            Err(e) => return Err(RuntimeError::ParserError(e)),
        };

        self.evaluate(program)
    }

    pub fn evaluate(&mut self, program: Vec<Statement>) -> Result<Value, RuntimeError> {
        let value = program
            .into_iter()
            .map(|statement| self.evaluate_statement(statement))
            .last()
            .unwrap_or(Value::Nothing);

        Ok(value)
    }

    fn evaluate_statement(&mut self, statement: Statement) -> Value {
        match statement {
            Statement::Empty => Value::Nothing,
            Statement::Expression(expression) => self.evaluate_expression(expression),
            Statement::VariableDeclaration {
                is_const,
                name,
                value_expression,
            } => {
                let value = self.evaluate_expression(*value_expression);
                self.environment
                    .declare_variable(name, value.clone(), is_const);

                value
            }
            Statement::Assignment(identifier, expression) => {
                let value = self.evaluate_expression(*expression);
                self.environment.assign_variable(identifier, value.clone());

                value
            }
            _ => unimplemented!(),
        }
    }

    fn evaluate_expression(&self, expression: Expression) -> Value {
        match expression {
            Expression::Literal(literal) => literal.into(),
            Expression::Binary(..) => self.evaluate_binary_expression(expression),
            Expression::Unary(op, expression) => {
                let evaluated = self.evaluate_expression(*expression);
                if op == Operator::Subtract {
                    (-evaluated).unwrap()
                } else {
                    evaluated
                }
            }
            Expression::Identifier(identifier) => {
                if identifier.kind == IdentifierKind::Function {
                    panic!("Functions are not yet implemented");
                }

                self.environment
                    .get_variable(&identifier.name)
                    .value
                    .clone()
            }
            _ => todo!(),
        }
    }

    fn evaluate_binary_expression(&self, expression: Expression) -> Value {
        let Expression::Binary(lhs, op, rhs) = expression else {
            unreachable!()
        };

        let lhs = self.evaluate_expression(*lhs);
        let rhs = self.evaluate_expression(*rhs);

        match op {
            Operator::Add => (lhs + rhs).unwrap(),
            Operator::Subtract => (lhs - rhs).unwrap(),
            Operator::Multiply => (lhs * rhs).unwrap(),
            Operator::Divide => (lhs / rhs).unwrap(),
            Operator::Modulo => (lhs % rhs).unwrap(),
            _ => panic!("Invalid operator provided"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs::File,
        io::{BufReader, Read},
    };

    use super::*;

    fn _create_interpreter_and_read_file(file_name: &str) -> (Interpreter, String) {
        let file = File::open(file_name).unwrap();

        let mut buf_reader = BufReader::new(file);
        let mut content = String::with_capacity(1024);

        buf_reader.read_to_string(&mut content).unwrap();

        (Interpreter::new(), content)
    }

    #[test]
    fn basic_math() {
        let (mut interpreter, content) =
            _create_interpreter_and_read_file("./examples/basic_math.krab");
        let evaluated = interpreter.evaluate_source(&content).unwrap();

        assert_eq!(evaluated, Value::Integer(1))
    }
}
