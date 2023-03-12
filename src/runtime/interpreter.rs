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

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Lexical Error: {0}")]
    LexicalError(LexicalError),
    #[error("Parser Error: {0}")]
    ParserError(ParserError),

    #[error("Runtime Error: invalid type arithmetic")]
    InvalidType,

    #[error("Runtime Error: variable `{0}` not found in this scope")]
    VariableNotFound(String),
    #[error("Runtime Error: cannot redeclare variable `{0}`")]
    CannotRedeclareVariable(String),
    #[error("Runtime Error: cannot reassign to constant variable `{0}`")]
    CannotReassignConstVariable(String),
    #[error("Runtime Error: cannot reassign a different type to `{0}`")]
    CannotReassignDifferentType(String),
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

    pub fn evaluate(&mut self, program: Body) -> Result<Value, RuntimeError> {
        let mut last_value = Value::Nothing;

        for node in program {
            last_value = match node {
                Node::Statement(statement) => {
                    self.evaluate_statement(statement)?;
                    Value::Nothing
                }
                Node::Expression(expression) => self.evaluate_expression(expression)?,
                Node::Empty => Value::Nothing,
            };
        }

        Ok(last_value)
    }

    fn evaluate_statement(&mut self, statement: Statement) -> Result<Value, RuntimeError> {
        let value = match statement {
            Statement::Expression(expression) => self.evaluate_expression(expression)?,
            Statement::VariableDeclaration {
                is_const,
                name,
                value_expression,
            } => {
                let value = self.evaluate_expression(*value_expression)?;
                self.environment
                    .declare_variable(name, value.clone(), is_const)?;

                value
            }
            Statement::Assignment(identifier, expression) => {
                let value = self.evaluate_expression(*expression)?;
                self.environment
                    .assign_variable(identifier, value.clone())?;

                value
            }
            _ => unimplemented!(),
        };

        Ok(value)
    }

    fn evaluate_expression(&self, expression: Expression) -> Result<Value, RuntimeError> {
        let value = match expression {
            Expression::Literal(literal) => literal.into(),
            Expression::Binary(..) => self.evaluate_binary_expression(expression)?,
            Expression::Unary(op, expression) => {
                let evaluated = self.evaluate_expression(*expression)?;
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
                    .get_variable(&identifier.name)?
                    .value
                    .clone()
            }
        };

        Ok(value)
    }

    fn evaluate_binary_expression(&self, expression: Expression) -> Result<Value, RuntimeError> {
        let Expression::Binary(lhs, op, rhs) = expression else {
            unreachable!()
        };

        let lhs = self.evaluate_expression(*lhs)?;
        let rhs = self.evaluate_expression(*rhs)?;

        match op {
            Operator::Add => (lhs + rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Subtract => (lhs - rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Multiply => (lhs * rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Divide => (lhs / rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Modulo => (lhs % rhs).map_err(|_| RuntimeError::InvalidType),
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
