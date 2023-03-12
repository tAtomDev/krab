use crate::{
    common::{tokens::Operator, Value},
    core::{ast::*, lexer::Lexer, parser::Parser},
};

use super::Environment;

pub struct Interpreter {
    pub environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None),
        }
    }

    pub fn evaluate(&mut self, src: &str) -> Value {
        let lex = Lexer::new(src).lex();
        let program = Parser::new(lex).parse();

        program
            .into_iter()
            .map(|statement| self.evaluate_statement(statement))
            .last()
            .unwrap_or(Value::Nothing)
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
                    panic!("Functions not yet implemented");
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
            panic!("Expected binary expression");
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
    use std::{fs::File, io::{BufReader, Read}};

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
        let (mut interpreter, content) = _create_interpreter_and_read_file("./examples/basic_math.krab");
        let evaluated = interpreter.evaluate(&content);

        assert_eq!(evaluated, Value::Integer(1))
    }
}
