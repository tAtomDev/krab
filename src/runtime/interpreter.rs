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
