use std::collections::HashMap;

use crate::{
    common::Value,
    core::{
        ast::*,
        lexer::{Lexer, Operator},
        parser::Parser,
    },
};

// pub struct Environment {
//     pub variables: HashMap<String, Value>,
// }

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate(&self, src: &str) -> Option<Value> {
        let lex = Lexer::new(src).lex();
        let program = Parser::new(lex).parse();

        let mut last_value = None;
        for statement in program {
            let evaluated = self.evaluate_statement(statement);
            if let Some(value) = last_value.clone() {
                let sum: anyhow::Result<Value> = value + evaluated;
                if let Ok(sum_value) = sum {
                    last_value = Some(sum_value);
                }
            } else {
                last_value = Some(evaluated);
            }
        }

        last_value
    }

    fn evaluate_statement(&self, statement: Statement) -> Value {
        match statement {
            Statement::Expression(expression) => self.evaluate_expression(expression),
            _ => unimplemented!(),
        }
    }

    fn evaluate_expression(&self, expression: Expression) -> Value {
        match expression {
            Expression::Literal(literal) => literal.into(),
            Expression::Binary(..) => self.evaluate_binary_expression(expression),
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
