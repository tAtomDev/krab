use crate::{ast::Expression, types::Signature};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Function {
    pub signature: Signature,
    pub body_expression: Box<Expression>,
}

impl Function {
    pub fn new(sig: Signature, body_expression: Box<Expression>) -> Self {
        Self {
            signature: sig,
            body_expression,
        }
    }
}
