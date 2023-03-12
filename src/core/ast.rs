use crate::common::tokens::{Literal, Operator};

pub type Body = Vec<Node>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Node {
    Empty,
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Statement {
    VariableDeclaration {
        name: String,
        value_expression: Box<Expression>,
        is_const: bool,
    },
    Assignment(String, Box<Expression>),
    Expression(Expression),
    Return(Expression),
    While {
        condition: Expression,
        body: Body,
    },
    If {
        condition: Expression,
        body: Body,
        else_branch: Option<Body>,
    },
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Identifier {
    pub kind: IdentifierKind,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum IdentifierKind {
    Function,
    Variable,
}
