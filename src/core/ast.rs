use crate::common::{
    tokens::{Literal, Operator},
    ControlFlow, Type,
};

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
        ty: Type,
        value_expression: Box<Expression>,
        is_const: bool,
    },
    FunctionDeclaration {
        name: String,
        args: Vec<(Type, String)>,
        body: Box<Expression>,
    },
    Assignment(String, Box<Expression>),
    Expression(Expression),
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Body(Body),
    Call(String, Vec<Box<Expression>>),
    If {
        condition: Box<Expression>,
        body: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    ControlFlow(ControlFlow, Option<Box<Expression>>),
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
