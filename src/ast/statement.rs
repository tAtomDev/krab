use crate::types::{Ident, Signature, TypedIdent};

use super::Expression;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Statement {
    StructDeclaration(Ident, Vec<TypedIdent>),
    VariableDeclaration(TypedIdent, Expression),
    FunctionDeclaration(Signature, Expression),
    Expression(Expression),
    Assignment(Ident, Expression),
    Return(Expression),
}
