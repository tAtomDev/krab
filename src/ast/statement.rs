use crate::types::{Ident, Signature, TypedIdent};

use super::Expression;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(TypedIdent, Expression),
    Assignment(Ident, Expression),
    FunctionDeclaration(Signature, Expression),
    Return(Expression),
}
