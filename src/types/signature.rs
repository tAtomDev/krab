use crate::ast::SpanInterval;

use super::{Ident, Type, TypedIdent};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Signature {
    pub identifier: Ident,
    pub args: Vec<TypedIdent>,
    pub return_type: Type,
    pub span: SpanInterval,
}
