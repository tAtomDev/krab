use std::fmt::Display;

use crate::ast::Span;

use super::Type;

#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedIdent {
    pub ident: Ident,
    pub ty: Type,
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self {
            name: value,
            span: Span::EOF,
        }
    }
}
