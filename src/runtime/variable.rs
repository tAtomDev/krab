use crate::types::{Ident, Type, TypedIdent, Value};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessModifier {
    Constant,
    ReadWrite,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Variable {
    pub access_modifier: AccessModifier,
    pub ident: Ident,
    pub ty: Type,
    pub value: Value,
}

impl Variable {
    pub fn new(access_modifier: AccessModifier, typed_ident: TypedIdent, value: Value) -> Variable {
        Variable {
            access_modifier,
            ident: typed_ident.ident,
            ty: typed_ident.ty,
            value,
        }
    }
}
