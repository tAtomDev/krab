use std::collections::HashMap;

use crate::{
    ast::Span,
    error::LogicalError,
    runtime::AccessModifier,
    types::{Type, TypedIdent},
};

#[derive(Debug, Clone, Default)]
pub struct TypeScope {
    pub variables: HashMap<String, (Span, AccessModifier, Type)>,
    pub functions: HashMap<String, (Span, Type, Vec<TypedIdent>)>,
}

#[derive(Debug, Clone)]
pub struct TypeCache {
    pub scopes: Vec<TypeScope>,
    pub structs: HashMap<String, Vec<TypedIdent>>,
}

impl Default for TypeCache {
    fn default() -> Self {
        Self {
            scopes: vec![TypeScope::default()],
            structs: HashMap::new(),
        }
    }
}

impl TypeCache {
    pub fn new_empty() -> TypeCache {
        TypeCache::default()
    }

    pub fn scope(&mut self) -> &mut TypeScope {
        if self.scopes.is_empty() {
            self.push_scope();
        }

        self.scopes.last_mut().unwrap()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(TypeScope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare_struct(
        &mut self,
        name: &str,
        fields: &[TypedIdent],
        span: Span,
    ) -> Result<(), LogicalError> {
        if self.structs.contains_key(name) {
            return Err(LogicalError::StructAlreadyExists(name.to_string(), span));
        }

        self.structs.insert(name.to_string(), fields.to_vec());
        Ok(())
    }

    pub fn declare_function(
        &mut self,
        name: &str,
        return_ty: Type,
        args: &[TypedIdent],
        span: Span,
    ) -> Result<(), LogicalError> {
        let scope = self.scope();

        if scope.functions.contains_key(name) {
            return Err(LogicalError::FunctionAlreadyExistsInScope(
                name.to_string(),
                span,
            ));
        }

        scope
            .functions
            .insert(name.to_string(), (span, return_ty, args.to_vec()));

        Ok(())
    }

    pub fn declare_variable(
        &mut self,
        name: &str,
        ty: Type,
        access_modifier: AccessModifier,
        span: Span,
    ) -> Result<(), LogicalError> {
        let scope = self.scope();

        if scope.variables.contains_key(name) {
            return Err(LogicalError::VariableAlreadyExistsInScope(
                name.to_string(),
                span,
            ));
        }

        scope
            .variables
            .insert(name.to_string(), (span, access_modifier, ty));

        Ok(())
    }

    pub fn get_function(&self, name: impl Into<String>) -> Option<(Span, Type, Vec<TypedIdent>)> {
        let name = name.into();

        self.scopes
            .iter()
            .rev()
            .find_map(|v| v.functions.get(&name))
            .cloned()
    }

    pub fn get_variable(&self, name: impl Into<String>) -> Option<(Span, AccessModifier, Type)> {
        let name = name.into();

        self.scopes
            .iter()
            .rev()
            .find_map(|v| v.variables.get(&name))
            .cloned()
    }
}
