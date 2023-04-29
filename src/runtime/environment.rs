use crate::{
    ast::Expression,
    error::RuntimeError,
    types::{Ident, Signature, TypedIdent, Value},
};

use super::{variable::AccessModifier, Function, Scope, Variable};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub scopes: Vec<Scope>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::new_empty()],
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn scope(&mut self) -> &mut Scope {
        if self.scopes.is_empty() {
            self.push_scope();
        }

        self.scopes.last_mut().unwrap()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new_empty());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare_function(
        &mut self,
        sig: Signature,
        expression: Expression,
    ) -> Result<(), RuntimeError> {
        let scope = self.scope();
        let function = Function::new(sig, Box::new(expression));

        scope.declare_function(function)
    }

    pub fn get_function_mut(&mut self, name: impl Into<String>) -> Option<&mut Function> {
        let name = name.into();

        self.scopes
            .iter_mut()
            .rev()
            .find_map(|v| v.get_function_mut(&name))
    }

    pub fn get_function(&self, name: impl Into<String>) -> Option<&Function> {
        let name = name.into();

        self.scopes.iter().rev().find_map(|v| v.get_function(&name))
    }

    pub fn declare_variable(
        &mut self,
        access_modifier: AccessModifier,
        typed_ident: TypedIdent,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let scope = self.scope();
        let variable = Variable::new(access_modifier, typed_ident, value);

        scope.declare_variable(variable)
    }

    pub fn assign_variable(&mut self, ident: Ident, value: Value) -> Result<(), RuntimeError> {
        let variable = self
            .get_variable_mut(&ident.name)
            .ok_or(RuntimeError::VariableNotFound(ident.clone(), ident.span))?;

        if variable.ty != value.ty() {
            return Err(RuntimeError::CannotReassignDifferentType(
                ident.clone(),
                ident.span,
            ));
        }

        variable.value = value;

        Ok(())
    }

    pub fn get_variable_mut(&mut self, name: impl Into<String>) -> Option<&mut Variable> {
        let name = name.into();

        self.scopes
            .iter_mut()
            .rev()
            .find_map(|v| v.get_variable_mut(&name))
    }

    pub fn get_variable(&self, name: impl Into<String>) -> Option<&Variable> {
        let name = name.into();

        self.scopes.iter().rev().find_map(|v| v.get_variable(&name))
    }
}
