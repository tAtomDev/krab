use std::collections::HashMap;

use crate::{error::RuntimeError, prelude::Value, types::Ident};

use super::{Function, Variable};

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub functions: HashMap<String, Function>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new_empty()
    }
}

impl Scope {
    pub fn new_empty() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn declare_function(&mut self, function: Function) -> Result<(), RuntimeError> {
        let ident = &function.signature.identifier;
        if self.functions.contains_key(&ident.name) {
            return Err(RuntimeError::CannotRedeclareFunction(
                ident.clone(),
                ident.span,
            ));
        }

        self.functions.insert(ident.name.clone(), function);

        Ok(())
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }

    pub fn get_function_mut(&mut self, name: &str) -> Option<&mut Function> {
        self.functions.get_mut(name)
    }

    pub fn declare_variable(&mut self, variable: Variable) -> Result<(), RuntimeError> {
        if self.variables.contains_key(&variable.ident.name) {
            return Err(RuntimeError::CannotRedeclareVariable(
                variable.ident.clone(),
                variable.ident.span,
            ));
        }

        self.variables.insert(variable.ident.name.clone(), variable);

        Ok(())
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

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        self.variables.get(name)
    }

    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut Variable> {
        self.variables.get_mut(name)
    }
}
