use std::collections::HashMap;

use crate::common::{Value, Type};

use super::RuntimeError;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Variable {
    pub ty: Type,
    pub value: Value,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub parent: Option<Box<Environment>>,
    pub variables: HashMap<String, Variable>,
}

impl Environment {
    pub fn new(parent: Option<Box<Environment>>) -> Self {
        Self {
            parent,
            variables: HashMap::new(),
        }
    }

    pub fn declare_variable(
        &mut self,
        variable_name: impl Into<String>,
        ty: Type,
        value: Value,
        is_const: bool,
    ) -> Result<(), RuntimeError> {
        let variable_name = variable_name.into();
        if self.variables.contains_key(&variable_name) {
            return Err(RuntimeError::CannotRedeclareVariable(variable_name));
        }

        self.variables
            .insert(variable_name, Variable { ty, value, is_const });

        Ok(())
    }

    pub fn assign_variable(
        &mut self,
        variable_name: impl Into<String>,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let variable_name = variable_name.into();

        // Try to get the mutable variable from this environment
        let Some(variable) = self.variables.get_mut(&variable_name) else {
            // If it doesn't exists, try assigning it in the parent environment
            if let Some(parent) = &mut self.parent {
                return parent.assign_variable(variable_name, value)
            }

            return Err(RuntimeError::VariableNotFound(variable_name));
        };

        if variable.is_const {
            return Err(RuntimeError::CannotReassignConstVariable(variable_name));
        }

        // Check if variables are of the same type
        let value_ty = value.ty().map_err(|_| RuntimeError::UnknownType)?;
        if variable.ty != value_ty {
            return Err(RuntimeError::CannotReassignDifferentType(variable_name));
        }

        variable.value = value;
        Ok(())
    }

    pub fn get_variable(
        &self,
        variable_name: impl Into<String>,
    ) -> Result<&Variable, RuntimeError> {
        let variable_name = variable_name.into();

        // Try to get the variable from this environment
        if let Some(variable) = self.variables.get(&variable_name) {
            return Ok(variable);
        };

        // If nothing was found, try to get the variable from the parent environment
        if let Some(parent) = &self.parent {
            return parent.get_variable(variable_name);
        }

        // Trying to access an variable that was not defined
        Err(RuntimeError::VariableNotFound(variable_name))
    }
}
