use std::collections::HashMap;

use crate::common::Value;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Variable {
    pub value: Value,
    pub is_const: bool,
}

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
        value: Value,
        is_const: bool,
    ) {
        let variable_name = variable_name.into();
        if self.variables.contains_key(&variable_name) {
            panic!("Cannot redeclare variable `{}`", variable_name);
        }

        self.variables
            .insert(variable_name, Variable { value, is_const });
    }

    pub fn assign_variable(&mut self, variable_name: impl Into<String>, value: Value) {
        let variable_name = variable_name.into();

        // Try to get the mutable variable from this environment
        let Some(variable) = self.variables.get_mut(&variable_name) else {
            // If it doesn't exists, try assigning it in the parent environment
            if let Some(parent) = &mut self.parent {
                return parent.assign_variable(variable_name, value)
            }

            // Or panic if there's no parent to look for
            panic!("Variable `{}` not found", variable_name);
        };

        if variable.is_const {
            panic!("Cannot assign to constant variable `{}`", variable_name);
        }

        // Check if variables are of the same type
        if std::mem::discriminant(&variable.value) != std::mem::discriminant(&value) {
            panic!("Trying to assign a different type to `{}`", variable_name);
        }

        variable.value = value;
    }

    pub fn get_variable(&self, variable_name: impl Into<String>) -> &Variable {
        let variable_name = variable_name.into();

        // Try to get the variable from this environment
        if let Some(variable) = self.variables.get(&variable_name) {
            return variable;
        };

        // If nothing was found, try to get the variable from the parent environment
        if let Some(parent) = &self.parent {
            return parent.get_variable(variable_name);
        }

        // Trying to access an variable that was not defined
        panic!("Cannot find `{}` in this scope", variable_name);
    }
}