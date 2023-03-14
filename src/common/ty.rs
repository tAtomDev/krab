use std::fmt::Display;

use super::Value;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Custom(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::Custom(string) => write!(f, "{}", string.as_str()),
        }
    }
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match value.as_str() {
            "int" => Self::Int,
            "float" => Self::Float,
            "bool" => Self::Bool,
            "string" => Self::String,
            _ => Self::Custom(value),
        }
    }
}

impl Type {
    pub fn identifier(&self) -> String {
        match self {
            Self::Int => "int".to_string(),
            Self::Float => "float".to_string(),
            Self::Bool => "bool".to_string(),
            Self::String => "string".to_string(),
            Self::Custom(string) => string.clone(),
        }
    }
}

impl PartialEq<Value> for Type {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Int, Value::Integer(_)) => true,
            (Self::Float, Value::Float(_)) => true,
            (Self::Bool, Value::Boolean(_)) => true,
            (Self::String, Value::String(_)) => true,
            _ => false,
        }
    }
}
