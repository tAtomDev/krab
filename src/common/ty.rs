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

impl PartialEq<Value> for Type {
    fn eq(&self, other: &Value) -> bool {
        matches!(
            (self, other),
            (Self::Int, Value::Integer(_))
                | (Self::Float, Value::Float(_))
                | (Self::Bool, Value::Boolean(_))
                | (Self::String, Value::String(_))
        )
    }
}
