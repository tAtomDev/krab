use std::fmt::Display;

use super::Value;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Tuple(Vec<Type>),
    Custom(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::Tuple(vec) => write!(
                f,
                "({})",
                vec.iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Custom(string) => write!(f, "{}", string.as_str()),
        }
    }
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        let trimmed_value = value.trim();
        if trimmed_value.starts_with('(') && trimmed_value.ends_with(')') {
            let inner = trimmed_value[1..trimmed_value.len() - 1].trim();
            if inner.is_empty() {
                return Self::Tuple(vec![]);
            }

            let types = inner
                .split(',')
                .map(|s| s.trim().to_string().into())
                .collect::<Vec<Type>>();
            return Self::Tuple(types);
        }

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
            Self::Tuple(types) => {
                let identifiers = types
                    .iter()
                    .map(|ty| ty.identifier())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", identifiers)
            }
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
            (Self::Tuple(types), Value::Tuple(values)) => {
                if types.len() != values.len() {
                    return false;
                }
                types.iter().zip(values.iter()).all(|(t, v)| t.eq(v))
            }
            _ => false,
        }
    }
}
