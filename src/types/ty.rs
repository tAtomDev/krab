use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Custom(String),
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match value.as_str() {
            "bool" => Self::Boolean,
            "int" => Self::Integer,
            "float" => Self::Float,
            "string" => Self::String,
            _ => Self::Custom(value),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Integer => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Boolean => write!(f, "bool"),
            Self::String => write!(f, "String"),
            Self::Custom(identifier) => write!(f, "{identifier}"),
        }
    }
}
