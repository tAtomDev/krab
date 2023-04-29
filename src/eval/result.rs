use crate::{ast::Span, error::RuntimeError, prelude::Value, types::ControlFlow};

#[derive(Debug, Clone, PartialEq)]
pub enum EvalResult {
    Value(Value),
    ControlFlow(ControlFlow, Value),
}

impl From<Value> for EvalResult {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}

impl EvalResult {
    pub fn try_parse_value(self, span: Span) -> Result<Value, RuntimeError> {
        match self {
            Self::Value(value) => Ok(value),
            Self::ControlFlow(..) => Err(RuntimeError::IllegalReturnStatement(span)),
        }
    }
}
