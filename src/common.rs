use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Rem, Sub},
};

use anyhow::{bail, Context};

use crate::core::lexer::Literal;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(v) => write!(f, "{}i", v),
            Self::Float(v) => write!(f, "{}f", v),
            Self::Boolean(v) => write!(f, "{}b", v),
            Self::String(v) => write!(f, "\"{}\"", v),
        }
    }
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Integer(integer) => Self::Integer(integer),
            Literal::Float(float) => Self::Float(float),
            Literal::String(string) => Self::String(string),
            Literal::Boolean(boolean) => Self::Boolean(boolean),
        }
    }
}

impl Value {
    pub fn as_integer(&self) -> Option<i32> {
        match self {
            Value::Integer(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f32> {
        match self {
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s.as_str()),
            _ => None,
        }
    }
}

macro_rules! impl_value_op {
    ($op:ident, $op_name:ident, $error_msg:expr) => {
        impl $op_name for Value {
            type Output = anyhow::Result<Value>;

            fn $op(self, rhs: Self) -> Self::Output {
                match self {
                    Self::Integer(v) => {
                        let rhs = rhs
                            .as_integer()
                            .context(concat!("invalid type: Integer expected for ", $error_msg))?;
                        Ok(Self::Integer(v.$op(rhs)))
                    }
                    Self::Float(v) => {
                        let rhs = rhs
                            .as_float()
                            .context(concat!("invalid type: Float expected for ", $error_msg))?;
                        Ok(Self::Float(v.$op(rhs)))
                    }
                    _ => bail!(concat!(
                        "this type cannot be ",
                        stringify!($op_name),
                        "ed together"
                    )),
                }
            }
        }
    };
}

impl_value_op!(add, Add, "addition");
impl_value_op!(sub, Sub, "subtraction");
impl_value_op!(mul, Mul, "multiplication");
impl_value_op!(div, Div, "division");
impl_value_op!(rem, Rem, "modulus");
