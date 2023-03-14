use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

use anyhow::{bail, Context};

use super::tokens::Literal;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Nothing,
    Integer(i32),
    Float(f32),
    Boolean(bool),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nothing => write!(f, "()"),
            Self::Integer(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{:?}", v),
            Self::Boolean(v) => write!(f, "{}", v),
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
}

impl Neg for Value {
    type Output = anyhow::Result<Value>;

    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(v) => Ok(Self::Integer(-v)),
            Self::Float(v) => Ok(Self::Float(-v)),
            _ => bail!("Cannot negate type: {}", self),
        }
    }
}

macro_rules! impl_value_op {
    ($op:ident, $op_name:ident, $error_msg:expr) => {
        impl $op_name for Value {
            type Output = anyhow::Result<Value>;

            fn $op(self, rhs: Self) -> Self::Output {
                if self == Self::Nothing {
                    return Ok(rhs);
                }

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
                        "these types cannot be ",
                        stringify!($op),
                        "ed together"
                    )),
                }
            }
        }
    };
}

impl Add for Value {
    type Output = anyhow::Result<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        if self == Self::Nothing {
            return Ok(rhs);
        }

        match self {
            Self::Integer(v) => {
                let rhs = rhs
                    .as_integer()
                    .context(concat!("invalid type: Integer expected for addition"))?;
                Ok(Self::Integer(v.add(rhs)))
            }
            Self::Float(v) => {
                let rhs = rhs
                    .as_float()
                    .context(concat!("invalid type: Float expected for addition"))?;
                Ok(Self::Float(v.add(rhs)))
            }
            Self::String(v) => {
                let rhs = rhs.to_string();
                Ok(Self::String(format!("{v}{rhs}")))
            }
            _ => bail!("these types cannot be added"),
        }
    }
}

impl_value_op!(sub, Sub, "subtraction");
impl_value_op!(mul, Mul, "multiplication");
impl_value_op!(div, Div, "division");
impl_value_op!(rem, Rem, "modulus");
