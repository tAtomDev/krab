use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
};

use crate::{ast::tokens::Literal, error::ValueError};

use super::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Nothing
    Unit,
    /// true/false boolean value
    Boolean(bool),
    /// Integer number
    Integer(i32),
    /// floating-point number value
    Float(f32),
    /// String value
    String(String),
}

impl Value {
    pub const fn ty(&self) -> Type {
        match self {
            Self::Unit => Type::Unit,
            Self::Boolean(_) => Type::Boolean,
            Self::Integer(_) => Type::Integer,
            Self::Float(_) => Type::Float,
            Self::String(_) => Type::String,
        }
    }

    pub const fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Boolean(bool) => Ok(*bool),
            _ => Err(ValueError::BooleanExpected),
        }
    }
}

/// Convert from a Literal AST representation to a valid Value
impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Boolean(x) => Value::Boolean(x),
            Literal::Integer(x) => Value::Integer(x),
            Literal::Float(x) => Value::Float(x),
            Literal::String(x) => Value::String(x),
        }
    }
}

macro_rules! impl_value_op {
    ($op:ident, $op_trait:ident) => {
        impl $op_trait for Value {
            type Output = Result<Value, crate::error::ValueError>;

            fn $op(self, other: Value) -> Self::Output {
                match (self, other) {
                    (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x.$op(y))),
                    (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x.$op(y))),
                    _ => Err(crate::error::ValueError::TypeMismatch),
                }
            }
        }
    };
}

impl Add for Value {
    type Output = Result<Value, ValueError>;

    fn add(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x + y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
            _ => Err(ValueError::TypeMismatch),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, ValueError>;

    fn div(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    Err(ValueError::DivisionByZero)
                } else {
                    Ok(Value::Integer(x / y))
                }
            }
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x / y)),
            _ => Err(ValueError::TypeMismatch),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, ValueError>;

    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(x) => Ok(Self::Integer(-x)),
            Self::Float(x) => Ok(Self::Float(-x)),
            _ => Err(ValueError::TypeMismatch),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, ValueError>;

    fn not(self) -> Self::Output {
        match self {
            Self::Boolean(bool) => Ok(Value::Boolean(!bool)),
            _ => Err(ValueError::BooleanExpected),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Integer(x), Self::Integer(y)) if x > y => Some(Ordering::Greater),
            (Self::Integer(x), Self::Integer(y)) if x < y => Some(Ordering::Less),
            (Self::Integer(x), Self::Integer(y)) if x == y => Some(Ordering::Equal),

            (Self::Float(x), Self::Float(y)) if x > y => Some(Ordering::Greater),
            (Self::Float(x), Self::Float(y)) if x < y => Some(Ordering::Less),
            (Self::Float(x), Self::Float(y)) if x == y => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl_value_op!(sub, Sub);
impl_value_op!(mul, Mul);
impl_value_op!(rem, Rem);
