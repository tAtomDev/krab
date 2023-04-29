use std::{error::Error, fmt::Display};

use crate::{
    ast::{
        tokens::{Operator, Token},
        Span,
    },
    types::{Ident, Type, TypedIdent},
};

#[derive(Debug)]
pub enum LexicalError {
    UnexpectedCharacter(char, Span),

    InvalidIntegerLiteral(Span),
    InvalidFloatLiteral(Span),
    MissingAtEndOfStringLiteral(char, Span),

    InvalidIdentifierCharacter(char, Span),
    InvalidIdentifier(Span),

    UnexpectedEOF(Span),
}

impl Error for LexicalError {}

impl Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedCharacter(char, span) => {
                write!(f, "Unexpected character '{char}' at {span}")
            }
            Self::InvalidIntegerLiteral(span) => write!(f, "Invalid integer literal at {span}"),
            Self::InvalidFloatLiteral(span) => write!(f, "Invalid float literal at {span}"),
            Self::MissingAtEndOfStringLiteral(quote, span) => {
                write!(f, "Missing `{quote}` at end of string literal at {span}")
            }
            Self::InvalidIdentifierCharacter(ch, span) => {
                write!(f, "Invalid character `{ch}` at identifier at {span}")
            }
            Self::InvalidIdentifier(span) => write!(f, "Invalid identifier at {span}"),
            Self::UnexpectedEOF(span) => write!(f, "Unexpected EOF at {span}"),
        }
    }
}

#[derive(Debug)]
pub enum LogicalError {
    FunctionAlreadyExistsInScope(String, Span),
    VariableAlreadyExistsInScope(String, Span),

    VariableDoesNotExists(String, Span),
    FunctionDoesNotExists(String, Span),

    CannotAssignTypeToVariable(Type, Type, String, Span),
    ExpectedReturnTypeButFound(Type, Type, Span),
    IncorrectArgumentType(TypedIdent, Span),
    CannotAssignToConstant(String, Span),

    IncorrectFunctionArgumentsAmount(String, usize, usize, Span),
}

impl Error for LogicalError {}
impl Display for LogicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalError::FunctionAlreadyExistsInScope(name, span) => {
                write!(f, "Function `{name}` already exists in scope at {span}")
            }
            LogicalError::VariableAlreadyExistsInScope(name, span) => {
                write!(f, "Variable `{name}` already exists in scope at {span}")
            }
            LogicalError::VariableDoesNotExists(name, span) => {
                write!(f, "Variable `{name}` does not exists at {span}")
            }
            LogicalError::FunctionDoesNotExists(name, span) => {
                write!(f, "Function `{name}` does not exists at {span}")
            }
            LogicalError::CannotAssignTypeToVariable(a, b, identifier, span) => {
                write!(f, "Cannot assign type \"{a}\" to variable of type \"{b}\" `{identifier}` at {span}")
            }
            LogicalError::ExpectedReturnTypeButFound(expected, found, span) => write!(
                f,
                "Expected return type `{expected}` but found `{found}` at {span}"
            ),
            LogicalError::IncorrectArgumentType(typed_ident, span) => write!(
                f,
                "Argument `{}` is a `{}` type at {span}",
                typed_ident.ident, typed_ident.ty
            ),
            LogicalError::CannotAssignToConstant(identifier, span) => write!(
                f,
                "Cannot assign a different value to constant `{identifier}` at {span}"
            ),
            LogicalError::IncorrectFunctionArgumentsAmount(
                identifier,
                expected,
                provided,
                span,
            ) => {
                write!(f, "Expected {expected} arguments at function `{identifier}`, but provided {provided} at {span}")
            }
        }
    }
}

#[derive(Debug)]
pub enum SyntaxError {
    LogicalError(LogicalError),
    SemicolonExpected(Span),
    Expected(String, Span),
    ExpectedButFound(String, String, Span),
    TryingToParseUnexpectedToken(Token, Span),
    ExpectedOperatorButFound(Token, Span),

    ExpectedAValidIdentifier(Span),
    MustAssignToVariable(String, Span),

    InvalidType(Span),
    IncorrectType(Type, Type, Span),
    UnknownType(Span),

    IncorrectExpressionBranchTypes(Vec<Type>, Span),
}

impl Error for SyntaxError {}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LogicalError(error) => write!(f, "{error}"),
            Self::SemicolonExpected(span) => write!(f, "Semicolon `;` expected at {span}"),
            Self::Expected(expected, span) => write!(f, "Expected '{expected}' at {span}"),
            Self::ExpectedButFound(expected, found, span) => {
                write!(f, "Expected '{expected}' but found '{found}' at {span}")
            }

            Self::TryingToParseUnexpectedToken(token, span) => {
                write!(f, "Trying to parse unexpected token '{token}' at {span}")
            }

            Self::ExpectedOperatorButFound(token, span) => {
                write!(f, "Expected a operator but found '{token}' at {span}")
            }

            Self::ExpectedAValidIdentifier(span) => {
                write!(f, "Expected a valid identifier at {span}")
            }

            Self::MustAssignToVariable(identifier, span) => {
                write!(f, "You must assign a value to `{identifier}` at {span}")
            }

            Self::InvalidType(span) => write!(f, "Invalid type at {span}"),
            Self::IncorrectType(ty_a, ty_b, span) => {
                write!(f, "Trying to assign `{ty_a}` to `{ty_b}` at {span}")
            }
            Self::UnknownType(span) => write!(f, "Unknown type at {span}"),
            Self::IncorrectExpressionBranchTypes(types, span) => {
                write!(
                    f,
                    "Expression returning different types ({}) at {span}",
                    types
                        .iter()
                        .map(|ty| format!("`{ty}`"))
                        .collect::<Vec<_>>()
                        .join(" and ")
                )
            }
        }
    }
}

#[derive(Debug)]
pub enum ValueError {
    TypeMismatch,
    DivisionByZero,
    BooleanExpected,
}

impl Error for ValueError {}
impl Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueError::TypeMismatch => write!(f, "Type mismatch"),
            ValueError::DivisionByZero => write!(f, "Division by zero"),
            ValueError::BooleanExpected => write!(f, "Boolean expected"),
        }
    }
}

impl From<LogicalError> for SyntaxError {
    fn from(value: LogicalError) -> Self {
        Self::LogicalError(value)
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    ValueError(ValueError, Span),
    VariableNotFound(Ident, Span),
    FunctionNotFound(Ident, Span),

    InvalidUnaryOperator(Operator, Span),
    InvalidBinaryOperator(Operator, Span),

    CannotRedeclareFunction(Ident, Span),
    CannotRedeclareVariable(Ident, Span),
    CannotReassignDifferentType(Ident, Span),

    IncorrectAmountOfArguments(usize, usize, Span),
    IncorrectArgumentType(TypedIdent, Span),
    IllegalReturnStatement(Span),
}

impl Error for RuntimeError {}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::ValueError(error, span) => write!(f, "{error} at {span}"),
            RuntimeError::VariableNotFound(ident, span) => {
                write!(f, "Variable `{}` not found at {}", ident.name, span)
            }
            RuntimeError::FunctionNotFound(ident, span) => {
                write!(f, "Function `{}` not found at {}", ident.name, span)
            }
            RuntimeError::InvalidUnaryOperator(operator, span) => {
                write!(f, "Invalid unary operator `{operator}` at {span}")
            }
            RuntimeError::InvalidBinaryOperator(operator, span) => {
                write!(f, "Invalid binary operator `{operator}` at {span}")
            }
            RuntimeError::CannotRedeclareFunction(ident, span) => {
                write!(f, "Cannot redeclare function `{}` at {span}", ident.name)
            }
            RuntimeError::CannotRedeclareVariable(ident, span) => {
                write!(f, "Cannot redeclare variable `{}` at {span}", ident.name)
            }
            RuntimeError::CannotReassignDifferentType(ident, span) => {
                write!(
                    f,
                    "Cannot assign a value of a different type to variable `{}` at {span}",
                    ident.name
                )
            }
            RuntimeError::IncorrectAmountOfArguments(expected, provided, span) => write!(
                f,
                "expected {expected} arguments, but provided {provided} at {span}"
            ),
            RuntimeError::IncorrectArgumentType(typed_ident, span) => write!(
                f,
                "Argument `{}` is a `{}` type at {span}",
                typed_ident.ident, typed_ident.ty
            ),
            RuntimeError::IllegalReturnStatement(span) => {
                write!(f, "Illegal return statement at {span}")
            }
        }
    }
}
