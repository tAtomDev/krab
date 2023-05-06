use std::{fmt::Display, ops::Deref};

use crate::types::Type;

use super::Span;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub const EOF: Token = Token {
        span: Span::EOF,
        kind: TokenKind::Eof,
    };

    pub const fn new(span: Span, kind: TokenKind) -> Token {
        Token { span, kind }
    }

    pub fn is_semicolon(&self) -> bool {
        self.kind == TokenKind::Punctuation(Punctuation::Semicolon)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Deref for Token {
    type Target = TokenKind;
    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TokenKind {
    Literal(Literal),
    Operator(Operator),
    Keyword(Keyword),
    Punctuation(Punctuation),
    Identifier(String),
    Eof,
}

impl PartialEq<TokenKind> for Token {
    fn eq(&self, other: &TokenKind) -> bool {
        &self.kind == other
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(v) => write!(f, "{v}"),
            Self::Operator(v) => write!(f, "{v}"),
            Self::Keyword(v) => write!(f, "{v}"),
            Self::Punctuation(v) => write!(f, "{v}"),
            Self::Identifier(v) => write!(f, "{v}"),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Literal {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    String(String),
}

impl Literal {
    pub const fn ty(&self) -> Type {
        match self {
            Self::Boolean(_) => Type::Boolean,
            Self::Integer(_) => Type::Integer,
            Self::Float(_) => Type::Float,
            Self::String(_) => Type::String,
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(v) => write!(f, "{v}"),
            Self::Integer(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v:.2}"),
            Self::String(v) => write!(f, "\"{v}\""),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Operator {
    ///  +=
    AddAssign,
    /// -=
    SubtractAssign,
    /// *=
    MultiplyAssign,
    /// /=
    DivideAssign,
    /// %=
    ModuloAssign,
    /// !=
    NotEqual,
    /// !
    Not,
    /// +
    Add,
    /// -
    Subtract,
    /// *
    Multiply,
    /// /
    Divide,
    /// %
    Modulo,
    /// ==
    Equal,
    /// =
    Assignment,
    /// &&
    And,
    /// ||
    Or,
    /// >=
    GreaterOrEqual,
    /// >
    Greater,
    /// <=
    LessOrEqual,
    /// <
    Less,
}

pub const OPERATORS: [Operator; 20] = [
    Operator::AddAssign,
    Operator::SubtractAssign,
    Operator::MultiplyAssign,
    Operator::DivideAssign,
    Operator::ModuloAssign,
    Operator::NotEqual,
    Operator::Not,
    Operator::Add,
    Operator::Subtract,
    Operator::Multiply,
    Operator::Divide,
    Operator::Modulo,
    Operator::Equal,
    Operator::Assignment,
    Operator::And,
    Operator::Or,
    Operator::GreaterOrEqual,
    Operator::Greater,
    Operator::LessOrEqual,
    Operator::Less,
];

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddAssign => write!(f, "+="),
            Self::SubtractAssign => write!(f, "-="),
            Self::MultiplyAssign => write!(f, "*="),
            Self::DivideAssign => write!(f, "/="),
            Self::ModuloAssign => write!(f, "%="),
            Self::NotEqual => write!(f, "!="),
            Self::Not => write!(f, "!"),
            Self::Add => write!(f, "+"),
            Self::Multiply => write!(f, "*"),
            Self::Subtract => write!(f, "-"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Equal => write!(f, "=="),
            Self::Assignment => write!(f, "="),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::GreaterOrEqual => write!(f, ">="),
            Self::Greater => write!(f, ">"),
            Self::LessOrEqual => write!(f, "<="),
            Self::Less => write!(f, "<"),
        }
    }
}

impl Operator {
    pub const fn is_binary(self) -> bool {
        matches!(
            self,
            Operator::Add
                | Operator::Subtract
                | Operator::Multiply
                | Operator::Divide
                | Operator::Modulo
        )
    }

    pub const fn is_unary(self) -> bool {
        matches!(self, Operator::Subtract | Operator::Not)
    }

    pub const fn is_logical(self) -> bool {
        matches!(
            self,
            Operator::Equal
                | Operator::NotEqual
                | Operator::Not
                | Operator::Less
                | Operator::LessOrEqual
                | Operator::Greater
                | Operator::GreaterOrEqual
        )
    }

    pub const fn precedence(self) -> u8 {
        match self {
            Operator::Or => 1,
            Operator::And => 2,
            Operator::Equal | Operator::NotEqual => 3,
            Operator::Less
            | Operator::LessOrEqual
            | Operator::Greater
            | Operator::GreaterOrEqual => 4,
            Operator::Add | Operator::Subtract => 5,
            Operator::Multiply | Operator::Divide | Operator::Modulo => 6,
            Operator::Not => 8,
            _ => 0,
        }
    }

    pub const fn associativity(&self) -> Associativity {
        if self.is_binary() {
            Associativity::Left
        } else {
            Associativity::Right
        }
    }

    pub const fn parse_assignment_op(&self) -> Option<Operator> {
        match self {
            Self::AddAssign => Some(Operator::Add),
            Self::SubtractAssign => Some(Operator::Subtract),
            Self::MultiplyAssign => Some(Operator::Multiply),
            Self::DivideAssign => Some(Operator::Divide),
            Self::ModuloAssign => Some(Operator::Modulo),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Keyword {
    /// let
    Let,
    /// const
    Const,
    /// if
    If,
    /// else
    Else,
    /// struct
    Struct,
    /// while
    While,
    /// fn
    Function,
    /// return
    Return,
}

pub const KEYWORDS: [Keyword; 8] = [
    Keyword::Let,
    Keyword::Const,
    Keyword::If,
    Keyword::Else,
    Keyword::Struct,
    Keyword::While,
    Keyword::Function,
    Keyword::Return,
];

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
            Self::Const => write!(f, "const"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Struct => write!(f, "struct"),
            Self::While => write!(f, "while"),
            Self::Function => write!(f, "fn"),
            Self::Return => write!(f, "return"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Punctuation {
    /// (
    OpenParenthesis,
    /// )
    CloseParenthesis,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// [
    OpenBrackets,
    /// ]
    CloseBrackets,
    /// ;
    Semicolon,
    /// ,
    Comma,
    /// :
    Colon,
    /// .
    Dot,
    /// ?
    QuestionMark,
    /// ->
    Arrow,
}

pub const PUNCTUATIONS: [Punctuation; 12] = [
    Punctuation::OpenParenthesis,
    Punctuation::CloseParenthesis,
    Punctuation::OpenBrace,
    Punctuation::CloseBrace,
    Punctuation::OpenBrackets,
    Punctuation::CloseBrackets,
    Punctuation::Semicolon,
    Punctuation::Comma,
    Punctuation::Colon,
    Punctuation::Dot,
    Punctuation::QuestionMark,
    Punctuation::Arrow,
];

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpenParenthesis => write!(f, "("),
            Self::CloseParenthesis => write!(f, ")"),
            Self::OpenBrace => write!(f, "{{"),
            Self::CloseBrace => write!(f, "}}"),
            Self::OpenBrackets => write!(f, "["),
            Self::CloseBrackets => write!(f, "]"),
            Self::Semicolon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::Dot => write!(f, "."),
            Self::QuestionMark => write!(f, "?"),
            Self::Arrow => write!(f, "->"),
        }
    }
}

// Compare TokenKind to Punctuation
impl PartialEq<Punctuation> for TokenKind {
    fn eq(&self, other: &Punctuation) -> bool {
        match self {
            Self::Punctuation(punctuation) => punctuation == other,
            _ => false,
        }
    }
}

// Compare Punctuation to String
impl PartialEq<String> for Punctuation {
    fn eq(&self, other: &String) -> bool {
        &self.to_string() == other
    }
}
