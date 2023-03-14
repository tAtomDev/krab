use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token {
    Keyword(Keyword),
    Literal(Literal),
    Identifier(String),
    Operator(Operator),
    Punctuation(Punctuation),
    Eof,
    Invalid,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(x) => write!(f, "{x}"),
            Token::Literal(x) => write!(f, "{x}"),
            Token::Identifier(x) => write!(f, "{x}"),
            Token::Operator(x) => write!(f, "{x}"),
            Token::Punctuation(x) => write!(f, "{x}"),
            Token::Eof => write!(f, "EOF"),
            Token::Invalid => write!(f, "InvalidToken"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(x) => write!(f, "{x}"),
            Self::Float(x) => write!(f, "{x}"),
            Self::String(x) => write!(f, "\"{x}\""),
            Self::Boolean(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Keyword {
    Let,
    Const,
    If,
    Else,
    While,
    Function,
    Return,
    Break,
    Continue,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
            Self::Const => write!(f, "const"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
            Self::Function => write!(f, "fn"),
            Self::Return => write!(f, "return"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
        }
    }
}

pub const SEMICOLON_TOKEN: Token = Token::Punctuation(Punctuation::Semicolon);

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    And,
    Or,
    Not,
    Assignment,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    Arrow,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Power => write!(f, "^"),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::Not => write!(f, "!"),
            Self::Assignment => write!(f, "="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::LessOrEqual => write!(f, "<="),
            Self::GreaterOrEqual => write!(f, ">="),
            Self::Arrow => write!(f, "->"),
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
                | Operator::Power
        )
    }

    pub const fn is_unary(self) -> bool {
        matches!(self, Operator::Subtract | Operator::Not)
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
            Operator::Power => 7,
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
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Punctuation {
    OpenParenthesis,  // (
    CloseParenthesis, // )
    OpenBrace,        // {
    CloseBrace,       // }
    OpenBrackets,     // [
    CloseBrackets,    // ]
    Semicolon,        // ;
    Comma,            // ,
    Colon,            // :
    Dot,              // .
    QuestionMark,     // ?
}

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
        }
    }
}
