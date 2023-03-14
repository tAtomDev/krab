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
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
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
        matches!(self, Operator::Add | Operator::Subtract | Operator::Not)
    }

    pub const fn is_logical(self) -> bool {
        matches!(self, Operator::And | Operator::Or)
    }

    pub const fn logical_precedence(self) -> u8 {
        match self {
            Operator::And => 3,
            Operator::Or => 2,
            _ => 0,
        }
    }

    pub const fn precedence(self) -> u8 {
        match self {
            Operator::Add | Operator::Subtract => 1,
            Operator::Multiply | Operator::Divide | Operator::Modulo => 2,
            Operator::Equal | Operator::NotEqual => 3,
            Operator::Less
            | Operator::LessOrEqual
            | Operator::Greater
            | Operator::GreaterOrEqual => 4,
            _ => 5,
        }
    }

    pub const fn associativity(&self) -> Associativity {
        if self.is_binary() {
            Associativity::Left
        } else {
            Associativity::None
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Associativity {
    Left,
    None,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Punctuation {
    OpenParenthesis,  // (
    CloseParenthesis, // )
    OpenBrace,        // {
    CloseBrace,       // }
    OpenBrackets,     // [
    CloseBrackets,    // ]
    Semicolon,        //;
    Comma,            //,
    Colon,            //:
    Dot,              //.
    QuestionMark,     //?
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
