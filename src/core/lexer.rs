use crate::{
    common::{tokens::*, Span},
    util,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexicalError {
    #[error("missing '{0}' at end of string literal at {1}")]
    MissingAtEndOfStringLiteral(char, Span),
    #[error("invalid floating point literal at {0}")]
    InvalidFloatLiteral(Span),
    #[error("invalid or unexpected identifier character '{0}' at {1}")]
    InvalidIdentifierCharacter(char, Span),
    #[error("invalid identifier at {0}")]
    InvalidIdentifier(Span),
    #[error("invalid or illegal literal value at {0}")]
    InvalidLiteral(Span),
}

pub struct Lexer {
    source: Vec<char>,
    position: usize,
    char_pos: usize,
    loc: usize,
    current_span_loc: usize,
    waiting_for_identifier: bool,
}

impl Lexer {
    pub fn new(code: impl Into<String>) -> Self {
        let code = code.into();
        Self {
            source: code.chars().collect(),
            position: 0,
            char_pos: 0,
            loc: 1,
            current_span_loc: 0,
            waiting_for_identifier: false,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexicalError> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_at_end() {
            let token = self.scan_token()?;

            if token.kind == TokenKind::Invalid {
                continue;
            }

            tokens.push(token);
        }

        if !tokens.ends_with(&[Token::EOF]) {
            tokens.push(Token::EOF);
        }

        Ok(tokens)
    }

    fn make_span(&self) -> Span {
        Span::new(self.char_pos, self.char_pos, self.current_span_loc)
    }

    fn advance_pos(&mut self) {
        if self.current_char() == '\n' {
            self.char_pos = 0;
            self.loc += 1;
        }

        self.position += 1;
        self.char_pos += 1;
    }

    fn advance_char(&mut self) -> char {
        self.advance_pos();

        *self.source.get(self.position - 1).unwrap()
    }

    fn current_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            *self.source.get(self.position).unwrap()
        }
    }

    fn check_char_and_advance(&mut self, char: char) -> bool {
        if self.current_char() == char {
            self.advance_pos();

            return true;
        }

        false
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<Token, LexicalError> {
        if self.is_at_end() {
            return Ok(Token::EOF);
        }

        let start = self.char_pos;
        let loc = self.loc;

        self.current_span_loc = loc;

        let c = self.advance_char();

        if c == '/' && self.check_char_and_advance('/') {
            // This is a single-line comment, so we skip until the end of the line.
            while !self.is_at_end() && self.current_char() != '\n' {
                self.advance_char();
            }

            // We continue scanning for the next token.
            return self.scan_token();
        }

        let token_kind = match c {
            '(' => TokenKind::Punctuation(Punctuation::OpenParenthesis),
            ')' => TokenKind::Punctuation(Punctuation::CloseParenthesis),
            '[' => TokenKind::Punctuation(Punctuation::OpenBrackets),
            ']' => TokenKind::Punctuation(Punctuation::CloseBrackets),
            '{' => TokenKind::Punctuation(Punctuation::OpenBrace),
            '}' => TokenKind::Punctuation(Punctuation::CloseBrace),
            ';' => TokenKind::Punctuation(Punctuation::Semicolon),
            ':' => TokenKind::Punctuation(Punctuation::Colon),
            ',' => TokenKind::Punctuation(Punctuation::Comma),
            '.' => TokenKind::Punctuation(Punctuation::Dot),
            '?' => TokenKind::Punctuation(Punctuation::QuestionMark),

            '+' => TokenKind::Operator(Operator::Add),
            '-' => {
                if self.check_char_and_advance('>') {
                    TokenKind::Operator(Operator::Arrow)
                } else {
                    TokenKind::Operator(Operator::Subtract)
                }
            }
            '*' => TokenKind::Operator(Operator::Multiply),
            '/' => {
                if self.check_char_and_advance('/') {
                    // Ignore single-line comments
                    while !self.is_at_end() && self.current_char() != '\n' {
                        self.advance_char();
                    }

                    TokenKind::Invalid
                } else if self.check_char_and_advance('*') {
                    // Ignore multi-line comments
                    let mut comment_level = 1;
                    while comment_level > 0 && !self.is_at_end() {
                        let c1 = self.advance_char();
                        let c2 = self.current_char();
                        if c1 == '*' && c2 == '/' {
                            self.advance_char();
                            comment_level -= 1;
                        } else if c1 == '/' && c2 == '*' {
                            self.advance_char();
                            comment_level += 1;
                        }
                    }

                    TokenKind::Invalid
                } else {
                    TokenKind::Operator(Operator::Divide)
                }
            }
            '%' => TokenKind::Operator(Operator::Modulo),
            '^' => TokenKind::Operator(Operator::Power),
            '!' => {
                if self.check_char_and_advance('=') {
                    TokenKind::Operator(Operator::NotEqual)
                } else {
                    TokenKind::Operator(Operator::Not)
                }
            }

            '=' => {
                if self.check_char_and_advance('=') {
                    TokenKind::Operator(Operator::Equal)
                } else {
                    TokenKind::Operator(Operator::Assignment)
                }
            }
            '>' => {
                if self.check_char_and_advance('=') {
                    TokenKind::Operator(Operator::GreaterOrEqual)
                } else {
                    TokenKind::Operator(Operator::Greater)
                }
            }
            '<' => {
                if self.check_char_and_advance('=') {
                    TokenKind::Operator(Operator::LessOrEqual)
                } else {
                    TokenKind::Operator(Operator::Less)
                }
            }

            c if c == '&' && self.check_char_and_advance('&') => TokenKind::Operator(Operator::And),
            c if c == '|' && self.check_char_and_advance('|') => TokenKind::Operator(Operator::Or),

            '"' | '\'' => self.string(c)?,
            c if c.is_ascii_digit() => self.number(c)?,
            c if c.is_alphabetic() || c == '_' => self.keyword_or_identifier(c)?,
            _ => TokenKind::Invalid,
        };

        if let TokenKind::Keyword(keyword) = &token_kind {
            match keyword {
                Keyword::Function | Keyword::Let => self.waiting_for_identifier = true,
                _ => {}
            }
        }

        let end = self.char_pos;
        let span = Span::new(start, end, loc);

        let token = Token::new(token_kind, span);

        Ok(token)
    }

    fn string(&mut self, quote: char) -> Result<TokenKind, LexicalError> {
        let mut string = String::new();
        let mut failed = true;

        while !self.is_at_end() {
            if self.current_char() == quote {
                failed = false;
                self.advance_char();
                break;
            }

            string.push(self.advance_char());
        }

        if failed {
            return Err(LexicalError::MissingAtEndOfStringLiteral(
                quote,
                self.make_span(),
            ));
        }

        Ok(TokenKind::Literal(Literal::String(string)))
    }

    fn number(&mut self, first_number: char) -> Result<TokenKind, LexicalError> {
        let mut string = String::new();
        let mut is_float = false;

        string.push(first_number);

        while !self.is_at_end() {
            if self.current_char() == '.' {
                if is_float {
                    return Err(LexicalError::InvalidFloatLiteral(self.make_span()));
                }

                is_float = true;
            } else if !self.current_char().is_ascii_digit() {
                break;
            }

            string.push(self.advance_char());
        }

        if is_float {
            Ok(TokenKind::Literal(Literal::Float(
                string
                    .parse::<f32>()
                    .map_err(|_| LexicalError::InvalidLiteral(self.make_span()))?,
            )))
        } else {
            Ok(TokenKind::Literal(Literal::Integer(
                string
                    .parse::<i32>()
                    .map_err(|_| LexicalError::InvalidLiteral(self.make_span()))?,
            )))
        }
    }

    fn keyword_or_identifier(&mut self, char: char) -> Result<TokenKind, LexicalError> {
        let mut string = String::new();

        string.push(char);

        while !self.is_at_end() {
            let current_char = self.current_char();
            if util::can_lexer_skip(current_char) {
                break;
            }

            string.push(self.advance_char());
        }

        let token_kind = match string.as_str() {
            "let" => TokenKind::Keyword(Keyword::Let),
            "const" => TokenKind::Keyword(Keyword::Const),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "while" => TokenKind::Keyword(Keyword::While),
            "fn" => TokenKind::Keyword(Keyword::Function),
            "return" => TokenKind::Keyword(Keyword::Return),
            "break" => TokenKind::Keyword(Keyword::Break),
            "continue" => TokenKind::Keyword(Keyword::Continue),

            "true" => TokenKind::Literal(Literal::Boolean(true)),
            "false" => TokenKind::Literal(Literal::Boolean(false)),

            _ => self.identifier(string)?,
        };

        Ok(token_kind)
    }

    fn identifier(&mut self, mut string: String) -> Result<TokenKind, LexicalError> {
        while !self.is_at_end() {
            let current_char = self.current_char();

            if current_char == ';' || current_char != '_' && util::can_lexer_skip(current_char) {
                break;
            }

            if !current_char.is_ascii_whitespace()
                && current_char != '_'
                && !current_char.is_ascii_alphanumeric()
            {
                return Err(LexicalError::InvalidIdentifierCharacter(
                    current_char,
                    self.make_span(),
                ));
            }

            string.push(self.advance_char());
        }

        if string.is_empty() {
            return Err(LexicalError::InvalidIdentifier(self.make_span()));
        }

        Ok(TokenKind::Identifier(string))
    }
}

#[cfg(test)]
mod tests {
    use super::Keyword::*;
    use super::Operator::*;
    use super::Punctuation::*;
    use super::TokenKind::*;
    use super::*;

    #[test]
    fn hello_world() {
        let code = r#"
        fn main() {
            let x = "Hello, world!";
            print(x);
        }
        "#;

        let lex = Lexer::new(code).lex().unwrap();

        assert_eq!(
            lex,
            vec![
                Keyword(Function),
                Identifier("main".into()),
                Punctuation(OpenParenthesis),
                Punctuation(CloseParenthesis),
                Punctuation(OpenBrace),
                Keyword(Let),
                Identifier("x".into()),
                Operator(Assignment),
                Literal(super::Literal::String("Hello, world!".into())),
                Punctuation(Semicolon),
                Identifier("print".into()),
                Punctuation(OpenParenthesis),
                Identifier("x".into()),
                Punctuation(CloseParenthesis),
                Punctuation(Semicolon),
                Punctuation(CloseBrace),
                Eof,
            ]
        );
    }

    #[test]
    fn math_operators() {
        let code = r#"
            let a = 10 + 5 - 3.5 * 2 / 4;
            let b = ((a + 1) * 2 - 4) % 3;
        "#;

        let lex = Lexer::new(code).lex().unwrap();

        assert_eq!(
            lex,
            vec![
                Keyword(Let),
                Identifier("a".into()),
                Operator(Assignment),
                Literal(super::Literal::Integer(10)),
                Operator(Add),
                Literal(super::Literal::Integer(5)),
                Operator(Subtract),
                Literal(super::Literal::Float(3.5)),
                Operator(Multiply),
                Literal(super::Literal::Integer(2)),
                Operator(Divide),
                Literal(super::Literal::Integer(4)),
                Punctuation(Semicolon),
                Keyword(Let),
                Identifier("b".into()),
                Operator(Assignment),
                Punctuation(OpenParenthesis),
                Punctuation(OpenParenthesis),
                Identifier("a".into()),
                Operator(Add),
                Literal(super::Literal::Integer(1)),
                Punctuation(CloseParenthesis),
                Operator(Multiply),
                Literal(super::Literal::Integer(2)),
                Operator(Subtract),
                Literal(super::Literal::Integer(4)),
                Punctuation(CloseParenthesis),
                Operator(Modulo),
                Literal(super::Literal::Integer(3)),
                Punctuation(Semicolon),
                Eof,
            ]
        );
    }

    #[test]
    fn boolean_operators() {
        let code = r#"
            let a = true && false;
            let b = !a || true;
        "#;

        let lex = Lexer::new(code).lex().unwrap();

        assert_eq!(
            lex,
            vec![
                Keyword(Let),
                Identifier("a".into()),
                Operator(Assignment),
                Literal(super::Literal::Boolean(true)),
                Operator(And),
                Literal(super::Literal::Boolean(false)),
                Punctuation(Semicolon),
                Keyword(Let),
                Identifier("b".into()),
                Operator(Assignment),
                Operator(Not),
                Identifier("a".into()),
                Operator(Or),
                Literal(super::Literal::Boolean(true)),
                Punctuation(Semicolon),
                Eof,
            ]
        );
    }

    #[test]
    fn function_declaration() {
        let code = r#"
            fn add(a: i32, b: i32) -> i32 {
                return a + b;
            }
        "#;

        let lex = Lexer::new(code).lex().unwrap();

        assert_eq!(
            lex,
            vec![
                Keyword(Function),
                Identifier("add".into()),
                Punctuation(OpenParenthesis),
                Identifier("a".into()),
                Punctuation(Colon),
                Identifier("i32".into()),
                Punctuation(Comma),
                Identifier("b".into()),
                Punctuation(Colon),
                Identifier("i32".into()),
                Punctuation(CloseParenthesis),
                Operator(Arrow),
                Identifier("i32".into()),
                Punctuation(OpenBrace),
                Keyword(Return),
                Identifier("a".into()),
                Operator(Add),
                Identifier("b".into()),
                Punctuation(Semicolon),
                Punctuation(CloseBrace),
                Eof,
            ]
        );
    }

    #[test]
    fn variable_declaration() {
        let code = r#"
            let x = 42;
            let y: i32 = 123;
        "#;

        let lex = Lexer::new(code).lex().unwrap();

        assert_eq!(
            lex,
            vec![
                Keyword(Let),
                Identifier("x".into()),
                Operator(Assignment),
                Literal(super::Literal::Integer(42)),
                Punctuation(Semicolon),
                Keyword(Let),
                Identifier("y".into()),
                Punctuation(Colon),
                Identifier("i32".into()),
                Operator(Assignment),
                Literal(super::Literal::Integer(123)),
                Punctuation(Semicolon),
                Eof,
            ]
        );
    }

    #[test]
    fn control_flow() {
        let code = r#"
            fn is_even(x: i32) -> bool {
                if x % 2 == 0 {
                    return true;
                } else {
                    return false;
                }
            }
    
            fn main() {
                let n = 10;
                if is_even(n) {
                    print("{} is even", n);
                } else {
                    print("{} is odd", n);
                }
            }
        "#;

        let lex = Lexer::new(code).lex().unwrap();

        assert_eq!(
            lex,
            vec![
                Keyword(Function),
                Identifier("is_even".into()),
                Punctuation(OpenParenthesis),
                Identifier("x".into()),
                Punctuation(Colon),
                Identifier("i32".into()),
                Punctuation(CloseParenthesis),
                Operator(Arrow),
                Identifier("bool".into()),
                Punctuation(OpenBrace),
                Keyword(If),
                Identifier("x".into()),
                Operator(Modulo),
                Literal(super::Literal::Integer(2)),
                Operator(Equal),
                Literal(super::Literal::Integer(0)),
                Punctuation(OpenBrace),
                Keyword(Return),
                Literal(super::Literal::Boolean(true)),
                Punctuation(Semicolon),
                Punctuation(CloseBrace),
                Keyword(Else),
                Punctuation(OpenBrace),
                Keyword(Return),
                Literal(super::Literal::Boolean(false)),
                Punctuation(Semicolon),
                Punctuation(CloseBrace),
                Punctuation(CloseBrace),
                Keyword(Function),
                Identifier("main".into()),
                Punctuation(OpenParenthesis),
                Punctuation(CloseParenthesis),
                Punctuation(OpenBrace),
                Keyword(Let),
                Identifier("n".into()),
                Operator(Assignment),
                Literal(super::Literal::Integer(10)),
                Punctuation(Semicolon),
                Keyword(If),
                Identifier("is_even".into()),
                Punctuation(OpenParenthesis),
                Identifier("n".into()),
                Punctuation(CloseParenthesis),
                Punctuation(OpenBrace),
                Identifier("print".into()),
                Punctuation(OpenParenthesis),
                Literal(super::Literal::String("{} is even".into())),
                Punctuation(Comma),
                Identifier("n".into()),
                Punctuation(CloseParenthesis),
                Punctuation(Semicolon),
                Punctuation(CloseBrace),
                Keyword(Else),
                Punctuation(OpenBrace),
                Identifier("print".into()),
                Punctuation(OpenParenthesis),
                Literal(super::Literal::String("{} is odd".into())),
                Punctuation(Comma),
                Identifier("n".into()),
                Punctuation(CloseParenthesis),
                Punctuation(Semicolon),
                Punctuation(CloseBrace),
                Punctuation(CloseBrace),
                Eof,
            ]
        );
    }
}
