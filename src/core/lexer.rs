use crate::{
    ast::{tokens::*, *},
    error::LexicalError,
};

pub struct Lexer {
    source: Vec<char>,
    loc: u32,
    line_pos: u32,
    pos: usize,
}

impl Lexer {
    pub fn new(source: impl Into<String>) -> Self {
        let source = source.into();
        Self {
            source: source.chars().collect(),
            loc: 1,
            line_pos: 1,
            pos: 0,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexicalError> {
        let mut tokens = Vec::new();
        while let Some(token) = self.scan_token()? {
            tokens.push(token);
        }

        tokens.push(Token::EOF);

        Ok(tokens)
    }

    fn make_span(&self) -> Span {
        Span::new(self.loc, self.line_pos, self.pos as u32)
    }

    fn advance_pos(&mut self) {
        self.pos += 1;

        if self.current_char() == '\n' || self.current_char() == '\r' {
            self.line_pos = 1;
            self.loc += 1;
        } else {
            self.line_pos += 1;
        }
    }

    fn advance_multiple_positions(&mut self, positions: usize) {
        (0..positions).for_each(|_| self.advance_pos());
    }

    fn advance_char(&mut self) -> char {
        let char = self.current_char();
        self.advance_pos();

        char
    }

    fn current_char(&self) -> char {
        self.source.get(self.pos).copied().unwrap_or('\0')
    }

    fn current_and_next_chars(&self, number: usize) -> Vec<char> {
        self.source[self.pos..]
            .iter()
            .take(number)
            .copied()
            .collect()
    }

    fn is_at_end(&self) -> bool {
        self.current_char() == '\0'
    }

    fn check_char_and_advance(&mut self, ch: char) -> bool {
        if self.current_char() == ch {
            self.advance_pos();
            true
        } else {
            false
        }
    }

    fn check_string_and_advance(&mut self, base_char: char, string: &str) -> bool {
        let size = string.chars().count() - 1;

        let next_string = [base_char]
            .iter()
            .chain(self.current_and_next_chars(size).iter())
            .collect::<String>();

        if next_string == string {
            self.advance_multiple_positions(size);
            true
        } else {
            false
        }
    }

    fn scan_token(&mut self) -> Result<Option<Token>, LexicalError> {
        if self.is_at_end() {
            return Ok(None);
        }

        let span = self.make_span();

        let c = self.advance_char();

        if c == '/' && self.check_char_and_advance('/') {
            // This is a single-line comment, so we skip until the end of the line.
            while !self.is_at_end() && self.current_char() != '\n' {
                self.advance_pos();
            }

            return self.scan_token();
        }

        // Lex punctuations
        for punctuation in PUNCTUATIONS.iter() {
            let punc = punctuation.to_string();
            if self.check_string_and_advance(c, &punc) {
                return Ok(Some(Token::new(span, TokenKind::Punctuation(*punctuation))));
            }
        }

        // Lex operators
        for operator in OPERATORS.iter() {
            let op = operator.to_string();
            if self.check_string_and_advance(c, &op) {
                return Ok(Some(Token::new(span, TokenKind::Operator(*operator))));
            }
        }

        // Lex keywords
        for keyword in KEYWORDS.iter() {
            let kw = keyword.to_string();
            if self.check_string_and_advance(c, &kw) {
                return Ok(Some(Token::new(span, TokenKind::Keyword(keyword.clone()))));
            }
        }

        match c {
            ' ' | '\n' | '\r' => self.scan_token(),
            // Lex string literals
            '\'' | '"' => {
                let quote = c;
                let mut string = String::new();
                let mut backslash = false;

                loop {
                    if self.is_at_end() {
                        return Err(LexicalError::MissingAtEndOfStringLiteral(quote, span));
                    }

                    let c = self.advance_char();
                    if c == '\\' && !backslash {
                        backslash = true;
                        continue;
                    }

                    if c == quote && !backslash {
                        break;
                    }

                    backslash = false;
                    string.push(c);
                }

                let token_kind = TokenKind::Literal(Literal::String(string));
                Ok(Some(Token::new(span, token_kind)))
            }
            // Lex number literals
            c if c.is_ascii_digit() => {
                let mut number_string = String::from(c);
                let mut is_float_literal = false;
                loop {
                    let c = self.current_char();
                    if c.is_ascii_digit() {
                        number_string.push(c);
                    } else if c == '.' {
                        // Check if float literal doesn't contains two .
                        if number_string.contains('.') {
                            return Err(LexicalError::InvalidFloatLiteral(self.make_span()));
                        }
                        is_float_literal = true;
                        number_string.push(c);
                    } else {
                        break;
                    }

                    self.advance_char();
                }

                let token_kind = TokenKind::Literal(match is_float_literal {
                    true => Literal::Float(
                        number_string
                            .parse()
                            .map_err(|_| LexicalError::InvalidFloatLiteral(span))?,
                    ),
                    false => Literal::Integer(
                        number_string
                            .parse()
                            .map_err(|_| LexicalError::InvalidIntegerLiteral(span))?,
                    ),
                });

                Ok(Some(Token::new(span, token_kind)))
            }
            c if c.is_ascii() && (!c.is_ascii_punctuation() || c == '_') => {
                let mut string = String::from(c);

                while !self.is_at_end() {
                    let current_char = self.current_char();
                    if current_char == ' '
                        || current_char == '\n'
                        || current_char == '\r'
                        || (current_char != '_' && current_char.is_ascii_punctuation())
                    {
                        break;
                    }

                    if current_char != '_' && current_char.is_ascii_punctuation() {
                        return Err(LexicalError::InvalidIdentifierCharacter(
                            current_char,
                            self.make_span(),
                        ));
                    }

                    string.push(self.advance_char());
                }

                if string.is_empty() {
                    return Err(LexicalError::InvalidIdentifier(span));
                }

                let token_kind = match string.as_str() {
                    "true" => TokenKind::Literal(Literal::Boolean(true)),
                    "false" => TokenKind::Literal(Literal::Boolean(false)),
                    _ => TokenKind::Identifier(string),
                };

                Ok(Some(Token::new(span, token_kind)))
            }
            _ => Err(LexicalError::UnexpectedCharacter(c, span)),
        }
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
                Punctuation(Arrow),
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
                Punctuation(Arrow),
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
