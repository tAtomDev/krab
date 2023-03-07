use crate::util;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Literal(Literal),
    Identifier(String),
    Operator(Operator),
    Punctuation(Punctuation),
    Eof,
    Invalid,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Let,
    If,
    Else,
    While,
    Function,
    Return,
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
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
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    Arrow,
}

#[derive(Debug, Clone, PartialEq)]
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

pub struct Lexer {
    source: String,
    position: usize,
    waiting_for_identifier: bool,
}

impl Lexer {
    pub fn new(code: impl Into<String>) -> Self {
        Self {
            source: code.into(),
            position: 0,
            waiting_for_identifier: false,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_at_end() {
            let token = self.scan_token();
            if token == Token::Invalid {
                continue;
            }

            tokens.push(token);
        }

        tokens.push(Token::Eof);
        tokens
    }

    fn advance_char(&mut self) -> char {
        self.position += 1;
        self.source.chars().nth(self.position - 1).unwrap()
    }

    fn current_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.position).unwrap()
        }
    }

    fn check_char_and_advance(&mut self, char: char) -> bool {
        if self.current_char() == char {
            self.position += 1;
            return true;
        }

        false
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.source.len()
    }

    fn scan_token(&mut self) -> Token {
        let c = self.advance_char();

        let token = match c {
            '(' => Token::Punctuation(Punctuation::OpenParenthesis),
            ')' => Token::Punctuation(Punctuation::CloseParenthesis),
            '[' => Token::Punctuation(Punctuation::OpenBrackets),
            ']' => Token::Punctuation(Punctuation::CloseBrackets),
            '{' => Token::Punctuation(Punctuation::OpenBrace),
            '}' => Token::Punctuation(Punctuation::CloseBrace),
            ';' => Token::Punctuation(Punctuation::Semicolon),
            ':' => Token::Punctuation(Punctuation::Colon),
            ',' => Token::Punctuation(Punctuation::Comma),
            '.' => Token::Punctuation(Punctuation::Dot),
            '?' => Token::Punctuation(Punctuation::QuestionMark),

            '+' => Token::Operator(Operator::Add),
            '-' => {
                if self.check_char_and_advance('>') {
                    Token::Operator(Operator::Arrow)
                } else {
                    Token::Operator(Operator::Subtract)
                }
            }
            '*' => Token::Operator(Operator::Multiply),
            '/' => Token::Operator(Operator::Divide),
            '%' => Token::Operator(Operator::Modulo),
            '^' => Token::Operator(Operator::Power),
            '!' => Token::Operator(Operator::Not),

            '=' => {
                if self.check_char_and_advance('=') {
                    Token::Operator(Operator::Equal)
                } else {
                    Token::Operator(Operator::Assignment)
                }
            }
            '>' => {
                if self.check_char_and_advance('=') {
                    Token::Operator(Operator::GreaterOrEqual)
                } else {
                    Token::Operator(Operator::Greater)
                }
            }
            '<' => {
                if self.check_char_and_advance('=') {
                    Token::Operator(Operator::LessOrEqual)
                } else {
                    Token::Operator(Operator::Less)
                }
            }

            c if c == '&' && self.check_char_and_advance('&') => Token::Operator(Operator::And),
            c if c == '|' && self.check_char_and_advance('|') => Token::Operator(Operator::Or),

            '"' | '\'' => self.string(c),
            c if c.is_ascii_digit() => self.number(c),
            c if c.is_alphabetic() => self.keyword_or_identifier(c),
            _ => Token::Invalid,
        };

        if let Token::Keyword(keyword) = &token {
            match *keyword {
                Keyword::Function | Keyword::Let => self.waiting_for_identifier = true,
                _ => {}
            }
        }

        token
    }

    fn string(&mut self, quote: char) -> Token {
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
            panic!("Missing {} at the end of string literal", quote);
        }

        Token::Literal(Literal::String(string))
    }

    fn number(&mut self, first_number: char) -> Token {
        let mut string = String::new();
        let mut failed = true;
        let mut is_float = false;

        string.push(first_number);

        while !self.is_at_end() {
            if self.current_char() == '.' {
                if is_float {
                    panic!("Invalid floating point number formatting");
                }

                is_float = true;
            } else if !self.current_char().is_digit(9) {
                failed = false;
                break;
            }

            string.push(self.advance_char());
        }

        if failed {
            panic!("Invalid number literal");
        }

        if is_float {
            Token::Literal(Literal::Float(string.parse::<f32>().unwrap()))
        } else {
            Token::Literal(Literal::Integer(string.parse::<i32>().unwrap()))
        }
    }

    fn keyword_or_identifier(&mut self, char: char) -> Token {
        let mut string = String::new();

        string.push(char);

        while !self.is_at_end() {
            let current_char = self.current_char();
            if !util::is_char_valid_identifier(current_char) {
                break;
            }

            string.push(self.advance_char());
        }

        match string.as_str() {
            "let" => Token::Keyword(Keyword::Let),
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "while" => Token::Keyword(Keyword::While),
            "fn" => Token::Keyword(Keyword::Function),
            "return" => Token::Keyword(Keyword::Return),
            "break" => Token::Keyword(Keyword::Break),
            "continue" => Token::Keyword(Keyword::Continue),

            "true" => Token::Literal(Literal::Boolean(true)),
            "false" => Token::Literal(Literal::Boolean(false)),

            _ => self.identifier(string),
        }
    }

    fn identifier(&mut self, mut string: String) -> Token {
        while !self.is_at_end() {
            let current_char = self.current_char();

            if current_char != '_' && !util::is_char_valid_identifier(current_char) {
                break;
            }

            string.push(self.advance_char());
        }

        if string.is_empty() {
            panic!("Invalid identifier");
        }

        Token::Identifier(string)
    }
}

#[cfg(test)]
mod tests {
    use super::Keyword::*;
    use super::Operator::*;
    use super::Punctuation::*;
    use super::Token::*;
    use super::*;

    #[test]
    fn hello_world() {
        let code = r#"
        fn main() {
            let x = "Hello, world!";
            print(x);
        }
        "#;

        let lex = Lexer::new(code).lex();

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

        let lex = Lexer::new(code).lex();

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

        let lex = Lexer::new(code).lex();

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

        let lex = Lexer::new(code).lex();

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

        let lex = Lexer::new(code).lex();

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

        let lex = Lexer::new(code).lex();

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
