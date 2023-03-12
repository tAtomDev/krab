use crate::{
    ast::{Expression, Identifier, IdentifierKind, Statement},
    common::tokens::*,
};

pub struct Parser {
    position: usize,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            position: 0,
            tokens,
        }
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut program = Vec::new();

        while !self.is_at_end() {
            let statement = self.parse_statement();
            if statement != Statement::Empty {
                program.push(statement);
            }
        }

        program
    }

    pub fn current_token(&self) -> &Token {
        if self.is_at_end() {
            &Token::Eof
        } else {
            self.tokens.get(self.position).unwrap()
        }
    }

    pub fn is_at_end(&self) -> bool {
        let token = self.tokens.get(self.position).unwrap_or(&Token::Eof);
        *token == Token::Eof
    }

    fn expect_token(&mut self, token: Token) {
        if self.advance_token() != token {
            panic!(
                "Expected token: {:?}, but found {:?}",
                token,
                self.current_token()
            )
        }
    }

    fn expect_semicolon(&mut self) {
        if self.advance_token() != Token::Punctuation(Punctuation::Semicolon) {
            panic!("Expected semicolon ';'");
        }
    }

    fn return_positions(&mut self, positions: usize) {
        self.position -= positions;
    }

    fn advance_token(&mut self) -> Token {
        self.position += 1;
        self.tokens.get(self.position - 1).unwrap().clone()
    }

    fn parse_statement(&mut self) -> Statement {
        let token = self.current_token();
        match *token {
            Token::Identifier(_) => self.parse_identifier(),
            Token::Literal(_) => Statement::Expression(self.parse_expression()),
            Token::Keyword(_) => self.parse_keyword(),
            Token::Punctuation(Punctuation::Semicolon) => {
                self.advance_token();
                Statement::Empty
            }
            Token::Invalid => panic!("Invalid token: {:?}", token),
            Token::Eof => unreachable!(),
            _ => {
                let expr = self.parse_expression();
                if self.current_token() == &SEMICOLON_TOKEN {
                    self.advance_token();
                }

                Statement::Expression(expr)
            }
        }
    }

    fn parse_identifier(&mut self) -> Statement {
        let Token::Identifier(identifier) = self.advance_token() else {
            panic!("Expected identifier at parse_identifier");
        };

        match self.current_token() {
            Token::Operator(Operator::Assignment) => {
                self.advance_token();
                let expression = self.parse_expression();

                self.expect_semicolon();

                Statement::Assignment(identifier, Box::new(expression))
            }
            _ => {
                self.return_positions(1);
                Statement::Expression(self.parse_expression())
            }
        }
    }

    fn parse_keyword(&mut self) -> Statement {
        let Token::Keyword(keyword) = self.advance_token() else {
            panic!("Expected keyword at parse_keyword");
        };

        match keyword {
            Keyword::Let => self.declare_variable(false),
            Keyword::Const => self.declare_variable(true),
            _ => panic!("Keyword {:?} not yet implemented", keyword),
        }
    }

    fn declare_variable(&mut self, is_const: bool) -> Statement {
        let Token::Identifier(identifier) = self.advance_token() else {
            panic!("Expected a valid identifier");
        };

        self.expect_token(Token::Operator(Operator::Assignment));

        let expression = self.parse_expression();

        self.expect_semicolon();

        Statement::VariableDeclaration {
            is_const,
            name: identifier,
            value_expression: Box::new(expression),
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_binary(0)
    }

    fn parse_binary(&mut self, min_precedence: u8) -> Expression {
        let mut lhs = self.parse_primary();
        loop {
            let operator = self.current_token().clone();
            let Token::Operator(operator) = operator else {
                if operator == Token::Eof || operator == Token::Punctuation(Punctuation::CloseParenthesis) || operator == SEMICOLON_TOKEN {
                    break;
                }

                panic!("Expected operator, found '{:?}'", operator);
            };

            let operator_precedence = operator.precedence();
            if operator_precedence < min_precedence {
                break;
            }

            self.advance_token();
            let rhs = if operator.associativity() == Associativity::Left {
                self.parse_binary(operator_precedence + 1)
            } else {
                self.parse_binary(operator_precedence)
            };

            lhs = Expression::Binary(Box::new(lhs), operator, Box::new(rhs));
        }

        lhs
    }

    fn parse_primary(&mut self) -> Expression {
        let token = self.advance_token();

        match token {
            Token::Identifier(name) => Expression::Identifier(Identifier {
                kind: IdentifierKind::Variable,
                name,
            }),
            Token::Literal(literal) => Expression::Literal(literal),
            Token::Punctuation(Punctuation::OpenParenthesis) => {
                let expr = self.parse_expression();

                if self.advance_token() != Token::Punctuation(Punctuation::CloseParenthesis) {
                    panic!("Missing ')' at the end of the expression");
                }

                expr
            }
            _ => panic!("Trying to parse unexpected token: {:?}", token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::lexer::Lexer;

    #[test]
    fn basic_variable() {
        let code = r#"
            let x = 1 + 1;
            x = 5;
        "#;

        let program = Parser::new(Lexer::new(code).lex()).parse();
        assert_eq!(
            program,
            vec![
                Statement::VariableDeclaration {
                    name: "x".into(),
                    value_expression: Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Literal::Integer(1))),
                        Operator::Add,
                        Box::new(Expression::Literal(Literal::Integer(1)))
                    )),
                    is_const: false
                },
                Statement::Assignment(
                    "x".into(),
                    Box::new(Expression::Literal(Literal::Integer(5)))
                )
            ]
        )
    }

    #[test]
    fn complex_math_expression() {
        let code = "let x = x * ((2 + 3 * 4) / (5 - 1));";
        let program = Parser::new(Lexer::new(code).lex()).parse();
        assert_eq!(
            program,
            vec![Statement::VariableDeclaration {
                name: "x".into(),
                value_expression: Box::new(Expression::Binary(
                    Box::new(Expression::Identifier(Identifier {
                        kind: IdentifierKind::Variable,
                        name: "x".into()
                    })),
                    Operator::Multiply,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Binary(
                            Box::new(Expression::Literal(Literal::Integer(2))),
                            Operator::Add,
                            Box::new(Expression::Binary(
                                Box::new(Expression::Literal(Literal::Integer(3))),
                                Operator::Multiply,
                                Box::new(Expression::Literal(Literal::Integer(4)))
                            )),
                        )),
                        Operator::Divide,
                        Box::new(Expression::Binary(
                            Box::new(Expression::Literal(Literal::Integer(5))),
                            Operator::Subtract,
                            Box::new(Expression::Literal(Literal::Integer(1)))
                        )),
                    ))
                )),
                is_const: false,
            }]
        );
    }
}
