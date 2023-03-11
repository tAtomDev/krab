use crate::{
    ast::{Expression, Identifier, IdentifierKind, Statement},
    lexer::{Punctuation, Token},
};

use super::lexer::Associativity;

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

    fn advance_token(&mut self) -> Token {
        self.position += 1;
        self.tokens.get(self.position - 1).unwrap().clone()
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut program = Vec::new();

        while !self.is_at_end() {
            program.push(self.parse_statement());
        }

        program
    }

    fn parse_statement(&mut self) -> Statement {
        Statement::Expression(self.parse_expression())
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_binary(0)
    }

    fn parse_binary(&mut self, min_precedence: u8) -> Expression {
        let mut lhs = self.parse_primary();
        loop {
            let operator = self.current_token().clone();
            let Token::Operator(operator) = operator else {
                if operator == Token::Eof {
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
                kind: if self.current_token() == &Token::Punctuation(Punctuation::OpenParenthesis) {
                    IdentifierKind::Function
                } else {
                    IdentifierKind::Variable
                },
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
