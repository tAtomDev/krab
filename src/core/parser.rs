use crate::{
    ast::{Expression, Identifier, IdentifierKind, Statement},
    common::tokens::*,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("expected semicolon ';'")]
    ExpectedSemicolon,
    #[error("invalid token '{0}'")]
    InvalidToken(Token),
    #[error("expected a valid identifier")]
    ExpectedValidIdentifier,
    #[error("you must assign a value to the '{0}' variable")]
    MustAssignToVariable(String),
    #[error("expected operator but found '{0}'")]
    ExpectedOperatorButFound(Token),
    #[error("missing ')' at the end of the expression")]
    MissingClosingParenthesis,
    #[error("trying to parse unexpected operator: {0:?}")]
    TryingToParseUnexpectedOperator(Operator),
    #[error("trying to parse unexpected token: {0}")]
    TryingToParseUnexpectedToken(Token),
    #[error("invalid unary expression")]
    InvalidUnaryExpression,
}

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

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut program = Vec::new();

        while !self.is_at_end() {
            let statement = self.parse_statement()?;
            if statement != Statement::Empty {
                program.push(statement);
            }
        }

        Ok(program)
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

    fn expect_semicolon(&mut self) -> Result<(), ParserError> {
        if self.advance_token() != Token::Punctuation(Punctuation::Semicolon) {
            return Err(ParserError::ExpectedSemicolon);
        }

        Ok(())
    }

    fn return_positions(&mut self, positions: usize) {
        self.position -= positions;
    }

    fn advance_token(&mut self) -> Token {
        self.position += 1;
        self.tokens.get(self.position - 1).unwrap().clone()
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.current_token();
        let statement = match *token {
            Token::Identifier(_) => self.parse_identifier()?,
            Token::Literal(_) => Statement::Expression(self.parse_expression()?),
            Token::Keyword(_) => self.parse_keyword()?,
            Token::Punctuation(Punctuation::Semicolon) => {
                self.advance_token();
                Statement::Empty
            }

            Token::Invalid => return Err(ParserError::InvalidToken(token.clone())),

            Token::Eof => unreachable!(),
            _ => {
                let expr = self.parse_expression()?;
                if self.current_token() == &SEMICOLON_TOKEN {
                    self.advance_token();
                }

                Statement::Expression(expr)
            }
        };

        Ok(statement)
    }

    fn parse_identifier(&mut self) -> Result<Statement, ParserError> {
        let Token::Identifier(identifier) = self.advance_token() else {
            unreachable!()
        };

        let statement = match self.current_token() {
            Token::Operator(Operator::Assignment) => {
                self.advance_token();
                let expression = self.parse_expression()?;

                self.expect_semicolon()?;

                Statement::Assignment(identifier, Box::new(expression))
            }
            _ => {
                self.return_positions(1);
                Statement::Expression(self.parse_expression()?)
            }
        };

        Ok(statement)
    }

    fn parse_keyword(&mut self) -> Result<Statement, ParserError> {
        let Token::Keyword(keyword) = self.advance_token() else {
            unreachable!()
        };

        let statement = match keyword {
            Keyword::Let => self.declare_variable(false)?,
            Keyword::Const => self.declare_variable(true)?,
            _ => panic!("Keyword {:?} not yet implemented", keyword),
        };

        Ok(statement)
    }

    fn declare_variable(&mut self, is_const: bool) -> Result<Statement, ParserError> {
        let Token::Identifier(identifier) = self.advance_token() else {
            return Err(ParserError::ExpectedValidIdentifier);
        };

        if self.advance_token() != Token::Operator(Operator::Assignment) {
            return Err(ParserError::MustAssignToVariable(identifier));
        }

        let expression = self.parse_expression()?;

        self.expect_semicolon()?;

        Ok(Statement::VariableDeclaration {
            is_const,
            name: identifier,
            value_expression: Box::new(expression),
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_binary(0)
    }

    fn parse_binary(&mut self, min_precedence: u8) -> Result<Expression, ParserError> {
        let mut lhs = self.parse_primary()?;
        loop {
            let operator = self.current_token().clone();
            let Token::Operator(operator) = operator else {
                if operator == Token::Eof || operator == Token::Punctuation(Punctuation::CloseParenthesis) || operator == SEMICOLON_TOKEN {
                    break;
                }

                return Err(ParserError::ExpectedOperatorButFound(operator));
            };

            let operator_precedence = operator.precedence();
            if operator_precedence < min_precedence {
                break;
            }

            self.advance_token();
            let rhs = if operator.associativity() == Associativity::Left {
                self.parse_binary(operator_precedence + 1)?
            } else {
                self.parse_binary(operator_precedence)?
            };

            lhs = Expression::Binary(Box::new(lhs), operator, Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParserError> {
        let token = self.advance_token();

        let expression = match token {
            Token::Identifier(name) => Expression::Identifier(Identifier {
                kind: IdentifierKind::Variable,
                name,
            }),
            Token::Literal(literal) => Expression::Literal(literal),
            Token::Punctuation(Punctuation::OpenParenthesis) => {
                let expr = self.parse_expression()?;

                if self.advance_token() != Token::Punctuation(Punctuation::CloseParenthesis) {
                    return Err(ParserError::MissingClosingParenthesis);
                }

                expr
            }
            Token::Operator(op) => {
                if !op.is_unary() {
                    return Err(ParserError::TryingToParseUnexpectedOperator(op));
                }

                let expression = self.parse_primary()?;
                if let Expression::Unary(_, _) = &expression {
                    return Err(ParserError::InvalidUnaryExpression);
                }

                Expression::Unary(op, Box::new(expression))
            }
            _ => return Err(ParserError::TryingToParseUnexpectedToken(token)),
        };

        Ok(expression)
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
            x = 5 + -1;
        "#;

        let program = Parser::new(Lexer::new(code).lex().unwrap())
            .parse()
            .unwrap();
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
                    Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Operator::Add,
                        Box::new(Expression::Unary(
                            Operator::Subtract,
                            Box::new(Expression::Literal(Literal::Integer(1)))
                        ))
                    ))
                )
            ]
        )
    }

    #[test]
    fn complex_math_expression() {
        let code = "let x = x * ((2 + 3 * 4) / (5 - 1));";
        let program = Parser::new(Lexer::new(code).lex().unwrap())
            .parse()
            .unwrap();
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
