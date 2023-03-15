use crate::{
    ast::{Expression, Identifier, IdentifierKind, Statement},
    common::{tokens::*, ControlFlow, Type, Value},
};

use thiserror::Error;

use super::ast::{Body, Node};

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("expected semicolon ';'")]
    ExpectedSemicolon,
    #[error("invalid token {0}")]
    InvalidToken(Token),
    #[error("invalid expression")]
    InvalidExpression,
    #[error("expected a valid identifier")]
    ExpectedValidIdentifier,

    #[error("you must assign a value to the '{0}' variable")]
    MustAssignToVariable(String),
    #[error("expected operator but found {0}")]
    ExpectedOperatorButFound(Token),

    #[error("invalid function argument. Try something like this: `fn example(x: int)`")]
    InvalidFunctionArgument,

    #[error("invalid type")]
    InvalidType,
    #[error("this variable's type is unknown, try specifying it with `variable: TYPE = VALUE`")]
    UnknownType,
    #[error("trying to assign `{0}` to `{1}`")]
    IncorrectType(Type, Type),

    #[error("missing ')' at the end of the expression")]
    MissingClosingParenthesis,
    #[error("trying to parse unexpected operator: {0}")]
    TryingToParseUnexpectedOperator(Operator),
    #[error("trying to parse unexpected token: {0}")]
    TryingToParseUnexpectedToken(Token),
    #[error("invalid unary expression")]
    InvalidUnaryExpression,

    #[error("'{0}' expected")]
    Expected(char),
    #[error("expected {0} but found {1}")]
    ExpectedButFound(&'static str, String),
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

    pub fn parse(&mut self) -> Result<Body, ParserError> {
        let mut program = Vec::new();

        while !self.is_at_end() {
            let node = self.parse_node()?;
            if node != Node::Empty {
                program.push(node);
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

    fn parse_node(&mut self) -> Result<Node, ParserError> {
        match self.current_token() {
            Token::Punctuation(Punctuation::Semicolon) => {
                self.advance_token();
                Ok(Node::Empty)
            }
            Token::Punctuation(Punctuation::OpenBrace) => Ok(Node::Expression(self.parse_body()?)),
            _ => {
                let stmt = self.parse_statement()?;
                if let Statement::Expression(expr) = stmt {
                    if self.current_token() == &SEMICOLON_TOKEN {
                        self.advance_token();
                        return Ok(Node::Statement(Statement::Expression(expr)));
                    }

                    Ok(Node::Expression(expr))
                } else {
                    Ok(Node::Statement(stmt))
                }
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        match self.current_token() {
            Token::Punctuation(Punctuation::Semicolon) => Err(ParserError::ExpectedButFound(
                "a valid expression",
                ";".into(),
            )),
            Token::Keyword(_) => {
                let statement = self.parse_keyword()?;
                match statement {
                    Statement::Expression(expression) => Ok(expression),
                    _ => Err(ParserError::InvalidExpression),
                }
            }
            Token::Punctuation(Punctuation::OpenBrace) => self.parse_body(),
            _ => self.parse_binary(0),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token() {
            Token::Identifier(_) => self.parse_identifier(),
            Token::Literal(_) => Ok(Statement::Expression(self.parse_expression()?)),
            Token::Keyword(_) => self.parse_keyword(),
            Token::Invalid => Err(ParserError::InvalidToken(self.current_token().clone())),
            Token::Eof => unreachable!(),
            _ => {
                let expr = self.parse_expression()?;

                Ok(Statement::Expression(expr))
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<Statement, ParserError> {
        let identifier_token = self.advance_token();
        let identifier = match identifier_token {
            Token::Identifier(identifier) => identifier,
            _ => return Err(ParserError::ExpectedValidIdentifier),
        };

        let statement = match self.current_token() {
            Token::Operator(Operator::Assignment) => {
                self.advance_token();
                let expression = self.parse_expression()?;
                self.expect_semicolon()?;
                Statement::Assignment(identifier, Box::new(expression))
            }
            Token::Punctuation(Punctuation::OpenParenthesis) => {
                if self.advance_token() != Token::Punctuation(Punctuation::OpenParenthesis) {
                    return Err(ParserError::Expected('('));
                }

                let mut args = vec![];

                loop {
                    if self.is_at_end() {
                        return Err(ParserError::Expected(')'));
                    }

                    if self.current_token() == &Token::Punctuation(Punctuation::CloseParenthesis) {
                        self.advance_token();
                        break;
                    }

                    let expression = match self.parse_statement()? {
                        Statement::Expression(expression) => expression,
                        _ => return Err(ParserError::InvalidExpression),
                    };

                    args.push(Box::new(expression));

                    if self.advance_token() != Token::Punctuation(Punctuation::Comma) {
                        break;
                    }
                }

                Statement::Expression(Expression::Call(identifier, args))
            }
            _ => {
                self.return_positions(1);
                Statement::Expression(self.parse_expression()?)
            }
        };

        Ok(statement)
    }

    fn parse_keyword(&mut self) -> Result<Statement, ParserError> {
        let keyword_token = self.advance_token();
        let keyword = match keyword_token {
            Token::Keyword(keyword) => keyword,
            _ => unreachable!(),
        };

        match keyword {
            Keyword::Let => self.parse_variable_declaration(false),
            Keyword::Const => self.parse_variable_declaration(true),
            Keyword::Return => {
                let expression = self.parse_expression()?;
                self.expect_semicolon()?;

                Ok(Statement::Return(expression))
            }
            Keyword::If => {
                let expression = self.parse_if()?;

                Ok(Statement::Expression(expression))
            }
            Keyword::While => {
                let condition = self.parse_expression()?;

                let body = self.parse_body()?;

                Ok(Statement::Expression(Expression::While {
                    condition: Box::new(condition),
                    body: Box::new(body),
                }))
            }
            Keyword::Continue => {
                if self.current_token() == &SEMICOLON_TOKEN {
                    return Ok(Statement::Expression(Expression::ControlFlow(
                        ControlFlow::Continue,
                        None,
                    )));
                }

                let expression = self.parse_expression()?;
                self.expect_semicolon()?;

                Ok(Statement::Expression(Expression::ControlFlow(
                    ControlFlow::Continue,
                    Some(Box::new(expression)),
                )))
            }
            Keyword::Break => {
                if self.current_token() == &SEMICOLON_TOKEN {
                    return Ok(Statement::Expression(Expression::ControlFlow(
                        ControlFlow::Break,
                        None,
                    )));
                }

                let expression = self.parse_expression()?;
                self.expect_semicolon()?;

                Ok(Statement::Expression(Expression::ControlFlow(
                    ControlFlow::Break,
                    Some(Box::new(expression)),
                )))
            }
            Keyword::Function => {
                let identifier = match self.advance_token() {
                    Token::Identifier(identifier) => identifier,
                    _ => return Err(ParserError::ExpectedValidIdentifier),
                };

                let args = self.parse_function_args()?;

                let body = self.parse_body()?;

                Ok(Statement::FunctionDeclaration {
                    name: identifier,
                    args,
                    body: Box::new(body),
                })
            }
            _ => panic!("Keyword {:?} not implemented yet", keyword),
        }
    }

    fn parse_variable_declaration(&mut self, is_const: bool) -> Result<Statement, ParserError> {
        let identifier = match self.advance_token() {
            Token::Identifier(identifier) => identifier,
            _ => return Err(ParserError::ExpectedValidIdentifier),
        };

        // Parse explicit type, if present
        // KEY IDENT: TY = VALUE;
        // -> let x: int = 0;
        let explicit_ty = if self.current_token() == &Token::Punctuation(Punctuation::Colon) {
            self.advance_token();
            match self.advance_token() {
                Token::Identifier(type_identifier) => Some(Type::from(type_identifier)),
                _ => return Err(ParserError::ExpectedValidIdentifier),
            }
        } else {
            None
        };

        // Expect `=` token
        if self.advance_token() != Token::Operator(Operator::Assignment) {
            return Err(ParserError::MustAssignToVariable(identifier));
        }

        let value_expression = self.parse_expression()?;
        self.expect_semicolon()?;

        // Verify implicit and explicit types
        let ty = match (explicit_ty, try_parse_expression_type(&value_expression)?) {
            (Some(explicit_ty), Some(implicit_ty)) if explicit_ty != implicit_ty => {
                return Err(ParserError::IncorrectType(explicit_ty, implicit_ty));
            }
            (Some(explicit_ty), _) => explicit_ty,
            (_, Some(implicit_ty)) => implicit_ty,
            (_, None) => return Err(ParserError::UnknownType),
        };

        Ok(Statement::VariableDeclaration {
            name: identifier,
            ty,
            value_expression: Box::new(value_expression),
            is_const,
        })
    }

    fn parse_if(&mut self) -> Result<Expression, ParserError> {
        let condition = self.parse_expression()?;

        let body = self.parse_body()?;

        let else_expression = self.parse_else()?;

        Ok(Expression::If {
            condition: Box::new(condition),
            body: Box::new(body),
            else_branch: else_expression.map(Box::new),
        })
    }

    fn parse_else(&mut self) -> Result<Option<Expression>, ParserError> {
        if self.current_token() == &Token::Keyword(Keyword::Else) {
            // Skip the else keywor?/d
            self.advance_token();

            // Check for else if's
            if self.current_token() == &Token::Keyword(Keyword::If) {
                self.advance_token();
                return Ok(Some(self.parse_if()?));
            }

            // Simple else expression
            let body = self.parse_body()?;

            Ok(Some(body))
        } else {
            Ok(None)
        }
    }

    fn parse_function_args(&mut self) -> Result<Vec<(Type, String)>, ParserError> {
        if self.advance_token() != Token::Punctuation(Punctuation::OpenParenthesis) {
            return Err(ParserError::Expected('('));
        }

        let mut func_args = vec![];

        loop {
            if self.is_at_end() {
                return Err(ParserError::Expected(')'));
            }

            if self.current_token() == &Token::Punctuation(Punctuation::CloseParenthesis) {
                self.advance_token();
                break;
            }

            let identifier = match self.advance_token() {
                Token::Identifier(identifier) => identifier,
                _ => return Err(ParserError::InvalidFunctionArgument),
            };

            // Skip and check `IDENT:TYPE` Colon
            if self.advance_token() != Token::Punctuation(Punctuation::Colon) {
                return Err(ParserError::InvalidFunctionArgument);
            }

            let type_identifier = match self.advance_token() {
                Token::Identifier(identifier) => identifier,
                _ => return Err(ParserError::ExpectedValidIdentifier),
            };

            let ty = Type::from(type_identifier);

            func_args.push((ty, identifier));

            if self.advance_token() != Token::Punctuation(Punctuation::Comma) {
                break;
            }
        }

        Ok(func_args)
    }

    fn parse_body(&mut self) -> Result<Expression, ParserError> {
        if self.advance_token() != Token::Punctuation(Punctuation::OpenBrace) {
            return Err(ParserError::Expected('{'));
        }

        let mut body = vec![];

        loop {
            if self.is_at_end() {
                return Err(ParserError::Expected('}'));
            }

            if self.current_token() == &Token::Punctuation(Punctuation::CloseBrace) {
                self.advance_token();
                break;
            }

            let node = self.parse_node()?;

            body.push(node);
        }

        Ok(Expression::Body(body))
    }

    fn parse_binary(&mut self, min_precedence: u8) -> Result<Expression, ParserError> {
        match self.current_token() {
            // Ignore (
            Token::Punctuation(Punctuation::OpenParenthesis) => {}

            Token::Punctuation(p) => {
                return Err(ParserError::ExpectedButFound(
                    "a valid binary expression",
                    format!("'{}'", p),
                ))
            }
            _ => {}
        };

        let mut lhs = self.parse_primary()?;

        loop {
            let operator_token = self.current_token().clone();
            if operator_token == SEMICOLON_TOKEN {
                break;
            }

            let operator = match operator_token {
                Token::Operator(operator) => operator,
                Token::Eof | Token::Punctuation(_) => break,
                _ => return Err(ParserError::ExpectedOperatorButFound(operator_token)),
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
            Token::Operator(operator) => {
                if operator.is_unary() {
                    if let Token::Operator(op) = self.current_token() {
                        if op.is_unary() {
                            return Err(ParserError::InvalidUnaryExpression);
                        }
                    }

                    let expression = self.parse_expression()?;
                    Expression::Unary(operator, Box::new(expression))
                } else {
                    return Err(ParserError::TryingToParseUnexpectedOperator(operator));
                }
            }
            _ => return Err(ParserError::TryingToParseUnexpectedToken(token)),
        };

        Ok(expression)
    }
}

fn try_parse_expression_type(expression: &Expression) -> Result<Option<Type>, ParserError> {
    let ty = match &expression {
        Expression::Literal(literal) => {
            let v: Value = literal.clone().into();
            v.ty().map_err(|_| ParserError::InvalidType)?
        }
        Expression::Binary(a, op, b) => {
            let a = try_parse_expression_type(a)?;
            let b = try_parse_expression_type(b)?;

            if !op.is_logical() && a != b {
                return Ok(None);
            }

            a.ok_or(ParserError::InvalidType)?
        }
        Expression::Unary(_, v) => return try_parse_expression_type(v),
        _ => return Ok(None),
    };

    Ok(Some(ty))
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
                Node::Statement(Statement::VariableDeclaration {
                    name: "x".into(),
                    ty: Type::Int,
                    value_expression: Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Literal::Integer(1))),
                        Operator::Add,
                        Box::new(Expression::Literal(Literal::Integer(1)))
                    )),
                    is_const: false
                }),
                Node::Statement(Statement::Assignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Operator::Add,
                        Box::new(Expression::Unary(
                            Operator::Subtract,
                            Box::new(Expression::Literal(Literal::Integer(1)))
                        ))
                    ))
                ))
            ]
        )
    }

    #[test]
    fn complex_math_expression() {
        let code = "let x = 0; x = x * ((2 + 3 * 4) / (5 - 1)); x";
        let program = Parser::new(Lexer::new(code).lex().unwrap())
            .parse()
            .unwrap();

        assert_eq!(
            program,
            vec![
                Node::Statement(Statement::VariableDeclaration {
                    name: "x".into(),
                    ty: Type::Int,
                    is_const: false,
                    value_expression: Box::new(Expression::Literal(Literal::Integer(0)))
                }),
                Node::Statement(Statement::Assignment(
                    "x".into(),
                    Box::new(Expression::Binary(
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
                    ))
                )),
                Node::Expression(Expression::Identifier(Identifier {
                    kind: IdentifierKind::Variable,
                    name: "x".into()
                }))
            ]
        );
    }

    #[test]
    fn basic_if_else() {
        let code = r#"
            if true {
                1
            } else {
                2
            }
        "#;

        let program = Parser::new(Lexer::new(code).lex().unwrap())
            .parse()
            .unwrap();
        assert_eq!(
            program,
            vec![Node::Expression(Expression::If {
                condition: Box::new(Expression::Literal(Literal::Boolean(true))),
                body: Box::new(Expression::Body(vec![Node::Expression(
                    Expression::Literal(Literal::Integer(1))
                )])),
                else_branch: Some(Box::new(Expression::Body(vec![Node::Expression(
                    Expression::Literal(Literal::Integer(2))
                )]))),
            })]
        )
    }

    #[test]
    fn basic_while_loop() {
        let code = r#"
            let x = 0;
            while x < 5 {
                x = x + 1;
            }
            x
        "#;

        let program = Parser::new(Lexer::new(code).lex().unwrap())
            .parse()
            .unwrap();
        assert_eq!(
            program,
            vec![
                Node::Statement(Statement::VariableDeclaration {
                    name: "x".into(),
                    ty: Type::Int,
                    value_expression: Box::new(Expression::Literal(Literal::Integer(0))),
                    is_const: false,
                }),
                Node::Expression(Expression::While {
                    condition: Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier {
                            kind: IdentifierKind::Variable,
                            name: "x".into()
                        })),
                        Operator::Less,
                        Box::new(Expression::Literal(Literal::Integer(5)))
                    )),
                    body: Box::new(Expression::Body(vec![Node::Statement(
                        Statement::Assignment(
                            "x".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier(Identifier {
                                    kind: IdentifierKind::Variable,
                                    name: "x".into()
                                })),
                                Operator::Add,
                                Box::new(Expression::Literal(Literal::Integer(1)))
                            ))
                        )
                    )])),
                }),
                Node::Expression(Expression::Identifier(Identifier {
                    kind: IdentifierKind::Variable,
                    name: "x".into()
                }))
            ]
        )
    }
}
