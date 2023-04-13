use std::collections::HashMap;

use thiserror::Error;

use super::ast::*;
use crate::common::{tokens::*, *};

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("expected semicolon ';' at {0}")]
    ExpectedSemicolon(Span),
    #[error("invalid token {1} at {0}")]
    InvalidToken(Span, Token),
    #[error("invalid expression at {0}")]
    InvalidExpression(Span),
    #[error("expected a valid identifier at {0}")]
    ExpectedValidIdentifier(Span),

    #[error("you must assign a value to the '{1}' variable at {0}")]
    MustAssignToVariable(Span, String),
    #[error("expected operator but found {1} at {0}")]
    ExpectedOperatorButFound(Span, Token),

    #[error("invalid function argument at {0}. Try something like this: `fn example(x: int)`")]
    InvalidFunctionArgument(Span),

    #[error("invalid type at {0}")]
    InvalidType(Span),
    #[error("variable's type is unknown at {0}, try specifying it with `variable: TYPE = VALUE`")]
    UnknownType(Span),
    #[error("trying to assign `{1}` to `{2}` at {0}")]
    IncorrectType(Span, Type, Type),

    #[error("missing ')' at the end of the expression at {0}")]
    MissingClosingParenthesis(Span),
    #[error("trying to parse unexpected operator `{1}` at {0}")]
    TryingToParseUnexpectedOperator(Span, Operator),
    #[error("trying to parse unexpected token `{1}` at {0}")]
    TryingToParseUnexpectedToken(Span, Token),
    #[error("invalid unary expression at {0}")]
    InvalidUnaryExpression(Span),

    #[error("'{1}' expected at {0}")]
    Expected(Span, char),
    #[error("expected {1} but found {2} at {0}")]
    ExpectedButFound(Span, &'static str, String),
}

pub struct Parser {
    position: usize,
    tokens: Vec<Token>,
    function_return_type_cache: HashMap<String, Option<Type>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            position: 0,
            tokens,
            function_return_type_cache: HashMap::new(),
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

    fn span(&self) -> Span {
        self.current_token().span
    }

    pub fn current_token(&self) -> &Token {
        if self.is_at_end() {
            &Token::EOF
        } else {
            self.tokens.get(self.position).unwrap()
        }
    }

    pub fn is_at_end(&self) -> bool {
        let token = self.tokens.get(self.position).unwrap_or(&Token::EOF);
        token.is_eof()
    }

    fn expect_semicolon(&mut self) -> Result<(), ParserError> {
        let token = self.advance_token();
        if token != TokenKind::Punctuation(Punctuation::Semicolon) {
            return Err(ParserError::ExpectedSemicolon(token.span));
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
        match self.current_token().kind {
            TokenKind::Punctuation(Punctuation::Semicolon) => {
                self.advance_token();
                Ok(Node::Empty)
            }
            TokenKind::Punctuation(Punctuation::OpenBrace) => {
                Ok(Node::Expression(self.parse_body()?))
            }
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
        let token = self.current_token().clone();
        match token.kind {
            TokenKind::Punctuation(Punctuation::Semicolon) => Err(ParserError::ExpectedButFound(
                token.span,
                "a valid expression",
                ";".into(),
            )),
            TokenKind::Keyword(_) => {
                let statement = self.parse_keyword()?;
                match statement {
                    Statement::Expression(expression) => Ok(expression),
                    _ => Err(ParserError::InvalidExpression(token.span)),
                }
            }
            TokenKind::Punctuation(Punctuation::OpenBrace) => self.parse_body(),
            _ => self.parse_binary(0),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.current_token();
        match token.kind {
            TokenKind::Identifier(_) => self.parse_identifier(),
            TokenKind::Literal(_) => Ok(Statement::Expression(self.parse_expression()?)),
            TokenKind::Keyword(_) => self.parse_keyword(),
            TokenKind::Invalid => Err(ParserError::InvalidToken(
                token.span,
                self.current_token().clone(),
            )),
            TokenKind::Eof => unreachable!(),
            _ => {
                let expr = self.parse_expression()?;

                Ok(Statement::Expression(expr))
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<Statement, ParserError> {
        let identifier_token = self.advance_token();
        let identifier = match identifier_token.kind {
            TokenKind::Identifier(identifier) => identifier,
            _ => return Err(ParserError::ExpectedValidIdentifier(identifier_token.span)),
        };

        let statement = match self.current_token().kind {
            TokenKind::Operator(Operator::Assignment) => {
                self.advance_token();
                let expression = self.parse_expression()?;
                self.expect_semicolon()?;
                Statement::Assignment(identifier, Box::new(expression))
            }
            TokenKind::Punctuation(Punctuation::OpenParenthesis) => {
                let token = self.advance_token();
                if token != TokenKind::Punctuation(Punctuation::OpenParenthesis) {
                    return Err(ParserError::Expected(token.span, '('));
                }

                let mut args = vec![];

                loop {
                    if self.is_at_end() {
                        return Err(ParserError::Expected(Span::EOF, ')'));
                    }

                    if self.current_token()
                        == &TokenKind::Punctuation(Punctuation::CloseParenthesis)
                    {
                        self.advance_token();
                        break;
                    }

                    let expression = match self.parse_statement()? {
                        Statement::Expression(expression) => expression,
                        _ => return Err(ParserError::InvalidExpression(self.span())),
                    };

                    args.push(Box::new(expression));

                    if self.advance_token() != TokenKind::Punctuation(Punctuation::Comma) {
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
        let keyword = match keyword_token.kind {
            TokenKind::Keyword(keyword) => keyword,
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
            Keyword::Function => self.parse_function_declaration(),
            _ => panic!("Keyword {:?} not implemented yet", keyword),
        }
    }

    fn parse_variable_declaration(&mut self, is_const: bool) -> Result<Statement, ParserError> {
        let token = self.advance_token();
        let identifier = match token.kind {
            TokenKind::Identifier(identifier) => identifier,
            _ => return Err(ParserError::ExpectedValidIdentifier(token.span)),
        };

        // Parse explicit type, if present
        // KEY IDENT: TY = VALUE;
        // -> let x: int = 0;
        let explicit_ty = if self.current_token() == &TokenKind::Punctuation(Punctuation::Colon) {
            self.advance_token();

            let token = self.advance_token();
            match token.kind {
                TokenKind::Identifier(type_identifier) => Some(Type::from(type_identifier)),
                _ => return Err(ParserError::ExpectedValidIdentifier(token.span)),
            }
        } else {
            None
        };

        // Expect `=` token
        let token = self.advance_token();
        if token != TokenKind::Operator(Operator::Assignment) {
            return Err(ParserError::MustAssignToVariable(token.span, identifier));
        }

        let value_expression = self.parse_expression()?;
        self.expect_semicolon()?;

        // Verify implicit and explicit types
        let ty = match (
            explicit_ty,
            try_parse_expression_type(
                &self.function_return_type_cache,
                self.span(),
                &value_expression,
            )?,
        ) {
            (Some(explicit_ty), Some(implicit_ty)) if explicit_ty != implicit_ty => {
                return Err(ParserError::IncorrectType(
                    self.span(),
                    explicit_ty,
                    implicit_ty,
                ));
            }
            (Some(explicit_ty), _) => explicit_ty,
            (_, Some(implicit_ty)) => implicit_ty,
            (_, None) => return Err(ParserError::UnknownType(self.span())),
        };

        Ok(Statement::VariableDeclaration {
            name: identifier,
            ty,
            value_expression: Box::new(value_expression),
            is_const,
        })
    }

    fn parse_function_declaration(&mut self) -> Result<Statement, ParserError> {
        let token = self.advance_token();
        let identifier = match token.kind {
            TokenKind::Identifier(identifier) => identifier,
            _ => return Err(ParserError::ExpectedValidIdentifier(token.span)),
        };

        let args = self.parse_function_args()?;

        // Parse return type
        let mut ty = None;

        if self.current_token() == &TokenKind::Operator(Operator::Arrow) {
            self.advance_token();

            ty = match self.advance_token().kind {
                TokenKind::Identifier(type_identifier) => Some(Type::from(type_identifier)),
                _ => return Err(ParserError::ExpectedValidIdentifier(token.span)),
            };
        }

        // Parse body
        let body = self.parse_body()?;

        self.function_return_type_cache
            .insert(identifier.clone(), ty.clone());

        Ok(Statement::FunctionDeclaration {
            name: identifier,
            args,
            return_type: ty,
            body: Box::new(body),
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
        if self.current_token() == &TokenKind::Keyword(Keyword::Else) {
            // Skip the else keywor?/d
            self.advance_token();

            // Check for else if's
            if self.current_token() == &TokenKind::Keyword(Keyword::If) {
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
        let token = self.advance_token();
        if token != TokenKind::Punctuation(Punctuation::OpenParenthesis) {
            return Err(ParserError::Expected(token.span, '('));
        }

        let mut func_args = vec![];

        loop {
            if self.is_at_end() {
                return Err(ParserError::Expected(Span::EOF, ')'));
            }

            if self.current_token() == &TokenKind::Punctuation(Punctuation::CloseParenthesis) {
                self.advance_token();
                break;
            }

            let token = self.advance_token();
            let identifier = match token.kind {
                TokenKind::Identifier(identifier) => identifier,
                _ => return Err(ParserError::InvalidFunctionArgument(token.span)),
            };

            // Skip and check `IDENT:TYPE` Colon
            let token = self.advance_token();
            if token != TokenKind::Punctuation(Punctuation::Colon) {
                return Err(ParserError::InvalidFunctionArgument(token.span));
            }

            // Parse type identifier
            let token = self.advance_token();
            let type_identifier = match token.kind {
                TokenKind::Identifier(identifier) => identifier,
                _ => return Err(ParserError::ExpectedValidIdentifier(token.span)),
            };

            let ty = Type::from(type_identifier);

            func_args.push((ty, identifier));

            if self.advance_token() != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
        }

        Ok(func_args)
    }

    fn parse_body(&mut self) -> Result<Expression, ParserError> {
        let token = self.advance_token();
        if token != TokenKind::Punctuation(Punctuation::OpenBrace) {
            return Err(ParserError::Expected(token.span, '{'));
        }

        let mut body = vec![];

        loop {
            if self.is_at_end() {
                return Err(ParserError::Expected(Span::EOF, '}'));
            }

            if self.current_token() == &TokenKind::Punctuation(Punctuation::CloseBrace) {
                self.advance_token();
                break;
            }

            let node = self.parse_node()?;

            body.push(node);
        }

        Ok(Expression::Body(body))
    }

    fn parse_binary(&mut self, min_precedence: u8) -> Result<Expression, ParserError> {
        let token = self.current_token();
        match token.kind {
            // Ignore (
            TokenKind::Punctuation(Punctuation::OpenParenthesis) => {}

            TokenKind::Punctuation(p) => {
                return Err(ParserError::ExpectedButFound(
                    token.span,
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

            let operator = match operator_token.kind {
                TokenKind::Operator(operator) => operator,
                TokenKind::Eof | TokenKind::Punctuation(_) => break,
                _ => {
                    return Err(ParserError::ExpectedOperatorButFound(
                        operator_token.span,
                        operator_token,
                    ))
                }
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

        let expression = match token.kind {
            TokenKind::Identifier(name) => {
                let token = self.advance_token();
                if token != TokenKind::Punctuation(Punctuation::OpenParenthesis) {
                    self.return_positions(1);
                    return Ok(Expression::Identifier(Identifier {
                        kind: IdentifierKind::Variable,
                        name,
                    }));
                }

                let mut args = vec![];

                loop {
                    if self.is_at_end() {
                        return Err(ParserError::Expected(Span::EOF, ')'));
                    }

                    if self.current_token()
                        == &TokenKind::Punctuation(Punctuation::CloseParenthesis)
                    {
                        self.advance_token();
                        break;
                    }

                    let expression = match self.parse_statement()? {
                        Statement::Expression(expression) => expression,
                        _ => return Err(ParserError::InvalidExpression(self.span())),
                    };

                    args.push(Box::new(expression));

                    if self.advance_token() != TokenKind::Punctuation(Punctuation::Comma) {
                        break;
                    }
                }

                Expression::Call(name, args)
            }
            TokenKind::Literal(literal) => Expression::Literal(literal),
            TokenKind::Punctuation(Punctuation::OpenParenthesis) => {
                let expr = self.parse_expression()?;

                let token = self.advance_token();
                if token != TokenKind::Punctuation(Punctuation::CloseParenthesis) {
                    return Err(ParserError::MissingClosingParenthesis(token.span));
                }

                expr
            }
            TokenKind::Operator(operator) => {
                if operator.is_unary() {
                    if let TokenKind::Operator(op) = self.current_token().kind {
                        if op.is_unary() {
                            return Err(ParserError::InvalidUnaryExpression(
                                self.current_token().span,
                            ));
                        }
                    }

                    let expression = self.parse_expression()?;
                    Expression::Unary(operator, Box::new(expression))
                } else {
                    return Err(ParserError::TryingToParseUnexpectedOperator(
                        token.span, operator,
                    ));
                }
            }
            _ => return Err(ParserError::TryingToParseUnexpectedToken(token.span, token)),
        };

        Ok(expression)
    }
}

fn try_parse_expression_type(
    function_type_cache: &HashMap<String, Option<Type>>,
    span: Span,
    expression: &Expression,
) -> Result<Option<Type>, ParserError> {
    let ty = match &expression {
        Expression::Literal(literal) => {
            let v: Value = literal.clone().into();
            v.ty().map_err(|_| ParserError::InvalidType(span))?
        }
        Expression::Binary(a, op, b) => {
            let a = try_parse_expression_type(function_type_cache, span, a)?;
            let b = try_parse_expression_type(function_type_cache, span, b)?;

            if !op.is_logical() && a != b {
                return Ok(None);
            }

            a.ok_or(ParserError::InvalidType(span))?
        }
        Expression::Call(name, _) => match function_type_cache.get(name) {
            Some(ty) if ty.is_some() => ty.clone().unwrap(),
            _ => return Ok(None),
        },
        Expression::Unary(_, v) => return try_parse_expression_type(function_type_cache, span, v),
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
