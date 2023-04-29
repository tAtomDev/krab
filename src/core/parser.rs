use crate::{
    ast::{tokens::*, *},
    error::{LogicalError, SyntaxError},
    runtime::AccessModifier,
    types::{Ident, Signature, Type, TypedIdent},
};

use super::{parser_type_cache::TypeCache, parser_util};

#[derive(Debug, Clone)]
pub struct Parser {
    pos: usize,
    tokens: Vec<Token>,
    pub type_cache: TypeCache,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, type_cache: TypeCache) -> Parser {
        Parser {
            pos: 0,
            tokens,
            type_cache,
        }
    }

    pub fn load_tokens(&mut self, tokens: Vec<Token>) {
        self.pos = 0;
        self.tokens = tokens;
    }

    pub fn parse_ast(&mut self) -> Result<AST, SyntaxError> {
        let mut ast = AST::new("script");

        while !self.is_at_end() {
            ast.push(self.parse_node()?);
        }

        Ok(ast)
    }

    pub fn is_at_end(&self) -> bool {
        let token = self.tokens.get(self.pos).unwrap_or(&Token::EOF);
        token == &Token::EOF
    }

    pub fn current_span(&self) -> Span {
        self.current_token().span
    }

    pub fn current_token(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::EOF)
    }

    pub fn next_token(&self) -> &Token {
        self.tokens.get(self.pos + 1).unwrap_or(&Token::EOF)
    }

    pub fn advance_token(&mut self) -> Token {
        self.pos += 1;
        self.tokens.get(self.pos - 1).unwrap().clone()
    }

    /// Consume a token if it matches the given condition
    pub fn consume_token_if<F>(&mut self, condition: F) -> bool
    where
        F: Fn(&Token) -> bool,
    {
        if condition(self.current_token()) {
            self.advance_token();
            true
        } else {
            false
        }
    }

    pub fn consume_token_if_kind(&mut self, kind: &TokenKind) -> bool {
        if self.current_token() == kind {
            self.advance_token();
            true
        } else {
            false
        }
    }

    pub fn expect_token(&mut self, token_kind: TokenKind) -> Result<(), SyntaxError> {
        if self.consume_token_if_kind(&token_kind) {
            Ok(())
        } else {
            Err(SyntaxError::Expected(
                token_kind.to_string(),
                self.current_span(),
            ))
        }
    }

    pub fn expect_semicolon(&mut self) -> Result<(), SyntaxError> {
        if self.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::Semicolon)) {
            Ok(())
        } else {
            Err(SyntaxError::SemicolonExpected(self.current_span()))
        }
    }

    pub fn parse_node(&mut self) -> Result<Node, SyntaxError> {
        let start_span = self.current_span();

        let node_kind = if let Some(statement) = self.parse_statement()? {
            NodeKind::Statement(statement)
        } else {
            NodeKind::Expression(self.parse_expression()?)
        };

        let end_span = self.current_span();
        let interval = SpanInterval {
            start: start_span,
            end: end_span,
        };

        Ok(Node {
            kind: node_kind,
            span: interval,
        })
    }

    pub fn parse_statement(&mut self) -> Result<Option<Statement>, SyntaxError> {
        let span = self.current_span();

        let statement = match self.current_token().kind.clone() {
            TokenKind::Keyword(_) => self.parse_keyword_statement()?,
            TokenKind::Identifier(identifier) => {
                let mut assignment_op = None;

                // += -= *= /= and %=
                if let TokenKind::Operator(operator) = self.next_token().kind {
                    if let Some(op) = operator.parse_assignment_op() {
                        assignment_op = Some(op);
                    }
                }

                if assignment_op.is_some()
                    || self.next_token().kind == TokenKind::Operator(Operator::Assignment)
                {
                    // Advance identifier
                    self.advance_token();
                    // Advance operator
                    self.advance_token();

                    let expression_span = self.current_span();
                    let expression = self.parse_expression()?;

                    // TYPE CACHE: Check if variable exists
                    let Some((_span, access_modifier, type_cache)) = self.type_cache.get_variable(&identifier) else {
                        return Err(LogicalError::VariableDoesNotExists(identifier, span).into());
                    };

                    if access_modifier == AccessModifier::Constant {
                        return Err(LogicalError::CannotAssignToConstant(identifier, span).into());
                    }

                    // TYPE CACHE: Check if type is valid
                    let expression_type = expression
                        .try_parse_type(&mut self.type_cache, expression_span)?
                        .unwrap_or(Type::Unit);
                    if expression_type != type_cache {
                        return Err(LogicalError::CannotAssignTypeToVariable(
                            expression_type,
                            type_cache,
                            identifier,
                            expression_span,
                        )
                        .into());
                    }

                    self.expect_semicolon()?;

                    let ident = Ident {
                        name: identifier,
                        span,
                    };

                    if let Some(op) = assignment_op {
                        Some(Statement::Assignment(
                            ident.clone(),
                            Expression::Binary(
                                Box::new(Expression::Identifier(ident)),
                                op,
                                Box::new(expression),
                            ),
                        ))
                    } else {
                        Some(Statement::Assignment(ident, expression))
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        Ok(statement)
    }

    pub fn parse_keyword_statement(&mut self) -> Result<Option<Statement>, SyntaxError> {
        let token = self.current_token().clone();
        let TokenKind::Keyword(keyword) = &token.kind else {
            unreachable!();
        };

        let statement = match keyword {
            Keyword::Let | Keyword::Const => {
                self.advance_token();
                let (identifier, identifier_span) = parser_util::parse_identifier_token(self)?;

                let access_modifier = match keyword {
                    Keyword::Const => AccessModifier::Constant,
                    _ => AccessModifier::ReadWrite,
                };

                // Parse explicit type, if present
                // KEY IDENT: TY = VALUE;
                // -> let x: int = 0;
                let explicit_ty =
                    if self.current_token() == &TokenKind::Punctuation(Punctuation::Colon) {
                        self.advance_token();

                        let (identifier, _) = parser_util::parse_identifier_token(self)?;
                        Some(Type::from(identifier))
                    } else {
                        None
                    };

                // Expect `=`
                if !self.consume_token_if_kind(&TokenKind::Operator(Operator::Assignment)) {
                    return Err(SyntaxError::MustAssignToVariable(identifier, token.span));
                }

                // Parse value expression
                let span = self.current_span();
                let expression = self.parse_expression()?;

                self.expect_semicolon()?;

                let implicit_ty = expression.try_parse_type(&mut self.type_cache, span)?;
                let ty = match (explicit_ty, implicit_ty) {
                    (Some(explicit_ty), Some(implicit_ty)) if explicit_ty != implicit_ty => {
                        return Err(SyntaxError::IncorrectType(explicit_ty, implicit_ty, span));
                    }
                    (Some(explicit_ty), _) => explicit_ty,
                    (_, Some(implicit_ty)) => implicit_ty,
                    _ => return Err(SyntaxError::UnknownType(span)),
                };

                self.type_cache.declare_variable(
                    &identifier,
                    ty.clone(),
                    access_modifier,
                    identifier_span,
                )?;

                Statement::VariableDeclaration(
                    TypedIdent {
                        ident: Ident {
                            name: identifier,
                            span: identifier_span,
                        },
                        ty,
                    },
                    expression,
                )
            }
            Keyword::Function => {
                self.advance_token();

                let (identifier, start_span) = parser_util::parse_identifier_token(self)?;

                // Parse (arg1: type, arg2: type, ..)
                let args =
                    parser_util::parse_typed_identifiers(self, Punctuation::OpenParenthesis)?;

                // Return type
                let return_type =
                    if self.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::Arrow)) {
                        parser_util::parse_type_token(self)?
                    } else {
                        Type::Unit
                    };

                // Create the function body scope
                self.type_cache.push_scope();
                for arg in args.iter() {
                    self.type_cache.declare_variable(
                        &arg.ident.name,
                        arg.ty.clone(),
                        AccessModifier::ReadWrite,
                        arg.ident.span,
                    )?;
                }

                // Declare itself to allow recursion
                self.type_cache.declare_function(
                    &identifier,
                    return_type.clone(),
                    &args,
                    start_span,
                )?;

                // Function body
                let body_start_span = self.current_span();
                let body_expression = self.parse_body_expression()?;

                // TYPE CHECK: check if body expression respect the function return type
                let body_type = body_expression
                    .try_parse_type(&mut self.type_cache, body_start_span)?
                    .unwrap_or(Type::Unit);

                if body_type != return_type {
                    return Err(LogicalError::ExpectedReturnTypeButFound(
                        return_type,
                        body_type,
                        body_start_span,
                    )
                    .into());
                }

                // Pop function body scope
                self.type_cache.pop_scope();

                // Declare the function in the outer scope
                self.type_cache.declare_function(
                    &identifier,
                    return_type.clone(),
                    &args,
                    start_span,
                )?;

                Statement::FunctionDeclaration(
                    Signature {
                        identifier: Ident {
                            name: identifier,
                            span: start_span,
                        },
                        args,
                        return_type,
                        span: SpanInterval {
                            start: start_span,
                            end: self.current_span(),
                        },
                    },
                    body_expression,
                )
            }
            Keyword::Return => {
                self.advance_token();

                if self.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::Semicolon)) {
                    Statement::Return(Expression::Empty)
                } else {
                    let expression = self.parse_expression()?;
                    self.expect_semicolon()?;

                    Statement::Return(expression)
                }
            }
            _ => return Ok(None),
        };

        Ok(Some(statement))
    }

    pub fn parse_expression(&mut self) -> Result<Expression, SyntaxError> {
        match self.current_token().kind {
            TokenKind::Keyword(Keyword::If) => {
                self.advance_token();

                let condition = self.parse_expression()?;

                // Parse `then` branch
                let branch = self.parse_body_expression()?;

                // Parse `else` branch
                let else_branch = match self.current_token().kind {
                    TokenKind::Keyword(Keyword::Else) => {
                        self.advance_token();

                        // Check if the next token is an "if" keyword
                        if self.current_token().kind == TokenKind::Keyword(Keyword::If) {
                            Some(self.parse_expression()?)
                        } else {
                            Some(self.parse_body_expression()?)
                        }
                    }
                    _ => None,
                };

                Ok(Expression::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(branch),
                    else_branch: else_branch.map(Box::new),
                })
            }
            TokenKind::Keyword(Keyword::While) => {
                self.advance_token();

                let condition = self.parse_expression()?;
                let body = self.parse_body_expression()?;

                Ok(Expression::While {
                    condition: Box::new(condition),
                    body: Box::new(body),
                })
            }
            TokenKind::Punctuation(Punctuation::Semicolon) => {
                self.advance_token();
                Ok(Expression::Empty)
            }
            TokenKind::Punctuation(Punctuation::OpenBrace) => self.parse_body_expression(),
            _ => self.parse_binary_expression(0),
        }
    }

    pub fn parse_body_expression(&mut self) -> Result<Expression, SyntaxError> {
        let expression = parser_util::parse_body_expression(self)?;

        Ok(expression)
    }

    /// Parses a binary expression with the given minimum precedence.
    ///
    /// This function implements the Pratt parsing algorithm to parse a binary expression
    /// in a recursive descent parser. The algorithm uses precedence and associativity rules
    /// to correctly parse expressions that involve operators with different priorities.
    ///
    /// Parameters:
    /// * `min_precedence`: the minimum precedence of operators that the function should consider.
    ///   This is used to ensure that operators with higher precedence are parsed first.
    /// ```
    pub fn parse_binary_expression(
        &mut self,
        min_precedence: u8,
    ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.parse_primary_expression()?;

        while let TokenKind::Operator(operator) = self.current_token().kind {
            let operator_precedence = operator.precedence();

            // If the operator has lower precedence than the minimum, we break out of the loop
            if operator_precedence < min_precedence {
                break;
            }

            // Skip the operator
            self.advance_token();

            // Parse the right-hand side of the expression
            let rhs = if operator.associativity() == Associativity::Left {
                // If the operator is left-associative, we parse the expression with one higher precedence
                self.parse_binary_expression(operator_precedence + 1)?
            } else {
                // If the operator is right-associative or has no associativity, we parse the expression with the same precedence
                self.parse_binary_expression(operator_precedence)?
            };

            lhs = Expression::Binary(Box::new(lhs), operator, Box::new(rhs));
        }

        Ok(lhs)
    }

    /// Parses a primary expression, which can be a literal, identifier or a parenthesized expression.
    /// E.g. `54`, `x` `(y + x - 35.15)`
    pub fn parse_primary_expression(&mut self) -> Result<Expression, SyntaxError> {
        // Check if the current token is an open parenthesis
        if self.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::OpenParenthesis)) {
            // Parse the expression inside the parenthesis
            let expression = self.parse_expression()?;

            // Ensure that the closing parenthesis is present
            if !self.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::CloseParenthesis)) {
                return Err(SyntaxError::ExpectedButFound(
                    "a closing parenthesis".into(),
                    format!("{}", self.current_token()),
                    self.current_token().span,
                ));
            }

            return Ok(expression);
        }

        // If not a parenthesized expression, parse a literal or identifier
        let token = self.advance_token();
        match &token.kind {
            TokenKind::Literal(literal) => Ok(Expression::Literal(literal.clone())),
            TokenKind::Identifier(identifier) => {
                let ident = Ident {
                    name: identifier.clone(),
                    span: token.span,
                };
                // Function call
                if self.current_token().kind == TokenKind::Punctuation(Punctuation::OpenParenthesis)
                {
                    let args = parser_util::parse_multiple_expressions(
                        self,
                        Punctuation::OpenParenthesis,
                    )?;

                    // TYPE CHECK: check if function exists
                    let Some((_, _, func_args)) = self.type_cache.get_function(identifier) else {
                        println!("{:?}", self.type_cache.scopes);
                        return Err(LogicalError::FunctionDoesNotExists(
                            identifier.to_owned(),
                            token.span,
                        )
                        .into());
                    };

                    if args.len() != func_args.len() {
                        return Err(LogicalError::IncorrectFunctionArgumentsAmount(
                            identifier.clone(),
                            func_args.len(),
                            args.len(),
                            token.span,
                        )
                        .into());
                    }

                    for (index, func_arg) in func_args.iter().enumerate() {
                        let arg_ty = args[index]
                            .try_parse_type(&mut self.type_cache, token.span)?
                            .unwrap_or(Type::Unit);
                        if func_arg.ty != arg_ty {
                            return Err(LogicalError::IncorrectArgumentType(
                                func_arg.clone(),
                                func_arg.ident.span,
                            )
                            .into());
                        }
                    }

                    Ok(Expression::Call(ident, args))
                } else {
                    // TYPE CHECK: check if variable exists
                    if self.type_cache.get_variable(identifier).is_none() {
                        return Err(LogicalError::VariableDoesNotExists(
                            identifier.to_owned(),
                            token.span,
                        )
                        .into());
                    }

                    Ok(Expression::Identifier(ident))
                }
            }
            // Unary not && negative
            TokenKind::Operator(op) if op.is_unary() => {
                let rhs = self.parse_expression()?;
                Ok(Expression::Unary(*op, Box::new(rhs)))
            }
            _ => Err(SyntaxError::TryingToParseUnexpectedToken(
                token.clone(),
                token.span,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::*;

    fn parse_expression(input: &str) -> Expression {
        let mut parser = Parser::new(Lexer::new(input).lex().unwrap(), TypeCache::new_empty());
        parser.parse_expression().unwrap()
    }

    fn parse_statement(input: &str) -> Statement {
        let mut parser = Parser::new(Lexer::new(input).lex().unwrap(), TypeCache::new_empty());
        parser.parse_statement().unwrap().unwrap()
    }

    #[test]
    fn literal_parsing() {
        assert_eq!(
            parse_expression("42"),
            Expression::Literal(Literal::Integer(42))
        );

        assert_eq!(
            parse_expression("3.14"),
            Expression::Literal(Literal::Float(3.14))
        );

        assert_eq!(
            [parse_expression("true"), parse_expression("false")],
            [
                Expression::Literal(Literal::Boolean(true)),
                Expression::Literal(Literal::Boolean(false))
            ]
        );

        assert_eq!(
            parse_expression("\"hello\""),
            Expression::Literal(Literal::String("hello".to_string()))
        );
    }

    #[test]
    fn binary_expression_parsing() {
        assert_eq!(
            parse_expression("1 + 2"),
            Expression::Binary(
                Box::new(Expression::Literal(Literal::Integer(1))),
                Operator::Add,
                Box::new(Expression::Literal(Literal::Integer(2)))
            )
        );

        assert_eq!(
            parse_expression("3 * 4 - 5"),
            Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Literal::Integer(3))),
                    Operator::Multiply,
                    Box::new(Expression::Literal(Literal::Integer(4)))
                )),
                Operator::Subtract,
                Box::new(Expression::Literal(Literal::Integer(5)))
            )
        );

        assert_eq!(
            parse_expression("6 / (7 + 8)"),
            Expression::Binary(
                Box::new(Expression::Literal(Literal::Integer(6))),
                Operator::Divide,
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Literal::Integer(7))),
                    Operator::Add,
                    Box::new(Expression::Literal(Literal::Integer(8)))
                ))
            )
        );
    }

    #[test]
    fn variable_statement_parsing() {
        assert_eq!(
            parse_statement("let x = 6 / (7 + 8);"),
            Statement::VariableDeclaration(
                TypedIdent {
                    ident: Ident::from("x".to_string()),
                    ty: Type::Integer
                },
                Expression::Binary(
                    Box::new(Expression::Literal(Literal::Integer(6))),
                    Operator::Divide,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Literal::Integer(7))),
                        Operator::Add,
                        Box::new(Expression::Literal(Literal::Integer(8)))
                    ))
                )
            )
        );
    }
}
