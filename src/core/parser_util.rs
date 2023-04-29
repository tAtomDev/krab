use crate::{
    ast::{
        tokens::{Punctuation, TokenKind},
        Expression, Span,
    },
    error::SyntaxError,
    types::{Ident, Type, TypedIdent},
};

use super::Parser;

/// Parses an identifier token and returns it as a string along with its corresponding span.
pub fn parse_identifier_token(parser: &mut Parser) -> Result<(String, Span), SyntaxError> {
    // Advance to the next token and check if it is an identifier.
    let identifier_token = parser.advance_token();
    if let TokenKind::Identifier(identifier) = &identifier_token.kind {
        Ok((identifier.clone(), identifier_token.span))
    } else {
        // If it's not an identifier, return an error.
        Err(SyntaxError::ExpectedAValidIdentifier(identifier_token.span))
    }
}

/// Parses a type token and returns a Type.
pub fn parse_type_token(parser: &mut Parser) -> Result<Type, SyntaxError> {
    // Parse the identifier token and create a Type from it.
    let (type_identifier, _) = parse_identifier_token(parser)?;
    Ok(Type::from(type_identifier))
}

/// Parses a single identifier with a corresponding type.
pub fn parse_single_typed_identifier(parser: &mut Parser) -> Result<TypedIdent, SyntaxError> {
    // Parse the identifier token and expect a colon afterwards.
    let (identifier, span) = parse_identifier_token(parser)?;
    parser.expect_token(TokenKind::Punctuation(Punctuation::Colon))?;
    // Parse the type token and create a TypedIdent from the identifier and type.
    let ty = parse_type_token(parser)?;
    Ok(TypedIdent {
        ident: Ident {
            name: identifier,
            span,
        },
        ty,
    })
}

/// Parses a delimited by comma list of items of type T.
pub fn parse_delimited<T>(
    parser: &mut Parser,
    start_punct: Punctuation,
    end_punct: Punctuation,
    parse_item: impl Fn(&mut Parser) -> Result<T, SyntaxError>,
) -> Result<Vec<T>, SyntaxError> {
    // Expect the start punctuation and initialize the items vector.
    parser.expect_token(TokenKind::Punctuation(start_punct))?;
    let mut items = vec![];
    // Parse items until the end punctuation is encountered or the end of the input is reached.
    while !parser.is_at_end() && parser.current_token().kind != end_punct {
        items.push(parse_item(parser)?);
        // Consume a comma if it is present.
        if !parser.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::Comma)) {
            break;
        }
    }
    // Expect the end punctuation.
    parser.expect_token(TokenKind::Punctuation(end_punct))?;
    Ok(items)
}

/// Parses a delimited list of TypedIdentifiers.
pub fn parse_typed_identifiers(
    parser: &mut Parser,
    expected_punctuation: Punctuation,
) -> Result<Vec<TypedIdent>, SyntaxError> {
    // Determine the start and end punctuation based on the expected punctuation.
    let (start_punct, end_punct) = match expected_punctuation {
        Punctuation::OpenBrace => (Punctuation::OpenBrace, Punctuation::CloseBrace),
        Punctuation::OpenParenthesis => {
            (Punctuation::OpenParenthesis, Punctuation::CloseParenthesis)
        }
        _ => {
            return Err(SyntaxError::Expected(
                "{' or '('".to_string(),
                parser.current_span(),
            ))
        }
    };
    // Parse the delimited list of TypedIdentifiers.
    parse_delimited(
        parser,
        start_punct,
        end_punct,
        parse_single_typed_identifier,
    )
}

/// Parses multiple expressions delimited by punctuation.
pub fn parse_multiple_expressions(
    parser: &mut Parser,
    expected_punctuation: Punctuation,
) -> Result<Vec<Expression>, SyntaxError> {
    let (start_punct, end_punct) = match expected_punctuation {
        Punctuation::OpenBrace => (Punctuation::OpenBrace, Punctuation::CloseBrace),
        Punctuation::OpenParenthesis => {
            (Punctuation::OpenParenthesis, Punctuation::CloseParenthesis)
        }
        _ => {
            return Err(SyntaxError::Expected(
                "{' or '('".to_string(),
                parser.current_span(),
            ))
        }
    };

    parse_delimited(parser, start_punct, end_punct, |p| p.parse_expression())
}

pub fn parse_body_expression(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let start_span = parser.current_span();
    if !parser.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::OpenBrace)) {
        return Err(SyntaxError::Expected('{'.to_string(), start_span));
    }

    let mut nodes = vec![];

    loop {
        if parser.is_at_end() {
            return Err(SyntaxError::Expected(
                '}'.to_string(),
                parser.current_span(),
            ));
        }

        // Stop at }
        if parser.consume_token_if_kind(&TokenKind::Punctuation(Punctuation::CloseBrace)) {
            break;
        }

        let node = parser.parse_node()?;
        nodes.push(node);
    }

    Ok(Expression::Body(nodes))
}
