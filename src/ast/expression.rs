use crate::{
    error::{LogicalError, SyntaxError},
    prelude::TypeCache,
    types::{Ident, Type},
    util::is_vec_all_same,
};

use super::{tokens::*, Node, NodeKind, Span, Statement};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expression {
    Empty,
    Literal(Literal),
    Identifier(Ident),
    Call(Ident, Vec<Expression>),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    Body(Vec<Node>),
}

impl Expression {
    pub fn try_parse_type(
        &self,
        type_cache: &mut TypeCache,
        span: Span,
    ) -> Result<Option<Type>, SyntaxError> {
        let ty = match self {
            Expression::Empty => return Ok(None),
            Expression::Literal(literal) => literal.ty(),
            Expression::Binary(lhs, op, rhs) => {
                let lhs = lhs.try_parse_type(type_cache, span)?;
                let rhs = rhs.try_parse_type(type_cache, span)?;

                if !op.is_logical() && lhs != rhs {
                    return Ok(None);
                }

                lhs.ok_or(SyntaxError::InvalidType(span))?
            }
            Expression::Unary(_, expression) => return expression.try_parse_type(type_cache, span),
            Expression::Identifier(identifier) => {
                let Some((_, _, ty)) = type_cache.get_variable(&identifier.name) else {
                    return Err(LogicalError::VariableDoesNotExists(identifier.name.to_owned(), span).into());
                };

                return Ok(Some(ty));
            }
            Expression::Call(identifier, _args) => {
                let Some((_, ty, _)) = type_cache.get_function(&identifier.name) else {
                    return Err(LogicalError::FunctionDoesNotExists(identifier.name.to_owned(), span).into());
                };

                return Ok(Some(ty));
            }
            Expression::If { .. } => {
                let branches = self.branch_types(type_cache, span)?;
                if branches.is_empty() || !is_vec_all_same(&branches) {
                    return Err(SyntaxError::IncorrectExpressionBranchTypes(branches, span));
                }

                branches[0].clone()
            }
            Expression::While { body, .. } => return body.try_parse_type(type_cache, span),
            Expression::Body(_) => {
                let branches = self.branch_types(type_cache, span)?;
                if !is_vec_all_same(&branches) {
                    return Err(SyntaxError::IncorrectExpressionBranchTypes(branches, span));
                }

                if branches.is_empty() {
                    return Ok(None);
                }

                branches[0].clone()
            }
        };

        Ok(Some(ty))
    }

    pub fn branch_types(
        &self,
        type_cache: &mut TypeCache,
        span: Span,
    ) -> Result<Vec<Type>, SyntaxError> {
        let mut types = vec![];

        match self {
            Expression::If {
                then_branch,
                else_branch,
                ..
            } => {
                let then_branch = then_branch
                    .try_parse_type(type_cache, span)?
                    .unwrap_or(Type::Unit);
                let else_branch = if let Some(else_branch) = else_branch {
                    else_branch
                        .try_parse_type(type_cache, span)?
                        .unwrap_or(Type::Unit)
                } else {
                    Type::Unit
                };

                types.push(then_branch);
                types.push(else_branch);
            }
            Expression::Body(nodes) => {
                let return_expressions = nodes.iter().filter_map(|n| {
                    if let NodeKind::Statement(Statement::Return(expression)) = &n.kind {
                        Some((expression, n.span.start))
                    } else {
                        None
                    }
                });

                for (expr, span) in return_expressions {
                    if let Some(ty) = expr.try_parse_type(type_cache, span)? {
                        types.push(ty);
                    }
                }

                if let Some(last_node) = nodes.last() {
                    if let NodeKind::Expression(expr) = &last_node.kind {
                        if let Some(ty) = expr.try_parse_type(type_cache, span)? {
                            types.push(ty);
                        }
                    }
                }
            }
            _ => unimplemented!(),
        };

        Ok(types)
    }
}
