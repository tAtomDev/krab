use crate::{
    ast::{tokens::Operator, Expression},
    error::ValueError,
    runtime::AccessModifier,
};

use super::*;

impl Eval for Expression {
    fn eval(&self, context: &Context) -> Result<EvalResult, RuntimeError> {
        let value = match self {
            Expression::Empty => Value::Unit,
            Expression::Literal(literal) => Value::from(literal.to_owned()),
            Expression::Identifier(ident) => {
                let env = context.env();
                let var = env
                    .get_variable(&ident.name)
                    .ok_or(RuntimeError::VariableNotFound(ident.clone(), ident.span))?;

                var.value.clone()
            }
            Expression::Call(ident, args) => {
                let mut env = context.env();
                let func = env
                    .get_function(&ident.name)
                    .ok_or(RuntimeError::FunctionNotFound(ident.clone(), ident.span))?
                    .clone();

                env.push_scope();
                drop(env);

                // Evaluate arguments
                let mut values = vec![];
                for arg in args.iter() {
                    values.push(arg.eval(context)?.try_parse_value(context.span.start)?);
                }

                if values.len() != func.signature.args.len() {
                    return Err(RuntimeError::IncorrectAmountOfArguments(
                        func.signature.args.len(),
                        values.len(),
                        ident.span,
                    ));
                }

                // Type-check arguments and load argument value into scope
                for (index, typed_arg) in func.signature.args.iter().enumerate() {
                    if typed_arg.ty != values[index].ty() {
                        return Err(RuntimeError::IncorrectArgumentType(
                            typed_arg.clone(),
                            ident.span,
                        ));
                    }

                    context.env().declare_variable(
                        AccessModifier::ReadWrite,
                        typed_arg.clone(),
                        values[index].clone(),
                    )?;
                }

                // Call function body
                let result = func
                    .body_expression
                    .eval(context)?
                    .try_parse_value(context.span.end)?;

                // Unload arguments
                context.env().pop_scope();

                result
            }
            Expression::Unary(op, rhs) => {
                let rhs = rhs.eval(context)?.try_parse_value(context.span.end)?;

                let value_result = match op {
                    Operator::Subtract => -rhs,
                    Operator::Not => !rhs,
                    _ => return Err(RuntimeError::InvalidUnaryOperator(*op, context.span.start)),
                };

                value_result.map_err(|e| RuntimeError::ValueError(e, context.span.start))?
            }
            Expression::Binary(lhs, op, rhs) => {
                let lhs = lhs.eval(context)?.try_parse_value(context.span.start)?;
                let rhs = rhs.eval(context)?.try_parse_value(context.span.end)?;

                let value_result = match op {
                    Operator::Add => lhs + rhs,
                    Operator::Subtract => lhs - rhs,
                    Operator::Multiply => lhs * rhs,
                    Operator::Divide => lhs / rhs,
                    Operator::Modulo => lhs % rhs,
                    Operator::Equal => Ok(Value::Boolean(lhs == rhs)),
                    Operator::NotEqual => Ok(Value::Boolean(lhs != rhs)),
                    Operator::And => Ok(Value::Boolean(
                        lhs.as_bool()
                            .map_err(|e| RuntimeError::ValueError(e, context.span.start))?
                            && rhs
                                .as_bool()
                                .map_err(|e| RuntimeError::ValueError(e, context.span.start))?,
                    )),
                    Operator::Or => Ok(Value::Boolean(
                        lhs.as_bool()
                            .map_err(|e| RuntimeError::ValueError(e, context.span.start))?
                            || rhs
                                .as_bool()
                                .map_err(|e| RuntimeError::ValueError(e, context.span.start))?,
                    )),
                    Operator::Greater => Ok(Value::Boolean(lhs > rhs)),
                    Operator::GreaterOrEqual => Ok(Value::Boolean(lhs >= rhs)),
                    Operator::Less => Ok(Value::Boolean(lhs < rhs)),
                    Operator::LessOrEqual => Ok(Value::Boolean(lhs <= rhs)),
                    _ => return Err(RuntimeError::InvalidBinaryOperator(*op, context.span.start)),
                };

                value_result.map_err(|e| RuntimeError::ValueError(e, context.span.start))?
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let Value::Boolean(condition_bool) = condition.eval(context)?.try_parse_value(context.span.start)? else {
                    return Err(RuntimeError::ValueError(ValueError::BooleanExpected, context.span.start));
                };

                if condition_bool {
                    return then_branch.eval(context);
                } else if let Some(else_branch) = else_branch {
                    return else_branch.eval(context);
                }

                Value::Unit
            }
            Expression::While { condition, body } => {
                let mut value = Value::Unit;
                loop {
                    if condition.eval(context)? != EvalResult::Value(Value::Boolean(true)) {
                        break;
                    }

                    let result = body.eval(context)?;
                    value = match result {
                        EvalResult::ControlFlow(..) => return Ok(result),
                        EvalResult::Value(value) => value,
                    };
                }

                value
            }
            Expression::Body(body) => {
                context.env().push_scope();
                let mut last = Value::Unit;
                for node in body {
                    last = match node.eval(context)? {
                        EvalResult::Value(value) => value,
                        EvalResult::ControlFlow(_flow, value) => return Ok(value.into()),
                    }
                }

                context.env().pop_scope();

                last
            }
        };

        Ok(value.into())
    }
}
