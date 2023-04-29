use crate::{ast::Statement, runtime::AccessModifier, types::ControlFlow};

use super::*;

impl Eval for Statement {
    fn eval(&self, context: &Context) -> Result<EvalResult, RuntimeError> {
        match self {
            Statement::VariableDeclaration(typed_ident, expression) => {
                let value = expression
                    .eval(context)?
                    .try_parse_value(context.span.start)?;
                context.env().declare_variable(
                    AccessModifier::ReadWrite,
                    typed_ident.clone(),
                    value,
                )?;
            }
            Statement::FunctionDeclaration(signature, body_expression) => {
                context
                    .env()
                    .declare_function(signature.clone(), body_expression.clone())?;
            }
            Statement::Assignment(ident, expression) => {
                let value = expression
                    .eval(context)?
                    .try_parse_value(context.span.start)?;

                context.env().assign_variable(ident.clone(), value)?;
            }
            Statement::Return(expression) => {
                let value = expression
                    .eval(context)?
                    .try_parse_value(context.span.start)?;

                return Ok(EvalResult::ControlFlow(ControlFlow::Return, value));
            }
            Statement::Expression(expression) => {
                expression.eval(context)?;
            }
        };

        Ok(Value::Unit.into())
    }
}
