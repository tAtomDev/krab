mod context;
mod eval_expression;
mod eval_node;
mod eval_statement;
mod result;

use crate::{error::RuntimeError, types::Value};
pub use context::Context;
pub use result::EvalResult;

pub trait Eval {
    fn eval(&self, context: &Context) -> Result<EvalResult, RuntimeError>;
}
