use crate::ast::{Node, NodeKind};

use super::*;

impl Eval for Node {
    fn eval(&self, context: &Context) -> Result<EvalResult, RuntimeError> {
        match &self.kind {
            NodeKind::Expression(expression) => expression.eval(&context.set_span(self.span)),
            NodeKind::Statement(statement) => statement.eval(&context.set_span(self.span)),
        }
    }
}
