use std::fmt::Debug;

use crate::{
    error::RuntimeError,
    eval::{Context, Eval},
    types::Value,
};

use super::Node;

#[derive(Clone, PartialEq, PartialOrd)]
pub struct AST {
    pub name: String,
    pub nodes: Vec<Node>,
}

impl AST {
    pub fn new(name: &str) -> AST {
        AST {
            name: name.into(),
            nodes: Vec::new(),
        }
    }

    pub fn eval_ast(&self, context: Context) -> Result<Value, RuntimeError> {
        let mut value = Value::Unit;

        for node in self.nodes.iter() {
            let context = context.set_span(node.span);
            value = node.eval(&context)?.try_parse_value(node.span.end)?;
        }

        Ok(value)
    }

    pub fn push(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

impl Debug for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            self.nodes
                .iter()
                .map(|n| n.kind.clone())
                .collect::<Vec<_>>()
        )
    }
}
