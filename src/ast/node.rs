use std::fmt::Debug;

use super::{Expression, SpanInterval, Statement};

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Node {
    pub kind: NodeKind,
    pub span: SpanInterval,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum NodeKind {
    Statement(Statement),
    Expression(Expression),
}

// <Node> == <NodeKing>
impl PartialEq<NodeKind> for Node {
    fn eq(&self, other: &NodeKind) -> bool {
        other == &self.kind
    }
}

// <Node> == <Expresion>
impl PartialEq<Expression> for Node {
    fn eq(&self, other: &Expression) -> bool {
        match &self.kind {
            NodeKind::Expression(expr) => *expr == *other,
            _ => false,
        }
    }
}

// <Node> == <Statement>
impl PartialEq<Statement> for Node {
    fn eq(&self, other: &Statement) -> bool {
        match &self.kind {
            NodeKind::Statement(expr) => *expr == *other,
            _ => false,
        }
    }
}
