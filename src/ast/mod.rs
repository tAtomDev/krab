#![allow(clippy::module_inception)]
use std::fmt::Display;

mod ast;
mod expression;
mod node;
mod statement;
pub mod tokens;

pub use ast::AST;
pub use expression::Expression;
pub use node::{Node, NodeKind};
pub use statement::Statement;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpanInterval {
    pub start: Span,
    pub end: Span,
}

impl SpanInterval {
    pub const EOF: SpanInterval = SpanInterval {
        start: Span::EOF,
        end: Span::EOF,
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub loc: u32,
    pub pos: u32,
    pub absolute_pos: u32,
}

impl Span {
    pub const EOF: Span = Span::new(0, 0, 0);

    pub const fn new(loc: u32, pos: u32, absolute_pos: u32) -> Span {
        Span {
            loc,
            pos,
            absolute_pos,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.pos == 0 && self.loc == 0 {
            write!(f, "EOF")
        } else {
            write!(f, "({}:{})", self.loc, self.pos)
        }
    }
}
