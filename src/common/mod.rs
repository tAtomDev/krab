pub mod tokens;
mod ty;
mod value;

use std::fmt::Display;

pub use ty::Type;
pub use value::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ControlFlow {
    Continue,
    Break,
    Return,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum SpanKind {
    Default,
    Eof,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub loc: usize,
    kind: SpanKind,
}

impl Span {
    pub const ZERO: Span = Span {
        start: 0,
        end: 0,
        loc: 0,
        kind: SpanKind::Default,
    };
    pub const EOF: Span = Span {
        start: 0,
        end: 0,
        loc: 0,
        kind: SpanKind::Eof,
    };

    pub const fn new(start: usize, end: usize, loc: usize) -> Span {
        Span {
            start,
            end,
            loc,
            kind: SpanKind::Default,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            SpanKind::Default => write!(f, "line {}, pos {}", self.loc, self.start),
            SpanKind::Eof => write!(f, "EOF"),
        }
    }
}
