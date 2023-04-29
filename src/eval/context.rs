use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

use crate::{
    ast::{Span, SpanInterval},
    runtime::Environment,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    pub span: SpanInterval,
    env: Rc<RefCell<Environment>>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            span: SpanInterval {
                start: Span::EOF,
                end: Span::EOF,
            },
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }
}

impl Context {
    pub const fn new(span: SpanInterval, env: Rc<RefCell<Environment>>) -> Self {
        Self { span, env }
    }

    pub fn env(&self) -> RefMut<Environment> {
        self.env.borrow_mut()
    }

    pub fn set_span(&self, span: SpanInterval) -> Self {
        Self {
            span,
            ..self.clone()
        }
    }
}
