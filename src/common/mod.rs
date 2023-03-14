pub mod tokens;
mod value;

pub use value::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ControlFlow {
    Continue,
    Break,
}
