pub mod tokens;
mod ty;
mod value;

pub use ty::Type;
pub use value::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ControlFlow {
    Continue,
    Break,
}