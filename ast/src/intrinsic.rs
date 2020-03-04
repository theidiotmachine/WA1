use serde::{Serialize, Deserialize};

use crate::expr::TypedExpr;

pub mod prelude {
    pub use super::Intrinsic;
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Intrinsic {
    ///memory.grow
    MemoryGrow(Box<TypedExpr>),
    ///memory.size
    MemorySize,
    ///unreachable
    Trap,
}