use crate::expr::TypedExpr;

pub mod prelude {
    pub use super::Intrinsic;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Intrinsic {
    MemoryGrow(Box<TypedExpr>),
    MemorySize,
    Trap,
    I32Ctz(Box<TypedExpr>),
    I64Ctz(Box<TypedExpr>),
}