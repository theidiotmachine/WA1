use crate::expr::TypedExpr;

pub mod prelude {
    pub use super::Intrinsic;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Intrinsic {
    ///memory.grow
    MemoryGrow(Box<TypedExpr>),
    ///memory.size
    MemorySize,
    ///unreachable
    Trap,
    ///i32 count trailing zeros
    I32Ctz(Box<TypedExpr>),
    ///i64 count trailing zeros
    I64Ctz(Box<TypedExpr>),
}