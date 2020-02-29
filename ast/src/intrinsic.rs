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
    ///i32 count leading zeros
    I32Clz(Box<TypedExpr>),
    ///i32 count trailing zeros
    I32Ctz(Box<TypedExpr>),
    ///i64 count trailing zeros
    I64Ctz(Box<TypedExpr>),
    //left shift
    I32ShL(Box<TypedExpr>, Box<TypedExpr>),
    //signed right shift
    I32ShRS(Box<TypedExpr>, Box<TypedExpr>),
    //unsigned right shift
    I32ShRU(Box<TypedExpr>, Box<TypedExpr>),
}