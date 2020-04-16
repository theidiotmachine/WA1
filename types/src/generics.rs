use serde::{Serialize, Deserialize};
use crate::{Type, PTR_MAX, U_32_MAX, U_64_MAX, get_bittage, Bittage};

pub mod prelude {
    pub use super::TypeConstraint;
    pub use super::TypeArg;
    pub use super::matches_type_constraint;
    pub use super::get_runtime_type_for_generic;
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub enum TypeConstraint{
    None,
    IsAStruct,
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct TypeArg{
    pub name: String,
    pub constraint: TypeConstraint,
}

pub fn matches_type_constraint(
    arg_type: &Type,
    constraint: &TypeConstraint
) -> bool {
    match constraint {
        TypeConstraint::None => true,
        TypeConstraint::IsAStruct => {
            match arg_type {
                Type::UnsafeStruct{name: _} => true,
                _ => false
            }
        }
    }
}

pub fn get_runtime_type_for_generic(
    arg_type: &Type
) -> Option<Type> {
    match arg_type {
        Type::Bool => Some(Type::Int(0, U_32_MAX)),
        Type::FloatLiteral(_) => Some(Type::Number),
        Type::Int(lower, upper) => {
            let bittage = get_bittage(*lower, *upper);
            match bittage{
                Bittage::S32 | Bittage::U32 => Some(Type::Int(0, U_32_MAX)),
                Bittage::S64 | Bittage::U64 => Some(Type::Int(0, U_64_MAX)),
                Bittage::OOR => None
            }
        },
        Type::Number => Some(Type::Number),
        Type::RealVoid => Some(Type::RealVoid),
        Type::UnsafeOption(_) => Some(Type::Int(0, PTR_MAX)),
        Type::UnsafePtr => Some(Type::Int(0, PTR_MAX)),
        Type::UnsafeStruct{name: _} => Some(Type::Int(0, PTR_MAX)),
        _ => None
    }
}