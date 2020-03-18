use serde::{Serialize, Deserialize};
use crate::Type;

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
        Type::BigInt => Some(Type::BigInt),
        Type::BigIntLiteral(_) => Some(Type::BigInt),
        Type::Boolean => Some(Type::Int),
        Type::FloatLiteral(_) => Some(Type::Number),
        Type::Int => Some(Type::Int),
        Type::IntLiteral(_) => Some(Type::Int),
        Type::Number => Some(Type::Number),
        Type::RealVoid => Some(Type::RealVoid),
        Type::UnsafeOption(_) => Some(Type::UnsafePtr),
        Type::UnsafePtr => Some(Type::UnsafePtr),
        Type::UnsafeSizeT => Some(Type::UnsafeSizeT),
        Type::UnsafeStruct{name: _} => Some(Type::UnsafePtr),
        _ => None
    }
}