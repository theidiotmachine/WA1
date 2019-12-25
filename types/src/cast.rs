use crate::Type;
use crate::FuncType;
use std::cmp;

pub mod prelude {
    pub use super::TypeCast;
    pub use super::try_cast;
    pub use super::FuncCallTypeCast;
    pub use super::get_type_casts_for_function_set;
}

/// Enum describing the types of implicit casts available
#[derive(Debug, Clone, PartialEq)]
pub enum TypeCast{
    /// A free type widening that is legal in the system but costs nothing at runtime, e.g.
    /// from Type::IntLiteral(4) tp Type::Int
    FreeWiden,
    /// One of the int widenings that we support. This is a cast from i32 to i64
    IntToBigIntWiden,
    /// One of the int widenings that we support. This is a cast from i32 to f64
    IntToNumberWiden,
    /// Not possible to cast these types
    None,
    /// Used by some places to indicate that the types are exact matches and don't need 
    /// a cast.
    NotNeeded,
}

/// Try implicitly casting a type to another type.
pub fn try_cast(from: &Type, to: &Type) -> TypeCast {
    if *from == *to {
        return TypeCast::NotNeeded;
    }

    if *from == Type::Unknown {
        // we can never widen from unknwon
        return TypeCast::None;
    }

    match to {
        //we can always widen to unknown, it's the bottom type
        Type::Unknown => TypeCast::FreeWiden,
        Type::Int => {
            match from {
                Type::IntLiteral(_) => TypeCast::FreeWiden,
                _ => TypeCast::None,
            }
        },
        Type::Number => {
            match from {
                Type::IntLiteral(_) => TypeCast::IntToNumberWiden,
                Type::Int => TypeCast::IntToNumberWiden,
                Type::FloatLiteral(_) => TypeCast::FreeWiden,
                _ => TypeCast::None,
            }
        },
        Type::BigInt => {
            match from {
                Type::IntLiteral(_) => TypeCast::IntToBigIntWiden,
                Type::Int => TypeCast::IntToBigIntWiden,
                Type::BigIntLiteral(_) => TypeCast::FreeWiden,
                _ => TypeCast::None,
            }
        },
        Type::String => {
            match from {
                Type::StringLiteral(_) => TypeCast::FreeWiden,
                _ => TypeCast::None,
            }
        },
        //we kind of don't care about ptrs. If you are messing with them you better know what you are doing, so we let
        // you cast them from all sorts of things
        Type::Ptr(_) => {
            match from {
                Type::IntLiteral(_) => TypeCast::FreeWiden,
                Type::Int => TypeCast::FreeWiden,
                Type::Ptr(_) => TypeCast::FreeWiden,
                _ => TypeCast::None,
            }
        }
        _ => TypeCast::None,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallTypeCast{
    pub func_type: FuncType,
    pub arg_type_casts: Vec<TypeCast>,
}

/// For a set of possible function types, find one that we can use given the proposed arg types. We'll try to find 
/// one that doesn't require any casting; if we can't we will return one that can be casted to.
pub fn get_type_casts_for_function_set(possible_func_types: &Vec<FuncType>, proposed_arg_types: &Vec<Type>) -> Option<FuncCallTypeCast> {
    let mut out: Option<FuncCallTypeCast> = None;
    let proposed_arg_types_len = proposed_arg_types.len();

    for possible_func_type in possible_func_types {
        let mut this_arg_type_casts: Vec<TypeCast> = vec![];
        let mut idx = 0;
        let mut level = 2;
        // if the arg lengths are different, don't even bother
        if proposed_arg_types_len != possible_func_type.in_types.len() {
            continue;
        }

        for proposed_arg_type in proposed_arg_types {
            let wanted_type = possible_func_type.in_types.get(idx).unwrap();
            let type_cast = try_cast(proposed_arg_type, wanted_type);
            match type_cast {
                TypeCast::NotNeeded => this_arg_type_casts.push(type_cast),
                TypeCast::FreeWiden => {
                    level = cmp::min(level, 2);
                    this_arg_type_casts.push(type_cast);
                },
                TypeCast::IntToBigIntWiden => {
                    level = cmp::min(level, 1);
                    this_arg_type_casts.push(type_cast);
                },
                TypeCast::IntToNumberWiden => {
                    level = cmp::min(level, 1);
                    this_arg_type_casts.push(type_cast);
                },
                TypeCast::None => {
                    level = 0;
                    break;
                }
            }
            
            idx += 1;
        }
        match level {
            2 => { 
                return Some(FuncCallTypeCast{arg_type_casts: this_arg_type_casts, func_type: possible_func_type.clone()});
            },
            1 => { 
                out = Some(FuncCallTypeCast{arg_type_casts: this_arg_type_casts, func_type: possible_func_type.clone()});
            },
            _ => {},
        }
    }

    out
}