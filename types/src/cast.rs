use crate::generics::TypeConstraint;
use crate::Type;
use crate::FuncType;
use std::cmp;

pub mod prelude {
    pub use super::TypeCast;
    pub use super::try_cast;
    pub use super::FuncCallTypeCast;
    pub use super::get_type_casts_for_function_set;
    pub use super::CastType;
}

/// Enum describing the types of implicit casts available
#[derive(Debug, Clone, PartialEq)]
pub enum TypeCast{
    /// A free type widening that is legal in the system but costs nothing at runtime, e.g.
    /// from Type::IntLiteral(4) tp Type::Int
    FreeWiden,
    /// One of the int casts that we support. This is a cast from i32 to i64
    IntToBigIntWiden,
    /// One of the int casts that we support. This is a cast from i32 to f64
    IntToNumberWiden,
    /// Not possible to cast these types
    None,
    /// Used by some places to indicate that the types are exact matches and don't need 
    /// a cast.
    NotNeeded,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CastType{
    /// Implicit cast from normal flow of data
    Implicit,
    /// Explicit cast caused by an 'as' keyword
    Explicit,
    /// Force cast due to compiler internals. A generic runtime cast to __ptr
    GenericForce,
    /// Force cast due to compiler internals. A downcast because of a type guard
    GuardForce,
}

/// Try casting a type to another type.
pub fn try_cast(from: &Type, to: &Type, cast_type: CastType) -> TypeCast {
    if *from == *to {
        return TypeCast::NotNeeded;
    }

    if *from == Type::Unknown {
        // we can never widen from unknown
        return TypeCast::None;
    }

    if *from == Type::Never {
        // we can always cast from never, it's the bottom type
        return TypeCast::FreeWiden;
    }

    match to {
        //we can always widen to unknown, it's the top type
        Type::Unknown => TypeCast::FreeWiden,
        Type::Int => {
            match from {
                Type::IntLiteral(_) => TypeCast::FreeWiden,
                Type::UnsafeSizeT => if cast_type == CastType::Implicit { TypeCast::None } else {TypeCast::FreeWiden}
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
        Type::UnsafePtr => {
            match from {
                Type::IntLiteral(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::Int => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::UnsafeStruct{name: _} => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::UnsafeSizeT => TypeCast::FreeWiden,
                Type::UnsafeOption(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::UnsafeSome(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::UnsafeNull => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                _ => TypeCast::None,
            }
        },
        
        //again, this is wrong, but
        Type::UnsafeSizeT => {
            match from {
                Type::IntLiteral(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::Int => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::UnsafePtr => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                _ => TypeCast::None,
            }
        },

        Type::UnsafeSome(inner_to) => {
            match from {
                Type::UnsafeOption(inner_from) => if cast_type == CastType::GuardForce{ 
                    let inner_cast = try_cast(inner_from, inner_to, cast_type);
                    //this is a bit eww
                    if inner_cast == TypeCast::NotNeeded {
                        TypeCast::FreeWiden
                    } else {
                        inner_cast
                    }
                } else { TypeCast::None },
                Type::UnsafeSome(inner_from) => try_cast(inner_from, inner_to, cast_type),
                _ => TypeCast::None,
            }
        },

        Type::UnsafeOption(inner_to) => {
            match from {
                Type::UnsafeOption(inner_from) => try_cast(inner_from, inner_to, cast_type),
                Type::UnsafeNull => TypeCast::FreeWiden,
                Type::UnsafeSome(inner_from) => try_cast(inner_from, inner_to, cast_type),
                _ => TypeCast::None,
            }
        },

        Type::UnsafeStruct{name: _} => {
            match from {
                Type::UnsafePtr => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeWiden },
                Type::Int => if cast_type == CastType::GenericForce { TypeCast::FreeWiden } else { TypeCast::None },
                _ => TypeCast::None,
            }
        },

        Type::VariableUsage{name: _, constraint} => {
            match constraint{
                TypeConstraint::None => TypeCast::NotNeeded,
                TypeConstraint::IsAStruct => {
                    try_cast(from, &Type::UnsafeStruct{name: String::from("")}, cast_type)
                }
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
    let mut level_out = -1;

    for possible_func_type in possible_func_types {
        let mut this_arg_type_casts: Vec<TypeCast> = vec![];
        let mut idx = 0;
        let mut level = 3;
        // if the arg lengths are different, don't even bother
        if proposed_arg_types_len != possible_func_type.in_types.len() {
            continue;
        }

        for proposed_arg_type in proposed_arg_types {
            let wanted_type = possible_func_type.in_types.get(idx).unwrap();
            let type_cast = try_cast(proposed_arg_type, wanted_type, CastType::Implicit);
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
            3 => { 
                return Some(FuncCallTypeCast{arg_type_casts: this_arg_type_casts, func_type: possible_func_type.clone()});
            },
            1 | 2 => { 
                if level > level_out {
                    level_out = level;
                    out = Some(FuncCallTypeCast{arg_type_casts: this_arg_type_casts, func_type: possible_func_type.clone()});
                }
            },
            _ => {},
        }
    }

    out
}