use crate::generics::TypeConstraint;
use crate::Type;
use crate::FuncType;
use crate::{Bittage, get_bittage, PTR_MAX, U_32_MAX};
use std::cmp;
use std::{i128};

pub mod prelude {
    pub use super::TypeCast;
    pub use super::try_cast;
    pub use super::FuncCallTypeCast;
    pub use super::{get_type_casts_for_function_set};
    pub use super::CastType;
    pub use super::TypeGuardDowncast;
    pub use super::try_guard_downcast_expr;
    pub use super::GenericCast;
    pub use super::{try_generic_unwrap, try_generic_wrap};
}

/// Enum describing the types of implicit casts available
#[derive(Debug, Clone, PartialEq)]
pub enum TypeCast{
    /// This is an upcast that costs nothing at runtime.
    FreeUpcast(Type),
    /// One of the int casts that we support. This is a cast from a narrower int to a wider int. This may or may not be a thing at runtime.
    IntWiden(i128, i128),
    /// One of the int casts that we support. This is a cast from i32 to f64 and will definitely have runtime cost.
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
        return TypeCast::FreeUpcast(to.clone());
    }

    match to {
        //we can always widen to unknown, it's the top type
        Type::Unknown => TypeCast::FreeUpcast(Type::Unknown),
        Type::Int(lower_to, upper_to) => {
            match from {
                Type::Int(lower_from, upper_from) => {
                    if lower_from >= lower_to && upper_from <= upper_to { TypeCast::IntWiden(*lower_to, *upper_to) } else { TypeCast::None }
                },
                Type::UnsafePtr => {
                    if cast_type == CastType::Explicit && *lower_to == 0 && *upper_to == PTR_MAX {
                        TypeCast::FreeUpcast(Type::UnsafePtr)
                    } else {
                        TypeCast::None
                    }
                },
                _ => TypeCast::None,
            }
        },
        Type::Number => {
            match from {
                Type::Int(lower_from, lower_to) => if *lower_from >= -9007199254740991 && *lower_to <= 9007199254740991 { TypeCast::IntToNumberWiden } else { TypeCast::None},
                Type::FloatLiteral(_) => TypeCast::FreeUpcast(Type::Number),
                _ => TypeCast::None,
            }
        },
        Type::String => {
            match from {
                Type::StringLiteral(_) => TypeCast::FreeUpcast(Type::String),
                _ => TypeCast::None,
            }
        },
        //we kind of don't care about ptrs. If you are messing with them you better know what you are doing, so we let
        // you cast them from all sorts of things
        Type::UnsafePtr => {
            match from {
                Type::Int(lower_from, upper_from) => if cast_type == CastType::Implicit || *lower_from < 0 || *upper_from > PTR_MAX { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeStruct{name: _} => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeOption(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeSome(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeNull => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                _ => TypeCast::None,
            }
        },

        Type::UnsafeSome(inner_to) => {
            match from {
                Type::UnsafeSome(inner_from) => try_cast(inner_from, inner_to, cast_type),
                _ => TypeCast::None,
            }
        },

        Type::UnsafeOption(inner_to) => {
            match from {
                Type::UnsafeOption(inner_from) => try_cast(inner_from, inner_to, cast_type),
                Type::UnsafeNull => TypeCast::FreeUpcast(to.clone()),
                Type::UnsafeSome(inner_from) => try_cast(inner_from, inner_to, cast_type),
                _ => TypeCast::None,
            }
        },

        Type::UnsafeStruct{name: _} => {
            match from {
                Type::UnsafePtr => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
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

/// Enum describing the types of implicit casts available
#[derive(Debug, Clone, PartialEq)]
pub enum GenericCast{
    /// This is a cast that costs nothing at runtime.
    FreeCast,
    /// Not possible to cast these types
    None,
    /// Used by some places to indicate that the types are exact matches and don't need 
    /// a cast.
    NotNeeded,
}

pub fn try_generic_wrap(from: &Type, to: &Type) -> GenericCast{
    if *from == *to {
        return GenericCast::NotNeeded;
    }

    match to {
        Type::Int(lower_to, upper_to) => {
            match from {
                //FIXME64BIT
                Type::UnsafePtr | Type::UnsafeNull | Type::UnsafeOption(_) | Type::UnsafeSome(_) | Type::UnsafeStruct{name: _} => 
                    if *lower_to < 0 || *upper_to > PTR_MAX { GenericCast::None } else {GenericCast::FreeCast},
                Type::Int(lower_from, upper_from) => {
                    let bittage_from = get_bittage(*lower_from, *upper_from);
                    let bitage_to = get_bittage(*lower_to, *upper_to);
                    match bittage_from{
                        Bittage::S32 | Bittage::U32 => match bitage_to {
                            Bittage::S32 | Bittage::U32 => GenericCast::FreeCast,
                            Bittage::S64 | Bittage::U64 | Bittage::OOR => GenericCast::None,
                        },
                        Bittage::S64 | Bittage::U64 => match bitage_to {
                            Bittage::S64 | Bittage::U64 => GenericCast::FreeCast,
                            Bittage::S32 | Bittage::U32 | Bittage::OOR => GenericCast::None,
                        },
                        Bittage::OOR => GenericCast::None
                    }
                },
                Type::Boolean => 
                    if *lower_to == 0 && *upper_to == U_32_MAX { GenericCast::FreeCast } else { GenericCast::None },
                _ => GenericCast::None
            }
        },
        Type::UnsafeOption(t) => {
            match from {
                Type::UnsafeOption(f) | Type::UnsafeSome(f) => try_generic_wrap(&f, &t),
                _ => GenericCast::None
            }
        },
        Type::UnsafeSome(t) => {
            match from {
                Type::UnsafeSome(f) => try_generic_wrap(&f, &t),
                _ => GenericCast::None
            }
        },
        _ => GenericCast::None
    }
}

pub fn try_generic_unwrap(from: &Type, to: &Type) -> GenericCast{
    if *from == *to {
        return GenericCast::NotNeeded;
    }

    match to {
        Type::UnsafePtr | Type::UnsafeNull | Type::UnsafeStruct{name: _} => {
            match from {
                Type::Int(lower_from, upper_from) => {
                    if *lower_from < 0 || *upper_from > PTR_MAX { GenericCast::None } else {GenericCast::FreeCast}
                },
                _ => GenericCast::None
            }
        },
        Type::Int(lower_to, upper_to) => {
            match from {
                Type::Int(lower_from, upper_from) => {
                    let bittage_from = get_bittage(*lower_from, *upper_from);
                    let bitage_to = get_bittage(*lower_to, *upper_to);
                    match bittage_from{
                        Bittage::S32 | Bittage::U32 => match bitage_to {
                            Bittage::S32 | Bittage::U32 => GenericCast::FreeCast,
                            Bittage::S64 | Bittage::U64 | Bittage::OOR => GenericCast::None,
                        },
                        Bittage::S64 | Bittage::U64 => match bitage_to {
                            Bittage::S64 | Bittage::U64 => GenericCast::FreeCast,
                            Bittage::S32 | Bittage::U32 | Bittage::OOR => GenericCast::None,
                        },
                        Bittage::OOR => GenericCast::None
                    }
                },
                _ => GenericCast::None
            }
        },
        Type::Boolean => {
            match from {
                Type::Int(lower_from, upper_from) =>
                    if *lower_from == 0 && *upper_from == U_32_MAX { GenericCast::FreeCast } else { GenericCast::None },
                _ => GenericCast::None
            }
        },
        Type::UnsafeOption(t) => {
            match from {
                Type::UnsafeOption(f) => try_generic_unwrap(&f, &t),
                Type::Int(lower_from, upper_from) => {
                    if *lower_from < 0 || *upper_from > PTR_MAX { GenericCast::None } else {GenericCast::FreeCast}
                },
                _ => GenericCast::None
            }
        },
        Type::UnsafeSome(t) => {
            match from {
                Type::UnsafeSome(f) => try_generic_unwrap(&f, &t),
                Type::Int(lower_from, upper_from) => {
                    if *lower_from < 0 || *upper_from > PTR_MAX { GenericCast::None } else {GenericCast::FreeCast}
                },
                _ => GenericCast::None
            }
        },
        _ => GenericCast::None
    }

}

/// Enum describing the types of implicit casts available
#[derive(Debug, Clone, PartialEq)]
pub enum TypeGuardDowncast{
    /// A free type narrowing that costs nothing at runtime
    FreeDowncast,
    /// Not possible to cast these types
    None,
    /// Used by some places to indicate that the types are exact matches and don't need 
    /// a cast.
    NotNeeded,
}

pub fn try_guard_downcast_expr(from: &Type, to: &Type) -> TypeGuardDowncast {
    if *from == *to {
        return TypeGuardDowncast::NotNeeded;
    }

    match to {
        Type::UnsafeSome(inner_to) => {
            match from {
                Type::UnsafeOption(inner_from) => { 
                    let inner_cast = try_guard_downcast_expr(inner_from, inner_to);
                    //this is a bit eww
                    if inner_cast == TypeGuardDowncast::NotNeeded {
                        TypeGuardDowncast::FreeDowncast
                    } else {
                        inner_cast
                    }
                },
                _ => TypeGuardDowncast::None,
            }
        },

        Type::Int(_, _) => {
            match from {
                Type::Int(_,_) => TypeGuardDowncast::FreeDowncast,
                _ => TypeGuardDowncast::None,
            }
        },

        _ => TypeGuardDowncast::None,
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
    let mut int_widen_cost_out: i128 = i128::MAX;

    for possible_func_type in possible_func_types {
        let mut this_arg_type_casts: Vec<TypeCast> = vec![];
        let mut idx = 0;
        let mut level = 3;
        let mut int_widen_cost: i128 = 0;
        // if the arg lengths are different, don't even bother
        if proposed_arg_types_len != possible_func_type.in_types.len() {
            continue;
        }

        for proposed_arg_type in proposed_arg_types {
            let wanted_type = possible_func_type.in_types.get(idx).unwrap();
            let type_cast = try_cast(proposed_arg_type, wanted_type, CastType::Implicit);
            match type_cast {
                TypeCast::NotNeeded => this_arg_type_casts.push(type_cast),
                TypeCast::FreeUpcast(_) => {
                    level = cmp::min(level, 3);
                    this_arg_type_casts.push(type_cast);
                },
                TypeCast::IntWiden(lower, upper) => {
                    level = cmp::min(level, 2);
                    let this_int_widen_cost = match proposed_arg_type{
                        Type::Int(lower_from, upper_from) => {
                            //the multiply is to make signed more likely than unsigned
                            2 * (upper - upper_from) + (lower_from - lower)
                        },
                        _ => unreachable!()     
                    };
                    int_widen_cost = cmp::max(this_int_widen_cost, int_widen_cost);

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
            2 => {
                if level >= level_out && int_widen_cost < int_widen_cost_out {
                    level_out = level;
                    int_widen_cost_out = int_widen_cost; 
                    out = Some(FuncCallTypeCast{arg_type_casts: this_arg_type_casts, func_type: possible_func_type.clone()});
                }
            },
            1 => { 
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