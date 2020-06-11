use crate::{Type, FullType, PassStyle, Mutability};
use crate::FuncType;
use crate::{Bittage, get_bittage, PTR_MAX, U_32_MAX, S_32_MAX, S_32_MIN, S_64_MAX, S_64_MIN, U_64_MAX, INT_U_64, INT_U_32, INT_S_64, INT_S_32};
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
    pub use super::{upcast_from_literal};
}

/// Enum describing the types of implicit casts available
#[derive(Debug, Clone, PartialEq)]
pub enum TypeCast{
    /// This is an upcast that costs nothing at runtime.
    FreeUpcast(FullType),
    /// One of the int casts that we support. This is a cast from a narrower int to a wider int. This may or may not be a thing at runtime.
    IntWiden(i128, i128),
    /// One of the int casts that we support. This is a cast from i32 to f64 and will definitely have runtime cost.
    IntToNumberWiden,
    /// Fail because of a const cast
    ConstFail,
    /// Not possible to cast these types
    None,
    /// Used by some places to indicate that the types are exact matches and don't need 
    /// a cast.
    NotNeeded,
    /// Means we must make a copy, because of const issues
    Clone,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CastType{
    /// Implicit cast from normal flow of data
    Implicit,
    /// Explicit cast caused by an 'as' keyword
    Explicit,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum ConstCast{
    Fail,
    Pass,
    Clone
}

fn const_check(from_pass_style: PassStyle, to_pass_style: PassStyle, from_mutability: Mutability, to_mutability: Mutability) -> ConstCast {
    //you can't pass a const reference to a mut reference
    if (to_pass_style == PassStyle::Reference || to_pass_style == PassStyle::ValueHoldingReference) && to_mutability == Mutability::Mut && from_mutability == Mutability::Const && from_pass_style != PassStyle::AtomicValue {
        ConstCast::Fail
    //the object must be duplicated
    } else if to_pass_style == PassStyle::Value && from_pass_style == PassStyle::Value && to_mutability == Mutability::Mut && from_mutability == Mutability::Const {
        ConstCast::Clone
    //if we don't know, assume the worst
    } else if (to_pass_style == PassStyle::Unknown || from_pass_style == PassStyle::Unknown) && to_mutability == Mutability::Mut && from_mutability == Mutability::Const {
        ConstCast::Fail
    } else {
        ConstCast::Pass
    }
}

///const check - you can't pass a mut reference to a const reference
pub fn const_fail(from_pass_style: PassStyle, to_pass_style: PassStyle, from_mutability: Mutability, to_mutability: Mutability) -> bool {
    (to_pass_style == PassStyle::Reference || to_pass_style == PassStyle::ValueHoldingReference) && to_mutability == Mutability::Mut && from_mutability == Mutability::Const && from_pass_style != PassStyle::AtomicValue
}

/// Try casting a type to another type.
pub fn try_cast(from: &FullType, to: &FullType, cast_type: CastType) -> TypeCast {
    if *from == *to {
        return TypeCast::NotNeeded;
    }

    let to_pass_style = to.get_pass_style();
    let from_pass_style = from.get_pass_style();

    let const_cast = const_check(from_pass_style, to_pass_style, from.mutability, to.mutability);
    if const_cast == ConstCast::Fail {
        return TypeCast::ConstFail;
    }

    let from_type = &from.r#type;
    let to_type = &to.r#type;

    if from_type == to_type {
        return match const_cast {
            ConstCast::Clone => TypeCast::Clone,
            ConstCast::Pass => TypeCast::NotNeeded,
            ConstCast::Fail => TypeCast::None
        };
    }

    if *from_type == Type::Unknown {
        // we can never widen from unknown
        return TypeCast::None;
    }

    if *from_type == Type::Never {
        // we can always cast from never, it's the bottom type
        return TypeCast::FreeUpcast(to.clone());
    }

    match to_type {
        //we can always widen to unknown, it's the top type
        Type::Unknown => TypeCast::FreeUpcast(FullType::new(&Type::Unknown, from.mutability)),
        Type::Int(lower_to, upper_to) => {
            match from_type {
                Type::Int(lower_from, upper_from) => {
                    if lower_from >= lower_to && upper_from <= upper_to { TypeCast::IntWiden(*lower_to, *upper_to) } else { TypeCast::None }
                },
                Type::UnsafePtr => {
                    if cast_type == CastType::Explicit && *lower_to == 0 && *upper_to == PTR_MAX {
                        TypeCast::FreeUpcast(FullType::new(&Type::UnsafePtr, from.mutability))
                    } else {
                        TypeCast::None
                    }
                },
                _ => TypeCast::None,
            }
        },
        Type::Number => {
            match from_type {
                Type::Int(lower_from, lower_to) => if *lower_from >= -9007199254740991 && *lower_to <= 9007199254740991 { TypeCast::IntToNumberWiden } else { TypeCast::None},
                Type::FloatLiteral(_) => TypeCast::FreeUpcast(FullType::new(&Type::Number, from.mutability)),
                _ => TypeCast::None,
            }
        },
        Type::String => {
            match from_type {
                Type::StringLiteral(_) => TypeCast::FreeUpcast(FullType::new(&Type::String, from.mutability)),
                _ => TypeCast::None,
            }
        },
        //we kind of don't care about ptrs. If you are messing with them you better know what you are doing, so we let
        // you cast them from all sorts of things
        Type::UnsafePtr => {
            match from_type {
                Type::Int(lower_from, upper_from) => if cast_type == CastType::Implicit || *lower_from < 0 || *upper_from > PTR_MAX { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeStruct{name: _} => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeOption(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeSome(_) => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                Type::UnsafeNull => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                _ => TypeCast::None,
            }
        },

        Type::UnsafeSome(inner_to) => {
            match from_type {
                Type::UnsafeSome(inner_from) => 
                    try_cast(&FullType::new(inner_from, from.mutability), &FullType::new(inner_to, to.mutability), cast_type),
                _ => TypeCast::None,
            }
        },

        Type::UnsafeOption(inner_to) => {
            match from_type {
                Type::UnsafeOption(inner_from) => try_cast(&FullType::new(inner_from, from.mutability), &FullType::new(inner_to, to.mutability), cast_type),
                Type::UnsafeNull => TypeCast::FreeUpcast(to.clone()),
                Type::UnsafeSome(inner_from) => try_cast(&FullType::new(inner_from, from.mutability), &FullType::new(inner_to, to.mutability), cast_type),
                _ => TypeCast::None,
            }
        },

        Type::UnsafeStruct{name: _} => {
            match from_type {
                Type::UnsafePtr => if cast_type == CastType::Implicit { TypeCast::None } else { TypeCast::FreeUpcast(to.clone()) },
                _ => TypeCast::None,
            }
        },

        Type::VariableUsage{name: to_name, constraint: to_constraint} => {
            if to_constraint.contains_trait(&String::from("IsAStruct")) {
                try_cast(from, &FullType::new(&Type::UnsafeStruct{name: String::from("")}, to.mutability), cast_type)
            } else {
                match from_type {
                    Type::VariableUsage{name: from_name, constraint: from_constraint} => {
                        if from_name == to_name {
                            match const_cast {
                                ConstCast::Clone => TypeCast::Clone,
                                ConstCast::Pass => TypeCast::NotNeeded,
                                ConstCast::Fail => TypeCast::None
                            }
                        } else {
                            if to_constraint.is_subset_of(from_constraint) {
                                return match const_cast {
                                    ConstCast::Clone => TypeCast::Clone,
                                    ConstCast::Pass => TypeCast::FreeUpcast(to.clone()),
                                    ConstCast::Fail => TypeCast::None
                                };
                                
                            } else {
                                TypeCast::None
                            }
                        }
                    },
                    _ => TypeCast::None
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

pub fn try_generic_wrap(from: &FullType, to: &FullType) -> GenericCast{
    if *from == *to {
        return GenericCast::NotNeeded;
    }

    let from_pass_style = to.get_pass_style();
    let to_pass_style = to.get_pass_style();

    if const_fail(from_pass_style, to_pass_style, from.mutability, to.mutability) {
        return GenericCast::None;
    }

    let from_type = &from.r#type;
    let to_type = &to.r#type;

    if from_type == to_type {
        return GenericCast::NotNeeded;
    }

    match to_type {
        Type::Int(lower_to, upper_to) => {
            match from_type {
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
                Type::Bool => 
                    if *lower_to == 0 && *upper_to == U_32_MAX { GenericCast::FreeCast } else { GenericCast::None },
                _ => GenericCast::None
            }
        },
        Type::UnsafeOption(t) => {
            match from_type {
                Type::UnsafeOption(f) | Type::UnsafeSome(f) => try_generic_wrap(&FullType::new(&f, from.mutability), &FullType::new(&t, to.mutability)),
                _ => GenericCast::None
            }
        },
        Type::UnsafeSome(t) => {
            match from_type {
                Type::UnsafeSome(f) => try_generic_wrap(&FullType::new(&f, from.mutability), &FullType::new(&t, to.mutability)),
                _ => GenericCast::None
            }
        },
        _ => GenericCast::None
    }
}

pub fn try_generic_unwrap(from: &FullType, to: &FullType) -> GenericCast{
    if *from == *to {
        return GenericCast::NotNeeded;
    }

    let from_pass_style = to.get_pass_style();
    let to_pass_style = to.get_pass_style();

    if const_fail(from_pass_style, to_pass_style, from.mutability, to.mutability) {
        return GenericCast::None;
    }

    let from_type = &from.r#type;
    let to_type = &to.r#type;

    if from_type == to_type {
        return GenericCast::NotNeeded;
    }

    match to_type {
        Type::UnsafePtr | Type::UnsafeNull | Type::UnsafeStruct{name: _} => {
            match from_type {
                Type::Int(lower_from, upper_from) => {
                    if *lower_from < 0 || *upper_from > PTR_MAX { GenericCast::None } else {GenericCast::FreeCast}
                },
                _ => GenericCast::None
            }
        },
        Type::Int(lower_to, upper_to) => {
            match from_type {
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
        Type::Bool => {
            match from_type {
                Type::Int(lower_from, upper_from) =>
                    if *lower_from == 0 && *upper_from == U_32_MAX { GenericCast::FreeCast } else { GenericCast::None },
                _ => GenericCast::None
            }
        },
        Type::UnsafeOption(t) => {
            match from_type {
                Type::UnsafeOption(f) => try_generic_unwrap(&FullType::new(&f, from.mutability), &FullType::new(&t, to.mutability)),
                Type::Int(lower_from, upper_from) => {
                    if *lower_from < 0 || *upper_from > PTR_MAX { GenericCast::None } else {GenericCast::FreeCast}
                },
                _ => GenericCast::None
            }
        },
        Type::UnsafeSome(t) => {
            match from_type {
                Type::UnsafeSome(f) => try_generic_unwrap(&FullType::new(&f, from.mutability), &FullType::new(&t, to.mutability)),
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

pub fn try_guard_downcast_expr(from: &FullType, to: &FullType) -> TypeGuardDowncast {
    if *from == *to {
        return TypeGuardDowncast::NotNeeded;
    }

    let from_pass_style = to.get_pass_style();
    let to_pass_style = to.get_pass_style();

    if const_fail(from_pass_style, to_pass_style, from.mutability, to.mutability) {
        return TypeGuardDowncast::None;
    }

    let from_type = &from.r#type;
    let to_type = &to.r#type;

    if from_type == to_type {
        return TypeGuardDowncast::NotNeeded;
    }

    match to_type {
        Type::UnsafeSome(inner_to) => {
            match from_type {
                Type::UnsafeOption(inner_from) => { 
                    let inner_cast = try_guard_downcast_expr(&FullType::new(&inner_from, from.mutability), &FullType::new(&inner_to, to.mutability));
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

        Type::UnsafeNull => {
            match from_type {
                Type::UnsafeOption(_) => {
                    TypeGuardDowncast::FreeDowncast
                },
                _ => TypeGuardDowncast::None,
            }
        },

        Type::Int(_, _) => {
            match from_type {
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
pub fn get_type_casts_for_function_set(possible_func_types: &Vec<FuncType>, proposed_arg_types: &Vec<FullType>) -> Option<FuncCallTypeCast> {
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
                TypeCast::FreeUpcast(_) | TypeCast::Clone => {
                    level = cmp::min(level, 3);
                    this_arg_type_casts.push(type_cast);
                },
                TypeCast::IntWiden(lower, upper) => {
                    level = cmp::min(level, 2);
                    let this_int_widen_cost = match proposed_arg_type.r#type{
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
                TypeCast::None | TypeCast::ConstFail => {
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

/// Used when we widen a variable. The problem here is if you have 
/// ```
/// let mut a = 3
/// ```
/// Then a will end up being of type Int<3,3> which is not much use to anyone. So we widen to the closest type available.
pub fn upcast_from_literal(t: &Type) -> Type {
    match t {
        Type::Int(lower_from, upper_from) => {
            if lower_from == upper_from {
                if *upper_from > U_64_MAX {
                    t.clone()
                } else if *upper_from > S_64_MAX {
                    INT_U_64
                } else if *upper_from > U_32_MAX {
                    INT_S_64
                } else if *upper_from > S_32_MAX {
                    INT_U_32
                } else if *upper_from > S_32_MIN {
                    INT_S_32
                } else if *upper_from > S_64_MIN {
                    INT_S_64
                } else {
                    t.clone()
                }
            } else {
                t.clone()
            }
        },
        Type::UnsafeSome(s) => Type::UnsafeOption(s.clone()),
        _ => t.clone()
    }
}