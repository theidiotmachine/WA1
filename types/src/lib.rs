use std::{u64};
use serde::{Serialize, Deserialize};

pub mod cast;
pub mod generics;
use cast::get_type_casts_for_function_set;
use cast::FuncCallTypeCast;
use cast::{TypeCast, CastType};
use cast::try_cast;
use generics::TypeConstraint;
use std::collections::HashMap;

pub mod prelude {
    pub use super::Type;
    pub use super::OpType;
    pub use super::FuncType;
    pub use super::Privacy;
    pub use super::{get_unary_op_type_cast, UnOpTypeCast};
    pub use super::{get_binary_op_type_cast, get_type_from_type_cast};
    pub use super::StructType;
    pub use super::StructMember;
    pub use super::cast::prelude::*;
    pub use super::generics::prelude::*;
    pub use super::{S_32_MAX, S_32_MIN, S_64_MAX, S_64_MIN, U_32_MAX, U_64_MAX, PTR_MAX};
    pub use super::{SIZE_T, INT_S_64, INT_S_32, INT_U_32, INT_U_64};
    pub use super::{Bittage, get_bittage, get_literal_bittage};
}

pub const S_64_MAX: i128 = 9_223_372_036_854_775_807;
pub const S_64_MIN: i128 = -9_223_372_036_854_775_808;
pub const U_64_MAX: i128 = 18_446_744_073_709_551_615;
pub const S_32_MAX: i128 = 2_147_483_647;
pub const S_32_MIN: i128 = -2_147_483_648;
pub const U_32_MAX: i128 = 4_294_967_295;
//FIXME64BIT
pub const PTR_MAX: i128 = 4_294_967_295;

pub enum Bittage{
    S32,
    S64,
    U32,
    U64,
    OOR,
}

pub fn get_bittage(lower: i128, upper: i128) -> Bittage {
    if lower >= 0 {
        if upper <= U_32_MAX {
            Bittage::U32
        } else if upper <= U_64_MAX {
            Bittage::U64
        } else {
            Bittage::OOR
        }
    } else {
        if lower >= S_32_MIN && upper <= S_32_MAX {
            Bittage::S32
        } else if lower >= S_64_MIN && upper <= S_64_MAX {
            Bittage::S64
        } else {
            Bittage::OOR
        }
    }
}

pub fn get_literal_bittage(v: i128) -> Bittage {
    if v <= S_32_MAX {
        if v >= S_32_MIN {
            Bittage::S32
        } else if v >= S_64_MIN {
            Bittage::S64
        } else {
            Bittage::OOR
        }
    } else if v <= U_32_MAX {
        Bittage::U32
    } else if v <= S_64_MAX {
        Bittage::S64
    } else if v <= U_64_MAX {
        Bittage::U64
    } else {
        Bittage::OOR
    }
}


use std::fmt::Display;
use std::fmt::Formatter;

// Type of a function.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FuncType{
    pub out_type: Type,
    pub in_types: Vec<Type>,
}

impl Display for FuncType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut vec: Vec<String> = vec![];
        for inner in &self.in_types{
            vec.push(format!("{}", inner));
        }
        write!(f, "({}) => {}", vec.join(","), self.out_type) 
    }
}

/// Type of an operator. These are used by the inference engine.
#[derive(Debug, Clone, PartialEq)]
pub enum OpType{
    /// The type of a simple operator that works like a function.
    SimpleOpType(Vec<FuncType>),
    /// The type of an assignment operator. rhs must be the same type as lhs, rv is same type as lhs.
    AssignmentOpType,
    /// The type of an assign and modify operator. This is, say +=. We only have an array of types
    /// here because they represent the various things the lhs can be; the rhs must be cast to
    /// that.
    AssignModifyOpType(Vec<Type>),
    /// The type of an equality or inequality operator. In my very simple type system, lhs must be the 
    /// same type as rhs; return value is boolean.
    EqualityOpType,
    /// Not implemented yet!
    NotImplementedOpType,
    ///the cast operator
    AsOpType,
    ///the static member operator, '.'
    StaticMemberOpType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Privacy{
    Public, Private, Protected
}

/// Data member of a struct.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructMember{
    pub name: String,
    pub r#type: Type,
}

/// user-defined struct. 
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructType{
    pub members: Vec<StructMember>
}

impl StructType {
    pub fn get_member_type_map(&self) -> HashMap<String, Type> {
        let mut out: HashMap<String, Type> = HashMap::new();
        for m in &self.members {
            out.insert(m.name.clone(), m.r#type.clone());
        }
        out
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    /// unit type that actually manifests as nothing
    RealVoid,
    /// unit type that is created at run time
    FakeVoid,
    /// top type
    Unknown,
    /// bottom type
    Never,
    ///f64 number
    Number,
    ///string
    String,
    ///Array
    Array(Box<Type>),
    ///integer type. First number is lower bound, inclusive. Second number is upper bound, inclusive 
    Int(i128, i128),
    ///boolean
    Bool,
    ///Func
    Func{func_type: Box<FuncType>},
    ///Tuple
    Tuple(Vec<Type>),
    /// untyped object literal - can be cast to a matching struct or class
    ObjectLiteral(HashMap<String, Type>),
    /// boxed type
    Any,
    /// Options. This is built into the type system because I want a zero-cost abstraction of a null pointer.
    Option(Box<Type>),
    /// The unsafe option type. May one day become the full option type.
    UnsafeOption(Box<Type>),
    /// The unsafe option type some
    UnsafeSome(Box<Type>),
    /// The unsafe option type none
    UnsafeNull,
    /// Option - some
    Some(Box<Type>),
    /// user type
    UserType{name: String, type_args: Vec<Type>},
    /// user struct type
    UnsafeStruct{name: String},
    /// not yet known - will be filled in by the type system
    Undeclared,
    ///Unresolved type variable
    VariableUsage{name: String, constraint: TypeConstraint},
    ///numeric literal of type 'number'
    FloatLiteral(f64),
    /// string literal
    StringLiteral(String),
    ///ptr - internal type. Param is alignment
    UnsafePtr,
    ///type literal. Not something that will ever appear at runtime, but is useful for parts of the AST
    TypeLiteral(Box<Type>),
    ///Type of a module. Not something that will ever appear at runtime.
    ModuleLiteral(String),
    /// __array type
    UnsafeArray(Box<Type>),
}

fn get_mangled_names(types: &Vec<Type>)->String{
    let mut vec: Vec<String> = vec![];
    for inner in types {
        vec.push(format!("{}", inner.get_mangled_name()));
    }
    vec.join(",")
}

impl Type{
    pub fn is_undeclared(&self) -> bool {
        match &self {
            Type::Undeclared => true,
            _ => false
        }
    }

    ///Is this an unresolved type variable?
    pub fn is_type_variable(&self) -> bool {
        match &self {
            Type::Any | Type::Bool | Type::FakeVoid | Type::FloatLiteral(_) | Type::Int(_, _)
                | Type::ModuleLiteral(_) | Type::Never | Type::Number | Type::RealVoid | Type::String | Type::StringLiteral(_) | Type::Undeclared 
                | Type::Unknown | Type::UnsafeNull | Type::UnsafePtr | Type::UnsafeStruct{name:_} 
                    => false,
            Type::Array(t) | Type::Option(t) | Type::Some(t) | Type::TypeLiteral(t) | Type::UnsafeArray(t) | Type::UnsafeOption(t) 
                | Type::UnsafeSome(t) => t.is_type_variable(),
            Type::Func{func_type: ft} => ft.out_type.is_type_variable() || ft.in_types.iter().any(|t| t.is_type_variable()),
            Type::ObjectLiteral(oles) => oles.iter().any(|ole| ole.1.is_type_variable()),
            Type::UserType{name:_, type_args: ts} | Type::Tuple(ts) => ts.iter().any(|t| t.is_type_variable()),
            Type::VariableUsage{name: _, constraint: _} => true,
        }
    }

    pub fn get_func_type(&self) -> Option<&FuncType> {
        match &self {
            Type::Func{func_type} => Some(func_type),
            _ => None
        }
    }

    pub fn get_mangled_name(&self) -> String {
        match self {
            Type::RealVoid => String::from("!Void"),
            Type::FakeVoid => String::from("!fakeVoid"),
            Type::Unknown => String::from("!Unknown"),
            Type::Never => String::from("!Never"),
            Type::Number => String::from("!Number"),
            Type::String => String::from("!String"),
            Type::Array(inner) => format!("!Array<{}>", inner.get_mangled_name()),
            Type::Int(lower, upper) => format!("!Int<{}, {}>", lower, upper),
            Type::Bool => String::from("!Bool"),
            Type::Func{func_type} => format!("!func_{}", func_type),
            Type::Tuple(types) => format!("!Tuple<{}>", get_mangled_names(types)),
            Type::Any => String::from("!Any"),
            Type::Option(inner) => format!("!Option<{}>", inner.get_mangled_name()),
            Type::UnsafeOption(inner) => format!("!__Option<{}>", inner.get_mangled_name()),
            Type::UnsafeSome(inner) => format!("!__Some<{}>", inner.get_mangled_name()),
            Type::UnsafeNull => format!("!__Null"),
            Type::Some(inner) => format!("!Some<{}>", inner.get_mangled_name()),
            Type::UserType{name, type_args} => format!("!type_{}<{}>", name, get_mangled_names(type_args)),
            Type::UnsafeStruct{name} => format!("!__struct_{}", name),
            Type::Undeclared => format!("!Undeclared"),
            Type::VariableUsage{name, constraint: _} => format!("!var_{}", name),
            Type::UnsafePtr => format!("!__Ptr"),
            Type::FloatLiteral(n) => format!("!fl_{}", n),
            Type::StringLiteral(n) => format!("!sl_\"{}\"", n),
            Type::ObjectLiteral(_) => format!("!ol_{{}}"),
            Type::TypeLiteral(inner) => format!("!TypeLiteral_{}", inner.get_mangled_name()),
            Type::UnsafeArray(inner) => format!("!__Array<{}>", inner.get_mangled_name()),
            Type::ModuleLiteral(name) => format!("!ModuleLiteral_{}", name),
        }
    }
}

fn display_types(types: &Vec<Type>)->String{
    let mut vec: Vec<String> = vec![];
    for inner in types {
        vec.push(format!("{}", inner));
    }
    vec.join(",")
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::RealVoid => write!(f, "Void"),
            Type::FakeVoid => write!(f, "Void"),
            Type::Unknown => write!(f, "Unknown"),
            Type::Never => write!(f, "Never"),
            Type::Number => write!(f, "Number"),
            Type::String => write!(f, "String"),
            Type::Array(inner) => write!(f, "Array<{}>", inner),
            Type::Int(lower, upper) => write!(f, "Int<{}, {}>", lower, upper),
            Type::Bool => write!(f, "Bool"),
            Type::Func{func_type} => write!(f, "{}", func_type),
            Type::Tuple(types) => write!(f, "Tuple<{}>", display_types(types)),
            Type::Any => write!(f, "Any"),
            Type::Option(inner) => write!(f, "Option<{}>", inner),
            Type::UnsafeOption(inner) => write!(f, "__Option<{}>", inner),
            Type::UnsafeSome(inner) => write!(f, "__Some<{}>", inner),
            Type::UnsafeNull => write!(f, "__Null"),
            Type::Some(inner) => write!(f, "Some<{}>", inner),
            Type::UserType{name, type_args} => write!(f, "{}<{}>", name, display_types(type_args)),
            Type::UnsafeStruct{name} => write!(f, "{}", name),
            Type::Undeclared => write!(f, "Undeclared"),
            Type::VariableUsage{name, constraint: _} => write!(f, "{}", name),
            Type::UnsafePtr => write!(f, "__Ptr"),
            Type::FloatLiteral(n) => write!(f, "{}", n),
            Type::StringLiteral(n) => write!(f, "\"{}\"", n),
            Type::ObjectLiteral(_) => write!(f, "{{}}"),
            Type::TypeLiteral(t) => write!(f, "type: {}", t),
            Type::UnsafeArray(t) => write!(f, "__Array<{}>", t),
            Type::ModuleLiteral(n) => write!(f, "module: {}", n),
        }
    }
}

pub const INT_S_64: Type = Type::Int(S_64_MIN, S_64_MAX);
pub const INT_S_32: Type = Type::Int(S_32_MIN, S_32_MAX);
pub const INT_U_64: Type = Type::Int(0, U_64_MAX);
pub const INT_U_32: Type = Type::Int(0, U_32_MAX);
pub const SIZE_T: Type = Type::Int(0, PTR_MAX);

pub struct UnOpTypeCast{
    pub r#type: Type,
    pub type_cast: TypeCast,
}

pub fn get_type_from_type_cast(type_cast: &TypeCast, orig_type: &Type) -> Type {
    match type_cast{
        TypeCast::NotNeeded => orig_type.clone(),
        TypeCast::FreeUpcast(to) => to.clone(),
        TypeCast::IntWiden(to_lower, to_upper) => Type::Int(*to_lower, *to_upper),
        TypeCast::IntToNumberWiden => Type::Number,
        TypeCast::None => {
            //if we get a return from get_type_casts_for_function_set it won't have a None
            unreachable!()
        }
    }
}

/// Given an operator type, and the type of the operand, find the type of
/// the resulting intermediate value.
pub fn get_unary_op_type_cast(op_type: &OpType, operand_type: &Type) -> Option<UnOpTypeCast>{
    match op_type {
        // given unary operators can only be simple, maybe I should change this
        OpType::SimpleOpType(func_types) => {
            let t = get_type_casts_for_function_set(&func_types, &vec![operand_type.clone()]);
            match t {
                None => None,
                Some(FuncCallTypeCast{func_type, arg_type_casts}) => {
                    let out_type_cast = arg_type_casts.get(0).unwrap().clone();
                    let out_type = get_type_from_type_cast(&out_type_cast, &func_type.in_types.get(0).unwrap());
                    Some(UnOpTypeCast{r#type: out_type, type_cast: out_type_cast})
                },
            }
        },
        _ => None
    }
}

pub struct BinOpTypeCast{
    pub lhs_type: Type,
    pub lhs_type_cast: TypeCast,
    pub rhs_type: Type,
    pub rhs_type_cast: TypeCast,
    pub out_type: Type,
}

/// Given a binary operator type, and the types of the operands, find the type
/// of the resulting intermediate value.
pub fn get_binary_op_type_cast(op_type: &OpType, lhs_type: &Type, rhs_type: &Type) -> Option<BinOpTypeCast> {
    match op_type {
        //simple binary operator like +.
        OpType::SimpleOpType(func_types) => {
            //we treat this guy like a function.
            let t = get_type_casts_for_function_set(&func_types, &vec![lhs_type.clone(), rhs_type.clone()]);
            match t {
                None => None,
                Some(FuncCallTypeCast{func_type, arg_type_casts}) => {
                    let out_lhs_type_cast = arg_type_casts.get(0).unwrap().clone();
                    let out_lhs_type = get_type_from_type_cast(&out_lhs_type_cast, &func_type.in_types.get(0).unwrap());

                    let out_rhs_type_cast = arg_type_casts.get(1).unwrap().clone();
                    let out_rhs_type = get_type_from_type_cast(&out_rhs_type_cast, &func_type.in_types.get(1).unwrap());

                    Some(BinOpTypeCast{lhs_type: out_lhs_type, lhs_type_cast: out_lhs_type_cast.clone(), rhs_type: out_rhs_type, rhs_type_cast: out_rhs_type_cast.clone(), out_type: func_type.out_type.clone()})
                }
            }
        },

        //assignment.
        OpType::AssignmentOpType => {
            //this is simple; either rhs can be cast to lhs or it can't
            let type_cast = try_cast(rhs_type, lhs_type, CastType::Implicit);
            let out_rhs_type = match &type_cast{
                TypeCast::NotNeeded | TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) => lhs_type.clone(),
                TypeCast::IntToNumberWiden => Type::Number,
                TypeCast::None => {
                    return None;
                }
            };
            Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: out_rhs_type, rhs_type_cast: type_cast, out_type: lhs_type.clone()})
        },

        // these guys are things like |=. 
        OpType::AssignModifyOpType(types) => {
            //First, find if the lhs is one of the permitted types
            let yes = types.iter().find(|t| *t == lhs_type).is_some();
            if yes {
                //if it is, see if the rhs can be cast to the lhs
                let type_cast = try_cast(rhs_type, lhs_type, CastType::Implicit);
                let out_rhs_type = match &type_cast{
                    TypeCast::NotNeeded | TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) => lhs_type.clone(),
                    TypeCast::IntToNumberWiden => Type::Number,
                    TypeCast::None => {
                        return None;
                    }
                };
                Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: out_rhs_type, rhs_type_cast: type_cast, out_type: lhs_type.clone()})
            } else {
                None
            }
        },

        //full equality (or inequality)
        OpType::EqualityOpType => {
            //try casting from one to the other. lhs to rhs first
            let type_cast = try_cast(lhs_type, &rhs_type, CastType::Implicit);
            match &type_cast{
                TypeCast::NotNeeded => 
                    Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: Type::Bool}),
                TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) | TypeCast::IntToNumberWiden => 
                    Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: type_cast, rhs_type: lhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: Type::Bool}),
                TypeCast::None => {
                    //now try going the other way
                    let type_cast = try_cast(rhs_type, &lhs_type, CastType::Implicit);
                    match &type_cast{
                        TypeCast::NotNeeded => 
                            Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: Type::Bool}),
                        TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) | TypeCast::IntToNumberWiden =>
                            Some(BinOpTypeCast{lhs_type: rhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: type_cast, out_type: Type::Bool}),
                        TypeCast::None => None
                    }    
                }
            }
        },

        //this is just a simple cast, so no need for all the fancy bin op stuff
        OpType::AsOpType => None,

        //member casting is done somewhere else
        OpType::StaticMemberOpType => None, 

        OpType::NotImplementedOpType => None,
    }
}
