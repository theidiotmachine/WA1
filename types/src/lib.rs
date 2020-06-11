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
    pub use super::{Type, Mutability, FullType, PassStyle};
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
    pub out_type: FullType,
    pub in_types: Vec<FullType>,
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
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
    UserType{name: String, type_args: Vec<Type>, inner: Box<Type>},
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
    TypeLiteral(Box<FullType>),
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
            Type::Array(t) | Type::Option(t) | Type::Some(t) | Type::UnsafeArray(t) | Type::UnsafeOption(t) 
                | Type::UnsafeSome(t) => t.is_type_variable(),
            Type::TypeLiteral(t) => t.r#type.is_type_variable(),
            Type::Func{func_type: ft} => ft.out_type.r#type.is_type_variable() || ft.in_types.iter().any(|t| t.r#type.is_type_variable()),
            Type::ObjectLiteral(oles) => oles.iter().any(|ole| ole.1.is_type_variable()),
            Type::UserType{name:_, type_args: ts, inner} => ts.iter().any(|t| t.is_type_variable()) || inner.is_type_variable(),
            Type::Tuple(ts) => ts.iter().any(|t| t.is_type_variable()),
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
            Type::UserType{name, type_args, inner: _} => format!("!type_{}<{}>", name, get_mangled_names(type_args)),
            Type::UnsafeStruct{name} => format!("!__struct_{}", name),
            Type::Undeclared => format!("!Undeclared"),
            Type::VariableUsage{name, constraint: _} => format!("!var_{}", name),
            Type::UnsafePtr => format!("!__Ptr"),
            Type::FloatLiteral(n) => format!("!fl_{}", n),
            Type::StringLiteral(n) => format!("!sl_\"{}\"", n),
            Type::ObjectLiteral(_) => format!("!ol_{{}}"),
            Type::TypeLiteral(inner) => format!("!TypeLiteral_{}", inner.r#type.get_mangled_name()),
            Type::UnsafeArray(inner) => format!("!__Array<{}>", inner.get_mangled_name()),
            Type::ModuleLiteral(name) => format!("!ModuleLiteral_{}", name),
        }
    }

    pub fn get_pass_style(&self) -> PassStyle{ 
        match self {
            Type::Any | Type::Func{func_type: _} | Type::ObjectLiteral(_) => panic!(),
            Type::Array(inner) | Type::Option(inner) | Type::Some(inner) | Type::UnsafeOption(inner) | Type::UnsafeSome(inner) => {
                match inner.get_pass_style() {
                    PassStyle::Value | PassStyle::AtomicValue => PassStyle::Value,
                    PassStyle::Reference | PassStyle::ValueHoldingReference => PassStyle::ValueHoldingReference,
                    PassStyle::Unknown => PassStyle::Unknown,
                }
            },
            Type::Bool | Type::FakeVoid | Type::FloatLiteral(_) | Type::Int(_, _) | Type::ModuleLiteral(_) | Type::Never 
                | Type::Number | Type::RealVoid | Type::TypeLiteral(_) | Type::Undeclared 
                | Type::UnsafeNull => PassStyle::AtomicValue,
            Type::String | Type::StringLiteral(_) => PassStyle::Value,
            Type::Tuple(inners) => {
                inners.iter().fold(PassStyle::Value, |acc, x| {
                    match x.get_pass_style() {
                        PassStyle::Value | PassStyle::AtomicValue => acc,
                        PassStyle::Reference | PassStyle::ValueHoldingReference => PassStyle::ValueHoldingReference,
                        PassStyle::Unknown => PassStyle::Unknown,
                    }
                })
            },
            Type::Unknown | Type::VariableUsage{name: _, constraint: _} => PassStyle::Unknown,
            Type::UnsafeArray(_) | Type::UnsafeStruct{name: _} => PassStyle::Reference,
            Type::UnsafePtr => PassStyle::Reference, //strictly wrong - the pointer is by value, but a pointer is pointing to something 
            Type::UserType{name: _, type_args: _, inner} => inner.get_pass_style()
        }
    }

    pub fn get_type_name(&self) -> String{
        match self {
            Type::Any => String::from("Any"),
            Type::Array(_) => String::from("Array"),
            Type::Bool => String::from("Bool"),
            Type::FakeVoid => String::from("FakeVoid"),
            Type::FloatLiteral(_) => String::from("FloatLiteral"),
            Type::Func{func_type} => {
                let in_names: Vec<String> = func_type.in_types.iter().map(|x| x.r#type.get_type_name()).collect();
                let out_name = func_type.out_type.r#type.get_type_name();
                format!("Fn ({}) -> {}", in_names.join(", "), out_name)
            },
            Type::Int(_, _) => String::from("Int"),
            Type::Never => String::from("Never"),
            Type::Number => String::from("Number"),
            Type::ModuleLiteral(_) => String::from("ModuleLiteral"),
            Type::ObjectLiteral(_) => String::from("ObjectLiteral"),
            Type::Option(_) => String::from("Option"),
            Type::RealVoid => String::from("Void"),
            Type::Some(_) => String::from("Some"),
            Type::String => String::from("String"),
            Type::StringLiteral(_) => String::from("StringLiteral"),
            Type::Tuple(_) => String::from("Tuple"),
            Type::TypeLiteral(_) => String::from("TypeLiteral"),
            Type::Undeclared => String::from("Undeclared"),
            Type::Unknown => String::from("Unknown"),
            Type::UnsafeArray(_) => String::from("__Array"),
            Type::UnsafeNull => String::from("__Null"),
            Type::UnsafeOption(_) => String::from("__Option"),
            Type::UnsafePtr => String::from("__Ptr"),
            Type::UnsafeSome(_) => String::from("__Some"),
            Type::UnsafeStruct{name} => name.clone(),
            Type::UserType{name, type_args: _, inner: _} => name.clone(),
            Type::VariableUsage{name, constraint: _} => name.clone(),
        }
    }

    pub fn get_type_args(&self) -> Vec<Type> {
        match self {
            Type::Any => vec![],
            Type::Array(inner) => vec![(**inner).clone()],
            Type::Bool => vec![],
            Type::FakeVoid => vec![],
            Type::FloatLiteral(_) => vec![],
            Type::Func{func_type: _} => vec![],
            Type::Int(_, _) => vec![],
            Type::ModuleLiteral(_) => vec![],
            Type::Never => vec![],
            Type::Number => vec![], 
            Type::ObjectLiteral(_) => vec![], 
            Type::Option(inner) => vec![(**inner).clone()], 
            Type::RealVoid => vec![], 
            Type::Some(inner) => vec![(**inner).clone()], 
            Type::String => vec![],
            Type::StringLiteral(_) => vec![],
            Type::Tuple(inner) => inner.clone(),
            Type::TypeLiteral(_) => vec![],
            Type::Undeclared => vec![], 
            Type::Unknown => vec![], 
            Type::UnsafeArray(inner) => vec![(**inner).clone()], 
            Type::UnsafeNull => vec![], 
            Type::UnsafeOption(inner) => vec![(**inner).clone()], 
            Type::UnsafePtr => vec![], 
            Type::UnsafeSome(inner) => vec![(**inner).clone()], 
            Type::UnsafeStruct{name: _} => vec![], 
            Type::UserType{name: _, type_args, inner: _} => type_args.clone(),
            Type::VariableUsage{name: _, constraint: _} => vec![],
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
            Type::UserType{name, type_args, inner: _} => write!(f, "{}<{}>", name, display_types(type_args)),
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

///How a type is passed.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
pub enum PassStyle{
    ///Passed by value. May be true pass by value (e.g. tuples) or synthetic pass by value (e.g. arrays)
    Value,
    ///Passed by value with no changeable internal components. Ints, bools, numbers.
    AtomicValue,
    ///Passed by reference. Obviously we pass a pointer through for these.
    Reference,
    ///This is data passed by value that holds data to be passed by reference. An example might be an array holding a ref class.
    ValueHoldingReference,
    ///We can't tell what the pass style is. We assume the worst.
    Unknown,
}

///Whether a type is const or mut
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
pub enum Mutability {
    Const,
    Mut,
    Unknown
}

impl Display for Mutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Mutability::Const => write!(f, ""),
            Mutability::Mut => write!(f, "mut"),
            Mutability::Unknown => write!(f, "?"),
        }
    }
}

///A type with a mutability modifier.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FullType{
    pub r#type: Type,
    pub mutability: Mutability,
}

impl FullType{
    pub fn get_pass_style(&self) -> PassStyle{ 
        self.r#type.get_pass_style()
    }

    pub fn new(t: &Type, m: Mutability) -> FullType { FullType{r#type: t.clone(), mutability: m }}
    pub fn new_const(t: &Type) -> FullType { FullType{r#type: t.clone(), mutability: Mutability::Const }}
    pub fn new_mut(t: &Type) -> FullType { FullType{r#type: t.clone(), mutability: Mutability::Mut}}
}

impl Display for FullType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.mutability, self.r#type)
    }
}

pub const INT_S_64: Type = Type::Int(S_64_MIN, S_64_MAX);
pub const INT_S_32: Type = Type::Int(S_32_MIN, S_32_MAX);
pub const INT_U_64: Type = Type::Int(0, U_64_MAX);
pub const INT_U_32: Type = Type::Int(0, U_32_MAX);
pub const SIZE_T: Type = Type::Int(0, PTR_MAX);

pub struct UnOpTypeCast{
    pub r#type: FullType,
    pub type_cast: TypeCast,
}

pub fn get_type_from_type_cast(type_cast: &TypeCast, orig_type: &FullType) -> FullType {
    match type_cast{
        TypeCast::NotNeeded => orig_type.clone(),
        TypeCast::FreeUpcast(to) => to.clone(),
        TypeCast::IntWiden(to_lower, to_upper) => FullType::new(&Type::Int(*to_lower, *to_upper), orig_type.mutability),
        TypeCast::IntToNumberWiden => FullType::new(&Type::Number, orig_type.mutability),
        TypeCast::None | TypeCast::ConstFail => {
            //if we get a return from get_type_casts_for_function_set it won't have a None
            unreachable!()
        },
        TypeCast::Clone => FullType::new_mut(&orig_type.r#type),
    }
}

/// Given an operator type, and the type of the operand, find the type of
/// the resulting intermediate value.
pub fn get_unary_op_type_cast(op_type: &OpType, operand_type: &FullType) -> Option<UnOpTypeCast>{
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
    pub lhs_type: FullType,
    pub lhs_type_cast: TypeCast,
    pub rhs_type: FullType,
    pub rhs_type_cast: TypeCast,
    pub out_type: FullType,
}

/// Given a binary operator type, and the types of the operands, find the type
/// of the resulting intermediate value.
pub fn get_binary_op_type_cast(op_type: &OpType, lhs_type: &FullType, rhs_type: &FullType) -> Option<BinOpTypeCast> {
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
                TypeCast::NotNeeded | TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) | TypeCast::Clone => lhs_type.clone(),
                TypeCast::IntToNumberWiden => FullType::new(&Type::Number, rhs_type.mutability),
                TypeCast::None | TypeCast::ConstFail => {
                    return None;
                }
            };
            Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: out_rhs_type, rhs_type_cast: type_cast, out_type: lhs_type.clone()})
        },

        // these guys are things like |=. 
        OpType::AssignModifyOpType(types) => {
            //First, find if the lhs is one of the permitted types
            let yes = types.iter().find(|t| **t == lhs_type.r#type).is_some();
            if yes {
                //if it is, see if the rhs can be cast to the lhs
                let type_cast = try_cast(rhs_type, lhs_type, CastType::Implicit);
                let out_rhs_type = match &type_cast{
                    TypeCast::NotNeeded | TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) | TypeCast::Clone => lhs_type.clone(),
                    TypeCast::IntToNumberWiden => FullType::new(&Type::Number, rhs_type.mutability),
                    TypeCast::None | TypeCast::ConstFail => {
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
                    Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: FullType::new_const(&Type::Bool)}),
                TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) | TypeCast::IntToNumberWiden | TypeCast::Clone => 
                    Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: type_cast, rhs_type: lhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: FullType::new_const(&Type::Bool)}),
                TypeCast::None | TypeCast::ConstFail => {
                    //now try going the other way
                    let type_cast = try_cast(rhs_type, &lhs_type, CastType::Implicit);
                    match &type_cast{
                        TypeCast::NotNeeded => 
                            Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: FullType::new_const(&Type::Bool)}),
                        TypeCast::FreeUpcast(_) | TypeCast::IntWiden(_,_) | TypeCast::IntToNumberWiden | TypeCast::Clone =>
                            Some(BinOpTypeCast{lhs_type: rhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: type_cast, out_type: FullType::new_const(&Type::Bool)}),
                        TypeCast::None | TypeCast::ConstFail => None
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
