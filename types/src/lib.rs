pub mod cast;
use cast::get_type_casts_for_function_set;
use cast::FuncCallTypeCast;
use cast::TypeCast;
use cast::try_cast;
use std::collections::HashMap;

pub mod prelude {
    pub use super::Type;
    pub use super::OpType;
    pub use super::FuncType;
    pub use super::ClassType;
    pub use super::ClassMember;
    pub use super::Privacy;
    pub use super::get_unary_op_type;
    pub use super::get_binary_op_type_cast;
    pub use super::AbstractTypeDecl;
    pub use super::StructType;
    pub use super::StructMember;
    pub use super::cast::prelude::*;
}

use std::fmt::Display;
use std::fmt::Formatter;

// Type of a function.
#[derive(Debug, Clone, PartialEq)]
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
    /// The type of an assign and modify operator. This is, say +=. We onlt have an array of types
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Privacy{
    Public, Private, Protected
}

/// Data memeber of a class.
#[derive(Debug, Clone, PartialEq)]
pub struct ClassMember{
    pub name: String,
    pub r#type: Type,
    pub privacy: Privacy,
}

/// User-defined class type. Only contains members at the moment.
#[derive(Debug, Clone, PartialEq)]
pub struct ClassType{
    pub members: Vec<ClassMember>,
}

/// Data member of a struct.
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember{
    pub name: String,
    pub r#type: Type,
}

/// user-defined struct. 
#[derive(Debug, Clone, PartialEq)]
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

/// This is part of the type system rewrite. I am not sure it's correct yet.
/// This is the body of a type function that consumes type args and returns a type.
#[derive(Debug, Clone, PartialEq)]
pub enum AbstractTypeBody{
    VariableUse(String),
    Array(Box<AbstractTypeBody>),
    Func(Vec<AbstractTypeBody>, Box<AbstractTypeBody>),
    UserType,
    Number,
    String,
    Boolean,
    Any,
    Unknown, //???
    RealVoid,
    FakeVoid,
    Never,
}

/// This is part of the type system rewrite. I am not sure it's correct yet.
/// This is a type function that conumes type args and returns a type.
#[derive(Debug, Clone, PartialEq)]
pub struct AbstractTypeDecl{
    pub args: Vec<String>,
    pub out: AbstractTypeBody,
}

#[derive(Debug, Clone, PartialEq)]
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
    ///64 bit int
    BigInt,
    ///32 bit int 
    Int,
    ///boolean
    Boolean,
    ///Func
    Func{func_type: Box<FuncType>, type_args: Vec<Type>},
    ///Tuple
    Tuple(Vec<Type>),
    /// untyped object literal - can be cast to a matching struct or class
    ObjectLiteral(HashMap<String, Type>),
    /// boxed type
    Any,
    /// Options. This is built into the type system because I want a zero-cost abstraction of a null pointer.
    Option(Box<Type>),
    /// Option - some
    Some(Box<Type>),
    /// user type
    UserClass{name: String, type_args: Vec<Type>},
    /// user struct type
    UnsafeUserStruct{name: String},
    /// not yet known - will be filled in by the typer
    Undeclared,
    ///Unresolved type var
    VariableUsage(String),
    ///numeric literal - 'number'
    FloatLiteral(f64),
    //numeric literal - big int
    BigIntLiteral(i64),
    //numeric literal - int
    IntLiteral(i32),
    /// string literal
    StringLiteral(String),
    ///ptr - internal type. Param is alignment
    UnsafePtr,
    ///
    UnsafeSizeT,
    ///type literal
    TypeLiteral(Box<Type>),
    /// __static_array type
    UnsafeStaticArray(Box<Type>, i32),
}

impl Type{
    pub fn is_undeclared(&self) -> bool {
        match &self {
            Type::Undeclared => true,
            _ => false
        }
    }

    pub fn get_func_type(&self) -> Option<&FuncType> {
        match &self {
            Type::Func{func_type, type_args: _} => Some(func_type),
            _ => None
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::RealVoid => write!(f, "void"),
            Type::FakeVoid => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
            Type::Never => write!(f, "never"),
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Array(inner) => write!(f, "Array<{}>", inner),
            Type::BigInt => write!(f, "bigint"),
            Type::Int => write!(f, "int"),
            Type::Boolean => write!(f, "boolean"),
            Type::Func{func_type, type_args: _} => write!(f, "{}", func_type),
            Type::Tuple(types) => {
                let mut vec: Vec<String> = vec![];
                for inner in types {
                    vec.push(format!("{}", inner));
                }
                write!(f, "[{}]", vec.join(",")) 
            },
            //Type::Object => write!(f, "object"),
            Type::Any => write!(f, "any"),
            Type::Option(inner) => write!(f, "Option<{}>", inner),
            Type::Some(inner) => write!(f, "Some<{}>", inner),
            Type::UserClass{name, type_args: _} => write!(f, "{}", name),
            Type::UnsafeUserStruct{name} => write!(f, "{}", name),
            Type::Undeclared => write!(f, "undeclared"),
            Type::VariableUsage(name) => write!(f, "{}", name),
            Type::UnsafePtr => write!(f, "__ptr"),
            Type::UnsafeSizeT => write!(f, "__size_t"),
            Type::FloatLiteral(n) => write!(f, "{}", n),
            Type::IntLiteral(n) => write!(f, "{}", n),
            Type::BigIntLiteral(n) => write!(f, "{}", n),
            Type::StringLiteral(n) => write!(f, "\"{}\"", n),
            Type::ObjectLiteral(_) => write!(f, "{{}}"),
            Type::TypeLiteral(t) => write!(f, "type: {}", t),
            Type::UnsafeStaticArray(t, s) => write!(f, "__static_array<{}, {}>", t, s),
        }
    }
}

/// Given an operator type, and the type of the operand, find the type of
/// the resulting intermediate value.
pub fn get_unary_op_type(op_type: &OpType, operand_type: &Type) -> Option<Type> {
    match op_type {
        // given unary operators can only be simple, maybe I should change this
        OpType::SimpleOpType(func_types) => {
            // find the instance that matches this
            let o_this_func_type = func_types.iter().find(|&func_type| func_type.in_types.len() == 1 && func_type.in_types[0] == *operand_type);
            match o_this_func_type {
                Some(this_func_type) => Some(this_func_type.out_type.clone()),
                None => None
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
                    let out_lhs_type = match &out_lhs_type_cast{
                        TypeCast::NotNeeded => func_type.in_types.get(0).unwrap().clone(),
                        TypeCast::FreeWiden => lhs_type.clone(),
                        TypeCast::IntToBigIntWiden => Type::BigInt,
                        TypeCast::IntToNumberWiden => Type::Number,
                        TypeCast::None => {
                            //if we get a return from get_type_casts_for_function_set it won't have a None
                            return None;
                        }
                    };
                    let out_rhs_type_cast = arg_type_casts.get(1).unwrap().clone();
                    let out_rhs_type = match &out_rhs_type_cast{
                        TypeCast::NotNeeded => func_type.in_types.get(1).unwrap().clone(),
                        TypeCast::FreeWiden => lhs_type.clone(),
                        TypeCast::IntToBigIntWiden => Type::BigInt,
                        TypeCast::IntToNumberWiden => Type::Number,
                        TypeCast::None => {
                            //if we get a return from get_type_casts_for_function_set it won't have a None
                            return None;
                        }
                    };

                    Some(BinOpTypeCast{lhs_type: out_lhs_type, lhs_type_cast: out_lhs_type_cast.clone(), rhs_type: out_rhs_type, rhs_type_cast: out_rhs_type_cast.clone(), out_type: func_type.out_type.clone()})
                }
            }
        },

        //assignment.
        OpType::AssignmentOpType => {
            //this is simple; either rhs can be cast to lhs or it can't
            let type_cast = try_cast(rhs_type, lhs_type, true);
            let out_rhs_type = match &type_cast{
                TypeCast::NotNeeded => lhs_type.clone(),
                TypeCast::FreeWiden => lhs_type.clone(),
                TypeCast::IntToBigIntWiden => Type::BigInt,
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
                let type_cast = try_cast(rhs_type, lhs_type, true);
                let out_rhs_type = match &type_cast{
                    TypeCast::NotNeeded => lhs_type.clone(),
                    TypeCast::FreeWiden => lhs_type.clone(),
                    TypeCast::IntToBigIntWiden => Type::BigInt,
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
            let type_cast = try_cast(lhs_type, &rhs_type, true);
            match &type_cast{
                TypeCast::NotNeeded => 
                    Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: Type::Boolean}),
                TypeCast::FreeWiden | TypeCast::IntToBigIntWiden | TypeCast::IntToNumberWiden => 
                    Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: type_cast, rhs_type: lhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: Type::Boolean}),
                TypeCast::None => {
                    //now try going the other way
                    let type_cast = try_cast(rhs_type, &lhs_type, true);
                    match &type_cast{
                        TypeCast::NotNeeded => 
                            Some(BinOpTypeCast{lhs_type: lhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: TypeCast::NotNeeded, out_type: Type::Boolean}),
                        TypeCast::FreeWiden | TypeCast::IntToBigIntWiden | TypeCast::IntToNumberWiden =>
                            Some(BinOpTypeCast{lhs_type: rhs_type.clone(), lhs_type_cast: TypeCast::NotNeeded, rhs_type: rhs_type.clone(), rhs_type_cast: type_cast, out_type: Type::Boolean}),
                        TypeCast::None => None
                    }    
                }
            }
        },

        //this is just a simple cast, so no need for all the fancy bin op stuff
        OpType::AsOpType => None,

        OpType::NotImplementedOpType => None,
    }
}
