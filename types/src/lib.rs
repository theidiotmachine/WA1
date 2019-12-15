pub mod prelude {
    pub use super::Type;
    pub use super::OpType;
    pub use super::FuncType;
    pub use super::ClassType;
    pub use super::ClassMember;
    pub use super::Privacy;
    pub use super::get_unary_op_type;
    pub use super::get_binary_op_type;
    pub use super::try_up_cast_binary_op_lhs;
    pub use super::try_up_cast_binary_op_rhs;
    pub use super::AbsTypeDecl;
    pub use super::PtrAlign;
    pub use super::BinaryOpUpCast;
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
    /// The type of an assign and modify operator. This is, say +=. In my type system, 
    /// rv is the same as lhs, rhs can vary depending on lhs.
    AssignModifyOpType,
    /// The type of an equality or inequality operator. In my very simple type system, lhs must be the 
    /// same type as rhs; return value is boolean.
    EqualityOpType,
    /// Not implemented yet!
    NotImplementedOpType,
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

/// This is part of the type system rewrite. I am not sure it's correct yet.
/// This is the body of a type function that consumes type args and returns a type.
#[derive(Debug, Clone, PartialEq)]
pub enum AbsTypeBody{
    VariableUse(String),
    Array(Box<AbsTypeBody>),
    Func(Vec<AbsTypeBody>, Box<AbsTypeBody>),
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
pub struct AbsTypeDecl{
    pub args: Vec<String>,
    pub out: AbsTypeBody,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PtrAlign{
    Align8, Align16, Align32, Align64
}

impl PtrAlign{
    pub fn from_i32(i: i32) -> Option<PtrAlign> {
        match i {
            8 => Some(PtrAlign::Align8),
            16 => Some(PtrAlign::Align16),
            32 => Some(PtrAlign::Align32),
            64 => Some(PtrAlign::Align64),
            _ => None,
        }
    }
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
    
    ///object
    Object,

    /// boxed type
    Any,

    /// Options
    Option(Box<Type>),
    /// Option - some
    Some(Box<Type>),
    /// Option - none
    Null,

    /// user type
    UserClass{name: String, type_args: Vec<Type>},

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


    /// string lireral
    StringLiteral(String),

    ///ptr - internal type. Param is alignment
    Ptr(PtrAlign),
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
            Type::Object => write!(f, "object"),
            Type::Any => write!(f, "any"),
            Type::Option(inner) => write!(f, "Option<{}>", inner),
            Type::Some(inner) => write!(f, "Some<{}>", inner),
            Type::Null => write!(f, "null"),
            Type::UserClass{name, type_args: _} => write!(f, "{}", name),
            Type::Undeclared => write!(f, "undeclared"),
            Type::VariableUsage(name) => write!(f, "{}", name),
            Type::Ptr(p) => write!(f, "__ptr<{:#?}>", p),
            Type::FloatLiteral(n) => write!(f, "{}", n),
            Type::IntLiteral(n) => write!(f, "{}", n),
            Type::BigIntLiteral(n) => write!(f, "{}", n),
            Type::StringLiteral(n) => write!(f, "\"{}\"", n),
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


/// Given a binary operator type, and the types of the operands, find the type
/// of the resulting intermediate value.
pub fn get_binary_op_type(op_type: &OpType, lhs_type: &Type, rhs_type: &Type) -> Option<Type> {
    match op_type {
        OpType::SimpleOpType(func_types) => {
            // simply look for a matching operator type.
            let o_this_func_type = func_types.iter().find(
                |&func_type| func_type.in_types.len() == 2 && func_type.in_types[0] == *lhs_type && func_type.in_types[1] == *rhs_type 
            );
            match o_this_func_type {
                Some(this_func_type) => Some(this_func_type.out_type.clone()),
                None => None
            }
        },

        OpType::AssignmentOpType => {
            if lhs_type == rhs_type {
                Some(lhs_type.clone())
            } else {
                None
            }
        },

        // ok, today I will just assume that all sides are the same. This is of course wrong.
        OpType::AssignModifyOpType => {
            if lhs_type == rhs_type {
                Some(lhs_type.clone())
            } else {
                None
            }
        },

        OpType::EqualityOpType => {
            if lhs_type == rhs_type {
                Some(Type::Boolean)
            } else {
                None
            }
        },

        OpType::NotImplementedOpType => None,
    }
}

pub enum BinaryOpUpCast{
    UpCast{new_type: Type, out_type: Type},
    None
}

pub fn try_up_cast_binary_op_lhs(op_type: &OpType, lhs_type: &Type, rhs_type: &Type) -> BinaryOpUpCast {
    if *lhs_type == Type::Int {
        if *rhs_type == Type::Number {
            let hmm = get_binary_op_type(op_type, &Type::Number, rhs_type);
            match hmm {
                Some(t) => {
                    BinaryOpUpCast::UpCast{new_type: Type::Number, out_type: Type::Number}
                },
                _ => BinaryOpUpCast::None
            }   
        } else if *rhs_type == Type::BigInt {
            let hmm = get_binary_op_type(op_type, &Type::Number, rhs_type);
            match hmm {
                Some(t) => {
                    BinaryOpUpCast::UpCast{new_type: Type::BigInt, out_type: Type::BigInt}
                },
                _ => BinaryOpUpCast::None
            }
        } else {
            BinaryOpUpCast::None
        }
    } else {
        BinaryOpUpCast::None
    }
}

pub fn try_up_cast_binary_op_rhs(op_type: &OpType, lhs_type: &Type, rhs_type: &Type) -> BinaryOpUpCast {
    if *rhs_type == Type::Int {
        if *lhs_type == Type::Number {
            let hmm = get_binary_op_type(op_type, lhs_type, &Type::Number);
            match hmm {
                Some(t) => {
                    BinaryOpUpCast::UpCast{new_type: Type::Number, out_type: Type::Number}
                },
                _ => BinaryOpUpCast::None
            }   
        } else if *lhs_type == Type::BigInt {
            let hmm = get_binary_op_type(op_type, lhs_type, &Type::BigInt);
            match hmm {
                Some(t) => {
                    BinaryOpUpCast::UpCast{new_type: Type::BigInt, out_type: Type::BigInt}
                },
                _ => BinaryOpUpCast::None
            }
        } else {
            BinaryOpUpCast::None
        }
    } else {
        BinaryOpUpCast::None
    }
}