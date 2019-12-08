pub mod prelude {
    pub use super::Type;
    pub use super::OpType;
    pub use super::FuncType;
    pub use super::get_unary_op_type;
    pub use super::get_binary_op_type;
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
pub enum Type {
    /// unit type that actually manifests as nothing
    RealVoid,
    /// unit type that is created at run time
    FakeVoid,

    /// top type
    Unknown,
    /// bottom type
    Never,

    ///number
    Number,
    ///string
    String,
    ///Array
    Array(Box<Type>),
    ///64 bit int
    BigInt,
    ///boolean
    Boolean,
    ///Func
    Func(Box<FuncType>),

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
    User(String),

    /// not yet known - will be filled in by the typer
    Undeclared,
    ///Unresolved type var
    Variable(String),

    ///numeric literal - 'number'
    FloatLiteral(f64),

    //numeric literal - big int
    //BigIntLiteral(i64),

    /// string lireral
    StringLiteral(String),

    ///ptr - internal type. Param is one of 8, 16, 32 or 64
    Ptr(u32),
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
            Type::Func(func_type) => Some(func_type),
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
            Type::Boolean => write!(f, "boolean"),
            Type::Func(func_type) => write!(f, "{}", func_type),
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
            Type::User(name) => write!(f, "{}", name),
            Type::Undeclared => write!(f, "undeclared"),
            Type::Variable(name) => write!(f, "{}", name),
            Type::Ptr(p) => write!(f, "__ptr<{}>", p),
            Type::FloatLiteral(n) => write!(f, "{}", n),
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