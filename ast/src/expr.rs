use types::*;

pub mod prelude {
    pub use super::Expr;
    pub use super::TypedExpr;
    pub use super::BinaryOperator;
    pub use super::BinaryOperatorApplication;
    pub use super::UnaryOperator;
    pub use super::UnaryOperatorApplication;
}

use lazy_static;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator{
    Dot,
    Comma,
    GreaterThan,
    LessThan,
    Plus,
    Minus,
    Multiply,
    Mod,
    BitOr,
    BitAnd,
    BitXor,
    Divide,
    StrictEqual,
    StrictNotEqual,
    UnsignedRightShift,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LeftShift,
    RightShift,
    GreaterThanEqual,
    LessThanEqual,
    Exponent,
    In,
    InstanceOf,
    //the assignment operators. These are considered ops in js, even though they sideeffect
    Assign,
    PlusAssign,
    MinusAssign,
    ExponentAssign,
    MultiplyAssign,
    DivideAssign,
    ModAssign,
    LeftShiftAssign,
    RightShiftAssign,
    UnsignedRightShiftAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign
}

lazy_static!{
static ref COMPARISON_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Number, Type::Number], out_type: Type::Boolean},
    FuncType{in_types: vec![Type::BigInt, Type::BigInt], out_type: Type::Boolean}
]);

static ref MATHS_BIN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Number, Type::Number], out_type: Type::Number},
    FuncType{in_types: vec![Type::BigInt, Type::BigInt], out_type: Type::BigInt}
]);

static ref MATHS_UN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Number], out_type: Type::Number},
    FuncType{in_types: vec![Type::BigInt], out_type: Type::BigInt}
]);

static ref BIT_BIN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Number, Type::Number], out_type: Type::Number},
    FuncType{in_types: vec![Type::BigInt, Type::BigInt], out_type: Type::BigInt}
]);

static ref EQUALITY_OP: OpType = OpType::EqualityOpType;

static ref BOOL_BIN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Boolean, Type::Boolean], out_type: Type::Boolean},
]);

static ref BOOL_UN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Boolean], out_type: Type::Boolean},
]);

static ref ASSIGN_MODIFY_OP: OpType = OpType::AssignModifyOpType;

static ref ASSIGN_OP: OpType = OpType::AssignmentOpType;

/// clearly it would be nice to get these types
static ref NO_IDEA_OP: OpType = OpType::NotImplementedOpType;
}

impl BinaryOperator{
    pub fn is_assign_operator(&self) -> bool {
        match self {
            BinaryOperator::Assign => true,
            BinaryOperator::PlusAssign => true,
            BinaryOperator::MinusAssign => true,
            BinaryOperator::ExponentAssign => true,
            BinaryOperator::MultiplyAssign => true,
            BinaryOperator::DivideAssign => true,
            BinaryOperator::ModAssign => true,
            BinaryOperator::LeftShiftAssign => true,
            BinaryOperator::RightShiftAssign => true,
            BinaryOperator::UnsignedRightShiftAssign => true,
            BinaryOperator::BitAndAssign => true,
            BinaryOperator::BitXorAssign => true,
            BinaryOperator::BitOrAssign => true,
            _ => false
        }
    }

    pub fn is_simple_operator(&self) -> bool {
        match self {
            BinaryOperator::GreaterThan => true,
            BinaryOperator::LessThan => true,
            BinaryOperator::Plus => true,
            BinaryOperator::Minus => true,
            BinaryOperator::Multiply => true,
            BinaryOperator::Mod => true,
            BinaryOperator::BitOr => true,
            BinaryOperator::BitAnd => true,
            BinaryOperator::BitXor => true,
            BinaryOperator::Divide => true,
            BinaryOperator::StrictEqual => true,
            BinaryOperator::StrictNotEqual => true,
            BinaryOperator::UnsignedRightShift => true,
            BinaryOperator::LogicalAnd => true,
            BinaryOperator::LogicalOr => true,
            BinaryOperator::Equal => true,
            BinaryOperator::NotEqual => true,
            BinaryOperator::LeftShift => true,
            BinaryOperator::RightShift => true,
            BinaryOperator::GreaterThanEqual => true,
            BinaryOperator::LessThanEqual => true,
            BinaryOperator::Exponent => true,
            BinaryOperator::In => true,
            _ => false
        }
    }

    pub fn get_op_type(&self) -> &OpType {
        match self {
            BinaryOperator::Dot => &NO_IDEA_OP,
            BinaryOperator::Comma => &NO_IDEA_OP,
            BinaryOperator::GreaterThan => &COMPARISON_OP,
            BinaryOperator::LessThan => &COMPARISON_OP,
            BinaryOperator::Plus => &MATHS_BIN_OP,
            BinaryOperator::Minus => &MATHS_BIN_OP,
            BinaryOperator::Multiply => &MATHS_BIN_OP,
            BinaryOperator::Mod => &MATHS_BIN_OP,
            BinaryOperator::BitOr => &BIT_BIN_OP,
            BinaryOperator::BitAnd => &BIT_BIN_OP,
            BinaryOperator::BitXor => &BIT_BIN_OP,
            BinaryOperator::Divide => &MATHS_BIN_OP,
            BinaryOperator::StrictEqual => &EQUALITY_OP,
            BinaryOperator::StrictNotEqual => &EQUALITY_OP,
            BinaryOperator::UnsignedRightShift => &MATHS_BIN_OP,
            BinaryOperator::LogicalAnd => &BOOL_BIN_OP,
            BinaryOperator::LogicalOr => &BOOL_BIN_OP,
            BinaryOperator::Equal => &EQUALITY_OP,
            BinaryOperator::NotEqual => &EQUALITY_OP,
            BinaryOperator::LeftShift => &MATHS_BIN_OP,
            BinaryOperator::RightShift => &MATHS_BIN_OP,
            BinaryOperator::GreaterThanEqual => &COMPARISON_OP,
            BinaryOperator::LessThanEqual => &COMPARISON_OP,
            BinaryOperator::Exponent => &MATHS_BIN_OP,
            BinaryOperator::In => &NO_IDEA_OP,
            BinaryOperator::InstanceOf => &NO_IDEA_OP,
            BinaryOperator::Assign => &ASSIGN_OP,
            BinaryOperator::PlusAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::MinusAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::ExponentAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::MultiplyAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::DivideAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::ModAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::LeftShiftAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::RightShiftAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::UnsignedRightShiftAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::BitAndAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::BitXorAssign => &ASSIGN_MODIFY_OP,
            BinaryOperator::BitOrAssign => &ASSIGN_MODIFY_OP
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperatorApplication{
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>, 
    pub op: BinaryOperator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator{
    LogicalNot,
    BitNot,
    PostfixIncrement,
    PostfixDecrement,
    Minus,
    Plus,
    PrefixIncrement,
    PrefixDecrement,
}

impl UnaryOperator{
    pub fn get_op_type(&self) -> &OpType {
        match self {
            UnaryOperator::LogicalNot => &BOOL_UN_OP,
            UnaryOperator::BitNot => &MATHS_UN_OP,
            UnaryOperator::PostfixIncrement => &MATHS_UN_OP,
            UnaryOperator::PostfixDecrement=> &MATHS_UN_OP,
            UnaryOperator::Minus => &MATHS_UN_OP,
            UnaryOperator::Plus => &MATHS_UN_OP,
            UnaryOperator::PrefixIncrement => &MATHS_UN_OP,
            UnaryOperator::PrefixDecrement => &MATHS_UN_OP,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperatorApplication {
    pub expr: Box<TypedExpr>,
    pub op: UnaryOperator,
}

/// Something that returns something
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A binary operator
    BinaryOperator(BinaryOperatorApplication),
    /// unary operator
    UnaryOperator(UnaryOperatorApplication),
    /// float literal
    FloatLiteral(f64),
    /// int literal
    //IntLiteral(i64),
    ///true or false
    BoolLiteral(bool),
    //null
    Null,
    //void. Unlike TS we use void as the unit type, so you can instantiate it.
    Void,
    //string literal
    StringLiteral(String),
    //variable call
    GlobalVariableUse(String),
    LocalVariableUse(String),
    ClosureVariableUse(String),
    ParameterUse(String),
    //round brackets
    Parens(Box<TypedExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr{
    pub expr: Expr,
    pub r#type: Type,
}