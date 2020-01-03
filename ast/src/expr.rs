use types::*;
use crate::func::FuncDecl;
use crate::intrinsic::Intrinsic;
use ress::SourceLocation;

pub mod prelude {
    pub use super::Expr;
    pub use super::TypedExpr;
    pub use super::BinaryOperator;
    pub use super::BinaryOperatorApplication;
    pub use super::UnaryOperator;
    pub use super::UnaryOperatorApplication;
    pub use super::AssignmentOperator;
    pub use super::TypedLValueExpr;
    pub use super::LValueExpr;
    pub use super::GlobalVariableDecl;
    pub use super::ClosureRef;
    pub use super::VariableDecl;
    pub use super::ObjectLiteralElem;
}

use lazy_static;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator{
    Dot,
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
    As,
}

lazy_static!{
static ref COMPARISON_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Int, Type::Int], out_type: Type::Boolean},
    FuncType{in_types: vec![Type::Number, Type::Number], out_type: Type::Boolean},
    FuncType{in_types: vec![Type::BigInt, Type::BigInt], out_type: Type::Boolean},
    FuncType{in_types: vec![Type::Ptr, Type::Ptr], out_type: Type::Boolean},
    FuncType{in_types: vec![Type::SizeT, Type::SizeT], out_type: Type::Boolean},
]);

static ref MATHS_BIN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Int, Type::Int], out_type: Type::Int},
    FuncType{in_types: vec![Type::Number, Type::Number], out_type: Type::Number},
    FuncType{in_types: vec![Type::BigInt, Type::BigInt], out_type: Type::BigInt},
    FuncType{in_types: vec![Type::Ptr, Type::Ptr], out_type: Type::Ptr},
    FuncType{in_types: vec![Type::SizeT, Type::SizeT], out_type: Type::SizeT},
]);

static ref MATHS_UN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Int], out_type: Type::Int},
    FuncType{in_types: vec![Type::Number], out_type: Type::Number},
    FuncType{in_types: vec![Type::BigInt], out_type: Type::BigInt},
    FuncType{in_types: vec![Type::Ptr], out_type: Type::Ptr},
    FuncType{in_types: vec![Type::SizeT], out_type: Type::SizeT},
]);

static ref BIT_BIN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Int, Type::Int], out_type: Type::Int},
    FuncType{in_types: vec![Type::BigInt, Type::BigInt], out_type: Type::BigInt},
    FuncType{in_types: vec![Type::Ptr, Type::Ptr], out_type: Type::Ptr},
    FuncType{in_types: vec![Type::SizeT, Type::SizeT], out_type: Type::SizeT},
]);

static ref EQUALITY_OP: OpType = OpType::EqualityOpType;

static ref AS_OP: OpType = OpType::AsOpType;

static ref BOOL_BIN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Boolean, Type::Boolean], out_type: Type::Boolean},
]);

static ref BOOL_UN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Boolean], out_type: Type::Boolean},
]);

static ref BIT_UN_OP: OpType = OpType::SimpleOpType(vec![
    FuncType{in_types: vec![Type::Int], out_type: Type::Int},
    FuncType{in_types: vec![Type::SizeT], out_type: Type::SizeT},
    FuncType{in_types: vec![Type::BigInt], out_type: Type::BigInt},
]);

static ref MATHS_ASSIGN_MODIFY_OP: OpType = OpType::AssignModifyOpType(vec![
    Type::Int,
    Type::Number,
    Type::BigInt,
    Type::Ptr,
    Type::SizeT,
]);

static ref BIT_ASSIGN_MODIFY_OP: OpType = OpType::AssignModifyOpType(vec![
    Type::Int,
    Type::BigInt,
    Type::SizeT,
]);

static ref ASSIGN_OP: OpType = OpType::AssignmentOpType;

/// clearly it would be nice to get these types
static ref NO_IDEA_OP: OpType = OpType::NotImplementedOpType;
}

impl BinaryOperator{
    pub fn get_op_type(&self) -> &OpType {
        match self {
            BinaryOperator::Dot => &NO_IDEA_OP,
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
            BinaryOperator::As => &AS_OP,
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
            UnaryOperator::BitNot => &BIT_UN_OP,
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
pub enum AssignmentOperator{
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

impl AssignmentOperator{
    pub fn get_op_type(&self) -> &OpType {
        match self {
            AssignmentOperator::Assign => &ASSIGN_OP,
            AssignmentOperator::PlusAssign => &MATHS_ASSIGN_MODIFY_OP,
            AssignmentOperator::MinusAssign => &MATHS_ASSIGN_MODIFY_OP,
            AssignmentOperator::ExponentAssign => &MATHS_ASSIGN_MODIFY_OP,
            AssignmentOperator::MultiplyAssign => &MATHS_ASSIGN_MODIFY_OP,
            AssignmentOperator::DivideAssign => &MATHS_ASSIGN_MODIFY_OP,
            AssignmentOperator::ModAssign => &MATHS_ASSIGN_MODIFY_OP,
            AssignmentOperator::LeftShiftAssign => &BIT_ASSIGN_MODIFY_OP,
            AssignmentOperator::RightShiftAssign => &BIT_ASSIGN_MODIFY_OP,
            AssignmentOperator::UnsignedRightShiftAssign => &BIT_ASSIGN_MODIFY_OP,
            AssignmentOperator::BitAndAssign => &BIT_ASSIGN_MODIFY_OP,
            AssignmentOperator::BitXorAssign => &BIT_ASSIGN_MODIFY_OP,
            AssignmentOperator::BitOrAssign => &BIT_ASSIGN_MODIFY_OP
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperatorApplication {
    pub expr: Box<TypedExpr>,
    pub op: UnaryOperator,
}

/// Variable decl
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecl {
    pub internal_name: String,
    pub orig_name: String,
    pub r#type: Type,
    pub constant: bool,
    pub init: Option<TypedExpr>,
    pub closure_source: bool,
    pub arg: bool,
}

/// Variable decl
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariableDecl {
    pub name: String,
    pub r#type: Type,
    pub constant: bool,
    pub init: TypedExpr,
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureRef {
    pub internal_name: String,
    pub r#type: Type,
    pub constant: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectLiteralElem{
    pub name: String,
    pub value: TypedExpr,
}

/// Something that returns something. This is really an RValue.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A binary operator
    BinaryOperator(BinaryOperatorApplication),
    /// unary operator
    UnaryOperator(UnaryOperatorApplication),
    /// float literal
    FloatLiteral(f64),
    /// big int literal
    BigIntLiteral(i64),
    /// int literal
    IntLiteral(i32),
    ///true or false
    BoolLiteral(bool),
    ///null
    Null,
    ///void. Unlike TS we use void as the unit type, so you can instantiate it.
    Void,
    ///string literal
    StringLiteral(String),
    ///global variable use
    GlobalVariableUse(String),
    ///local variable use
    LocalVariableUse(String),
    ///closure variable use
    ClosureVariableUse(String),
    ///round brackets
    Parens(Box<TypedExpr>),
    /// assignmnet expression
    Assignment(TypedLValueExpr, AssignmentOperator, Box<TypedExpr>),
    /// static func call. Arg is func name.
    StaticFuncCall(String, Vec<TypedExpr>),
    /// dynamic func call.
    DynamicFuncCall(Box<TypedExpr>, Vec<TypedExpr>),
    /// Implicit i32 -> f64 cast
    IntToNumber(Box<TypedExpr>),
    /// Implicit i32 -> i64 cast
    IntToBigInt(Box<TypedExpr>),
    /// A type widen that has no runtime cost. Used to make the types of the AST correct
    FreeTypeWiden(Box<TypedExpr>),
    /// a block of code. In code, either a set of statements surrounded by squigglies, or a single statement
    Block(Vec<TypedExpr>),
    /// if-then-else
    IfThenElse(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),
    /// a function declaration. This might compile to a closure creation, a function pointer, or a no op 
    /// this is wrong - in JS this is an expression
    FuncDecl(FuncDecl),
    /// variable declaration
    VariableDecl(Box<VariableDecl>),
    /// global variable declaration. Only appears in the __start function.
    GlobalVariableDecl(Box<GlobalVariableDecl>),
    /// return statement
    Return(Box<Option<TypedExpr>>),
    /// if-then
    IfThen(Box<TypedExpr>, Box<TypedExpr>),
    /// while loop
    While(Box<TypedExpr>, Box<TypedExpr>),
    /// class decl
    ClassDecl(String),
    /// __struct declaration
    StructDecl(String),
    /// break
    Break,
    /// continue
    Continue,
    /// A single wasm instruction. 
    Intrinsic(Intrinsic),
    /// Member of a thing, such as a.b
    NamedMember(Box<TypedExpr>, String),
    /// object literal
    ObjectLiteral(Vec<ObjectLiteralElem>),
    /// constructor
    ConstructFromObjectLiteral(Type, Vec<ObjectLiteralElem>),
    ///type literal
    TypeLiteral(Type),
    ///__sizeof
    SizeOf(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr{
    pub expr: Expr,
    pub r#type: Type,
    pub is_const: bool,
    pub loc: SourceLocation,
}

impl TypedExpr{
    pub fn as_l_value(&self) -> Option<TypedLValueExpr> {
        match &self.expr {
            Expr::GlobalVariableUse(name) => { 
                if self.is_const {
                    None
                } else {
                    Some(TypedLValueExpr{r#type: self.r#type.clone(), expr: LValueExpr::GlobalVariableAssign(name.clone()), loc: self.loc})
                }
            },
            Expr::LocalVariableUse(name) => {
                if self.is_const {
                    None
                } else {
                    Some(TypedLValueExpr{r#type: self.r#type.clone(), expr: LValueExpr::LocalVariableAssign(name.clone()), loc: self.loc})
                }
            },
            Expr::ClosureVariableUse(name) => {
                if self.is_const {
                    None
                } else {
                    Some(TypedLValueExpr{r#type: self.r#type.clone(), expr: LValueExpr::ClosureVariableAssign(name.clone()), loc: self.loc})
                }
            },
            Expr::NamedMember(lhs, name) => {
                if self.is_const {
                    None
                } else {
                    let o_lhs = lhs.as_l_value();
                    match o_lhs {
                        None => None,
                        Some(lhs_lhs) => {
                            Some(TypedLValueExpr{r#type: self.r#type.clone(), expr: LValueExpr::StaticNamedMemberAssign(Box::new(lhs_lhs), name.clone()), loc: self.loc})
                        }
                    }
                }
            },
            Expr::Parens(inner) => {
                inner.as_l_value()
            },
            _ => None,
        }
    }
}

/// An l value expression
#[derive(Debug, Clone, PartialEq)]
pub enum LValueExpr{
    //Assign a global variable
    GlobalVariableAssign(String),
    //Assign a local variable
    LocalVariableAssign(String),
    //Assign a closure variable
    ClosureVariableAssign(String),
    //Assign a static named member of some data, so e.g. a.b
    StaticNamedMemberAssign(Box<TypedLValueExpr>, String),
    //Assign a static numeric member of some data, so e.g. a[2]
    StaticU32MemberAssign(Box<TypedLValueExpr>, u32),
}


#[derive(Debug, Clone, PartialEq)]
pub struct TypedLValueExpr{
    pub expr: LValueExpr,
    pub r#type: Type,
    pub loc: SourceLocation,
}