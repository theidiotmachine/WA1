use serde::{Serialize, Deserialize};

use types::*;
use crate::func::{FuncObjectCreation, FuncDecl};
use crate::intrinsic::Intrinsic;
use errs::prelude::*;

pub mod prelude {
    pub use super::Expr;
    pub use super::TypedExpr;
    pub use super::BinaryOperator;
    pub use super::UnaryOperator;
    pub use super::AssignmentOperator;
    pub use super::TypedLValueExpr;
    pub use super::LValueExpr;
    pub use super::GlobalVariableDecl;
    pub use super::GlobalVariableImport;
    pub use super::ClosureRef;
    pub use super::ObjectLiteralElem;
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    Exponent,
    In,
    InstanceOf,
    As,
}


#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
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


#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum AssignmentOperator{
    Assign,
    PlusAssign,
    MinusAssign,
    ExponentAssign,
    MultiplyAssign,
    DivideAssign,
    ModAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign
}

/// Variable decl
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GlobalVariableDecl {
    pub name: String,
    pub r#type: Type,
    pub constant: bool,
    pub init: Option<TypedExpr>,
    pub export: bool,
}

/// Variable decl
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GlobalVariableImport {
    pub name: String,
    pub r#type: Type,
    pub constant: bool,
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClosureRef {
    pub internal_name: String,
    pub r#type: Type,
    pub constant: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectLiteralElem{
    pub name: String,
    pub value: TypedExpr,
}

/// Something that returns something. This is really an RValue.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    /// A binary operator
    BinaryOperator{lhs: Box<TypedExpr>, rhs: Box<TypedExpr>, op: BinaryOperator},
    /// unary operator
    UnaryOperator{expr: Box<TypedExpr>, op: UnaryOperator},
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
    ///__null
    UnsafeNull,
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
    /// assignment expression
    Assignment(Box<TypedExpr>, TypedLValueExpr, Box<TypedExpr>),
    /// modify and assign expression
    ModifyAssignment(AssignmentOperator, Box<TypedExpr>, TypedLValueExpr, Box<TypedExpr>),
    /// static func call. Arg is func name.
    StaticFuncCall(String, FuncDecl, Vec<TypedExpr>),
    /// dynamic func call.
    DynamicFuncCall(Box<TypedExpr>, Vec<TypedExpr>),
    /// Member func call.
    MemberFuncCall(Box<TypedExpr>, String, Vec<TypedExpr>),
    /// A generic call that we haven't been able to resolve yet
    UnresolvedGenericFuncCall{name: String, unresolved_func_decl: FuncDecl, args: Vec<TypedExpr>, unresolved_types: Vec<Type>},
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
    FuncDecl(FuncObjectCreation),
    /// variable declaration
    VariableInit{internal_name: String, init: Box<Option<TypedExpr>>},
    /// global variable declaration. Only appears in the _start function.
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
    /// Member of a thing, such as a[b]
    DynamicMember(Box<TypedExpr>, Box<TypedExpr>),
    /// object literal
    ObjectLiteral(Vec<ObjectLiteralElem>),
    /// constructor - dynamic
    ConstructFromObjectLiteral(Type, Vec<ObjectLiteralElem>),
    /// constructor - static
    ConstructStaticFromObjectLiteral(Type, Vec<ObjectLiteralElem>),
    /// constructor - static
    ConstructStaticFromArrayLiteral(Type, Vec<TypedExpr>),
    ///type literal
    TypeLiteral(Type),
    ///__sizeof
    SizeOf(Type),
    ///No op expression
    NoOp,
    ///Tuple literal constructor
    TupleLiteral(Vec<TypedExpr>),
}

impl Expr{
    pub fn is_literal(&self) -> bool {
        match self {
            Expr::FloatLiteral(_) | Expr::BigIntLiteral(_) | Expr::IntLiteral(_) | Expr::BoolLiteral(_) | Expr::Null 
                | Expr::UnsafeNull | Expr::Void | Expr::StringLiteral(_) | Expr::ObjectLiteral(_) | Expr::TypeLiteral(_) 
                | Expr::TupleLiteral(_) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
                    Some(TypedLValueExpr{r#type: self.r#type.clone(), expr: LValueExpr::StaticNamedMemberAssign(lhs.clone(), lhs.r#type.clone(), name.clone()), loc: self.loc})
                }
            },
            Expr::DynamicMember(outer, inner) => {
                if self.is_const {
                    None
                } else {
                    Some(TypedLValueExpr{r#type: self.r#type.clone(), expr: LValueExpr::DynamicMemberAssign(outer.clone(), outer.r#type.clone(), inner.clone()), loc: self.loc})
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LValueExpr{
    //Assign a global variable
    GlobalVariableAssign(String),
    //Assign a local variable
    LocalVariableAssign(String),
    //Assign a closure variable
    ClosureVariableAssign(String),
    //Assign a static named member of some data, so e.g. a.b = c
    StaticNamedMemberAssign(Box<TypedExpr>, Type, String),
    //Assign a dynamic member of some data e.g. a[b] = c
    DynamicMemberAssign(Box<TypedExpr>, Type, Box<TypedExpr>),
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypedLValueExpr{
    pub expr: LValueExpr,
    pub r#type: Type,
    pub loc: SourceLocation,
}