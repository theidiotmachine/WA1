use crate::expr::TypedExpr;
use crate::func::FuncDecl;
use types::Type;

pub mod prelude {
    pub use super::Stmt;
    pub use super::VariableDecl;
    pub use super::GlobalVariableDecl;
    pub use super::ClosureRef;
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
    pub init: Option<TypedExpr>,
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureRef {
    pub internal_name: String,
    pub r#type: Type,
    pub constant: bool,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Any expression
    Expr(TypedExpr),
    /// a function declaration. This might compile to a closure creation, a function pointer, or a no op 
    /// this is wrong - in JS this is an expression
    FuncDecl(FuncDecl),
    /// variable declaration
    VariableDecl(VariableDecl),
    /// global variable declaration. Only appears in the __start function.
    GlobalVariableDecl(GlobalVariableDecl),
    /// return statement
    Return(Option<TypedExpr>),
    /// if-then
    IfThen(TypedExpr, Vec<Stmt>),
    /// if-then-else
    IfThenElse(TypedExpr, Vec<Stmt>, Vec<Stmt>),
    /// while loop
    While(TypedExpr, Vec<Stmt>),
    // class decl
    ClassDecl(String),
    /// break
    Break,
    /// continue
    Continue,
    
}