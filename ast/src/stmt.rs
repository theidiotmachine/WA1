use crate::expr::Expr;
use crate::func::FuncDecl;
use crate::types::Type;

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
    pub init: Option<Expr>,
    pub closure_source: bool,
    pub arg: bool,
}

/// Variable decl
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariableDecl {
    pub name: String,
    pub r#type: Type,
    pub constant: bool,
    pub init: Option<Expr>,
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
    Expr(Expr),
    /// a function declaration. This might compile to a closure creation, a function pointer, or a no op 
    /// this is wrong - in JS this is an expression
    FuncDecl(FuncDecl),
    /// variable declaration
    Variable(VariableDecl),
    /// return statement
    Return(Option<Expr>),
}