use types::Type;
use crate::stmt::Stmt;
use crate::stmt::VariableDecl;
use crate::stmt::ClosureRef;
use std::collections::HashMap;

pub mod prelude {
    pub use super::Func;
    pub use super::FuncArg;
    pub use super::FuncDecl;
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: String,
    pub return_type: Type,
    pub args: Vec<FuncArg>,
    pub export: bool,
    pub import: bool,
    /// if import is true, this will be an empty vec
    pub body: Vec<Stmt>,
    pub local_vars: Vec<VariableDecl>,
    pub closure: Vec<ClosureRef>,
    pub local_var_map: HashMap<String, u32>,
}

/// Function declaration. This in reality means a closure creation plus reference to function name
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub name: String,
    pub closure: Vec<ClosureRef>
}

/// Simple func arg, one with a name and a type, e.g. x: number
#[derive(Debug, Clone, PartialEq)]
pub struct FuncArg {
    pub name: String,
    pub r#type: Type,
}
