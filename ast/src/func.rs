use types::Type;
use crate::expr::VariableDecl;
use crate::expr::ClosureRef;
use std::collections::HashMap;
use crate::expr::TypedExpr;
use types::FuncType;

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
    /// if import is true, this will be a nop?
    pub body: TypedExpr,
    pub local_vars: Vec<VariableDecl>,
    pub closure: Vec<ClosureRef>,
    pub local_var_map: HashMap<String, u32>,
}

impl Func {
    pub fn get_arg_types(&self) -> Vec<Type> {
        let mut out: Vec<Type> = vec![];
        for arg in &self.args {
            out.push(arg.r#type.clone());
        }
        out
    }

    pub fn get_func_type(&self) -> FuncType {
        FuncType{out_type: self.return_type.clone(), in_types: self.get_arg_types()}
    }
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
