use serde::{Serialize, Deserialize};

use types::Type;
use crate::expr::VariableDecl;
use crate::expr::ClosureRef;
use std::collections::HashMap;
use crate::expr::TypedExpr;
use types::FuncType;

pub mod prelude {
    pub use super::Func;
    pub use super::FuncArg;
    pub use super::FuncObjectCreation;
    pub use super::FuncDecl;
}

/// Simple func arg, one with a name and a type, e.g. x: number
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncArg {
    pub name: String,
    pub r#type: Type,
}

/// Function definition
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Func {
    pub decl: FuncDecl,
    /// if import is true, this will be a nop?
    pub body: TypedExpr,
    pub local_vars: Vec<VariableDecl>,
    pub closure: Vec<ClosureRef>,
    pub local_var_map: HashMap<String, u32>,
}

/// Function definition as seen from another module
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncDecl {
    pub name: String,
    pub return_type: Type,
    pub args: Vec<FuncArg>,
    pub export: bool,
    pub import: bool,
}


impl Func {
    pub fn get_arg_types(&self) -> Vec<Type> {
        let mut out: Vec<Type> = vec![];
        for arg in &self.decl.args {
            out.push(arg.r#type.clone());
        }
        out
    }

    pub fn get_func_type(&self) -> FuncType {
        FuncType{out_type: self.decl.return_type.clone(), in_types: self.get_arg_types()}
    }
}

/// Function declaration. This means a declaration site. In reality that means a closure creation plus 
/// reference to table pointer if it's consumed.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FuncObjectCreation {
    pub name: String,
    pub closure: Vec<ClosureRef>
}


