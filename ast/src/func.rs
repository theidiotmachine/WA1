use serde::{Serialize, Deserialize};
use std::collections::HashMap;

use types::{Type, FullType};
use crate::expr::ClosureRef;
use crate::expr::TypedExpr;
use crate::expr::VariableMutability;
use types::FuncType;

pub mod prelude {
    pub use super::Func;
    pub use super::FuncArg;
    pub use super::FuncObjectCreation;
    pub use super::FuncDecl;
    pub use super::TypeGuard;
    pub use super::TypeGuardBranch;
    pub use super::LocalVar;
}

/// Simple func arg, one with a name and a type, e.g. x: number
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncArg {
    pub mutability: VariableMutability,
    pub name: String,
    pub r#type: FullType,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct LocalVar{
    pub mutability: VariableMutability,
    pub internal_name: String,
    pub r#type: FullType,
    pub closure_source: bool,
    pub arg: bool,
}

/// Function definition
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Func {
    pub decl: FuncDecl,
    /// Function body. None if imported.
    pub body: Option<TypedExpr>,
    pub local_vars: Vec<LocalVar>,
    pub closure: Vec<ClosureRef>,
    pub local_var_map: HashMap<String, u32>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct TypeGuardBranch {
    /// The literal expression that tells us the type. 
    pub literal: TypedExpr,
    /// The type this is guarded to
    pub guard_type: Type,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct TypeGuard {
    pub branches: Vec<TypeGuardBranch>,
}

/// Function definition as seen from another module
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncDecl {
    pub name: String,
    pub return_type: FullType,
    pub args: Vec<FuncArg>,
    pub export: bool,
    pub generic_impl: bool,
    pub type_guard: Option<TypeGuard>,
    pub member_func: bool,
}

impl FuncDecl{
    pub fn get_arg_types(&self) -> Vec<FullType> {
        let mut out: Vec<FullType> = vec![];
        for arg in &self.args {
            out.push(arg.r#type.clone());
        }
        out
    }

    pub fn get_func_type(&self) -> FuncType {
        FuncType{out_type: self.return_type.clone(), in_types: self.get_arg_types()}
    }
}

/// Function declaration. This means a declaration site. In reality that means a closure creation plus 
/// reference to table pointer if it's consumed.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FuncObjectCreation {
    pub name: String,
    pub closure: Vec<ClosureRef>
}
