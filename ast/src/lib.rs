use serde::{Serialize, Deserialize};

pub mod expr;
pub mod func;
pub mod intrinsic;
pub mod ast_types;

use expr::GlobalVariableDecl;
use func::{Func, FuncDecl};
use ast_types::TypeDecl;
use std::collections::HashMap;

pub mod prelude {
    pub use super::func::prelude::*;
    pub use super::expr::prelude::*;
    pub use super::intrinsic::prelude::*;
    pub use super::ast_types::prelude::*;
    pub use super::Program;
}

#[macro_use]
extern crate lazy_static;

/// The AST of a fully parsed program. 
#[derive(PartialEq, Debug, Deserialize, Serialize)]
pub struct Program {
    ///global variables
    pub globals: Vec<GlobalVariableDecl>,
    ///all the functions
    pub funcs: Vec<Func>,
    //start function
    pub start: String,
    /// name to index map
    pub global_var_map: HashMap<String, u32>,
    pub func_map: HashMap<String, u32>,
    pub type_map: HashMap<String, TypeDecl>,
}

/// The things exported from a compilation unit. Not reallY part of the AST but very related.
#[derive(Debug, Deserialize, Serialize)]
pub struct Exports{
    pub globals: Vec<GlobalVariableDecl>,
    pub funcs: Vec<FuncDecl>,
    pub types: Vec<TypeDecl>,
}

impl Exports{
    pub fn new() -> Exports {
        Exports{ globals: vec![], funcs: vec![], types: vec![] }
    }
}