use serde::{Serialize, Deserialize};

pub mod expr;
pub mod func;
pub mod intrinsic;
pub mod ast_types;

use expr::{GlobalVariableDecl, GlobalVariableImport};
use func::{Func, FuncDecl};
use ast_types::TypeDecl;
use std::collections::HashMap;
use types::prelude::TypeArg;

pub mod prelude {
    pub use super::func::prelude::*;
    pub use super::expr::prelude::*;
    pub use super::intrinsic::prelude::*;
    pub use super::ast_types::prelude::*;
    pub use super::AST;
    pub use super::Exports;
    pub use super::Imports;
    pub use super::GenericFunc;
}

#[macro_use]
extern crate lazy_static;

/// A generic function that consumes type arguments and can generate a concrete function
#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct GenericFunc{
    pub type_args: Vec<TypeArg>,
    pub func: Func
}

/// The AST of a fully parsed program. 
#[derive(PartialEq, Debug, Deserialize, Serialize)]
pub struct AST {
    ///global variables declared in this program
    pub global_decls: Vec<GlobalVariableDecl>,

    ///global variables imported in this program
    pub global_imports: Vec<GlobalVariableImport>,
    
    pub func_decls: Vec<Func>,
    pub func_imports: Vec<FuncDecl>,

    pub generic_func_decls: Vec<GenericFunc>,
    
    //start function
    pub start: String,

    pub type_map: HashMap<String, TypeDecl>,
}

/// The things exported from a compilation unit. Not really part of the AST but very related.
#[derive(Debug, Deserialize, Serialize)]
pub struct Exports{
    pub global_decls: Vec<GlobalVariableDecl>,
    pub global_imports: Vec<GlobalVariableImport>,
    pub func_decls: Vec<FuncDecl>,
    pub func_imports: Vec<FuncDecl>,
    pub generic_func_decls: Vec<GenericFunc>,
    pub types: Vec<TypeDecl>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Imports{
    pub exports: Exports,
    pub unique_name: String,
    pub module_name: String
}

impl Exports{
    pub fn new() -> Exports {
        Exports{ global_decls: vec![], global_imports: vec![], func_decls: vec![], func_imports: vec![], generic_func_decls: vec![], types: vec![] }
    }
}