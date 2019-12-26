pub mod expr;
pub mod func;
pub mod intrinsic;
pub mod ast_types;

use expr::GlobalVariableDecl;
use func::Func;
use ast_types::UserType;
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

/// a fully parsed 'program'. A set of globals (with initialisations)
#[derive(PartialEq, Debug)]
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
    pub type_map: HashMap<String, UserType>,
}
