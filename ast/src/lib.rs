pub mod stmt;
pub mod expr;
pub mod func;
pub mod types;

use stmt::GlobalVariableDecl;
use stmt::Stmt;
use func::Func;
use std::collections::HashMap;

pub mod prelude {
    pub use super::stmt::prelude::*;
    pub use super::func::prelude::*;
    pub use super::types::prelude::*;
    pub use super::expr::prelude::*;
    pub use super::Program;
}

/// a fully parsed 'program'. A set of globals (with initialisations)
#[derive(PartialEq, Debug)]
pub struct Program {
    ///global variables
    pub globals: Vec<GlobalVariableDecl>,
    ///all the functions
    pub funcs: Vec<Func>,
    //any other initializing statements
    pub init: Vec<Stmt>,
    /// name to index map
    pub global_var_map: HashMap<String, u32>,
    pub func_map: HashMap<String, u32>,
}
