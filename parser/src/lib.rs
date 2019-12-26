mod parser;
pub use parser::Parser;

pub mod prelude {
    pub use super::Parser;
}

use types::prelude::*;
use std::collections::HashMap;
use ast::prelude::*;
pub use errs::Error;

/// assert that something from an inner call was ok. Usage `let sthing = self.parse_sthing(); assert_ok!(sthing);`
#[macro_export]
macro_rules! assert_ok {
    ($e:ident) => (if $e.is_err() { return Err($e.unwrap_err()) }; let $e = $e?;)
}

#[macro_export]
macro_rules! assert_punct {
    ($s:ident, $p:path) => (
        let next = $s.next_item()?;
        if !next.token.matches_punct($p) {
            return $s.expected_token_error(&next, &[&format!("{:?}", $p)]);
        }
    )
}

#[derive(Debug)]
struct ParserContext {
    pub globals: Vec<GlobalVariableDecl>,
    pub global_var_map: HashMap<String, u32>,
    pub funcs: Vec<Func>,
    pub func_map: HashMap<String, u32>,
    pub errors: Vec<Error>,
    pub type_map: HashMap<String, UserType>
}

impl ParserContext {
    fn new() -> ParserContext {
        ParserContext{
            globals: vec![],
            funcs: vec![],
            errors: vec![],
            global_var_map: HashMap::new(),
            func_map: HashMap::new(),
            type_map: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct ParserFuncContext{
    pub local_vars: Vec<VariableDecl>,
    pub local_var_map: HashMap<String, u32>,
    pub closure: Vec<ClosureRef>,
    pub func_return_type: Type,
}

impl ParserFuncContext{
    pub fn new() -> ParserFuncContext{
        ParserFuncContext{
            local_vars: vec![],
            local_var_map: HashMap::new(),
            closure: vec![],
            func_return_type: Type::Undeclared,
        }
    }
}

/// The start/end index of a line
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Line {
    start: usize,
    end: usize,
}

/// The result type for the Parser operations
type Res<T> = Result<T, Error>;
