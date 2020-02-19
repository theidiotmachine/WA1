mod parser;
pub use parser::Parser;

pub mod prelude {
    pub use super::{Parser, StartFuncType};
}

use types::prelude::*;
use std::collections::HashMap;
use ast::prelude::*;
pub use errs::Error;
use ast::Imports;

#[derive(Debug, Clone, PartialEq)]
pub enum StartFuncType{
    ///The internal module start func
    Start,
    ///A full blown external start func
    WASMCallCtors
}


/// assert that something from an inner call was ok. Usage `let x = self.parse_x(); assert_ok!(x);`
#[macro_export]
macro_rules! assert_ok {
    ($e:ident) => (if $e.is_err() { return Err($e.unwrap_err()) }; let $e = $e?;)
}

/// Bridge into the old Res<> signature functions. Usage `let x = self.parse_x(); expect_ok!(x, parser_context, None);`
#[macro_export]
macro_rules! expect_ok {
    ($e:ident, $parser_context:ident, $error_return_value:ident) => (
        if $e.is_err() { 
            $parser_context.push_err($e.unwrap_err());
            return $error_return_value;
        } let $e = $e.unwrap();
    )
}

#[macro_export]
macro_rules! assert_semicolon {
    ($s:ident) => (
        let next = $s.next_item()?;
        if !next.token.matches_punct(Punct::SemiColon) {
            return $s.expected_token_error(&next, &[&";"]);
        }
    )
}

/// Get the next token, returning if it is not ok. Usage: `let next = assert_next!(self);`
#[macro_export]
macro_rules! assert_next {
    ($s:ident, $m:expr) => ({let next = $s.next_item(); if next.is_err() { return Err(Error::UnexpectedEoF($m.to_string())); }; next?} )
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

#[macro_export]
macro_rules! expect_punct {
    ($self:ident, $parser_context:ident, $punct:path, $error_return_value:ident) => (
        let e_next = $self.next_item();
        match e_next {
            Err(e) => {
                $parser_context.push_err(e);
                return $error_return_value;
            },
            Ok(next) => {
                if !next.token.matches_punct($punct) {
                    $parser_context.push_err(Error::UnexpectedToken(
                        next.location.clone(),
                        format!("Expected {}; found {:?}", $punct.to_string(), next.token),
                    ));
                }
            } 
        }
    )
}

#[macro_export]
macro_rules! expect_keyword {
    ($self:ident, $parser_context:ident, $keyword:path, $error_return_value:ident) => (
        let e_next = $self.next_item();
        match e_next {
            Err(e) => {
                $parser_context.push_err(e);
                return $error_return_value;
            },
            Ok(next) => {
                if !next.token.matches_keyword($keyword) {
                    $parser_context.push_err(Error::UnexpectedToken(
                        next.location.clone(),
                        format!("Expected {}; found {:?}", $keyword.to_string(), next.token),
                    ));
                }
            } 
        }
    )
}

/// Get the next token, returning if it is not ok. Usage: `let next = expect_next!(self, parser_context, None);`
#[macro_export]
macro_rules! expect_next {
    ($self:ident, $parser_context:ident, $error_return_value:ident) => (
        {
            let e_next = $self.next_item();
            match e_next {
                Err(e) => {
                    $parser_context.push_err(e);
                    return $error_return_value;
                },
                Ok(next) => {
                    next
                }
            }
        }
    )
}

#[macro_export]
macro_rules! assert_peek_punct {
    ($s:ident, $p:path) => (
        let next = $s.peek_next_item();
        if !next.token.matches_punct($p) {
            return $s.expected_token_error(&next, &[&format!("{:?}", $p)]);
        }
    )
}


#[macro_export]
macro_rules! assert_ident {
    ($n:ident, $m:expr) => (
        {
            let token = $n.token; 
            match token {
                Token::Ident(i) => i, 
                _ => return Err(Error::UnexpectedToken($n.location.clone(), $m.to_string())) 
            }
        }
    )
}

#[macro_export]
macro_rules! expect_ident {
    ($next:ident, $parser_context:ident, $message:expr) => (
        {
            let token = $next.token; 
            match token {
                Token::Ident(i) => i.to_string(), 
                _ => {
                    $parser_context.push_err(Error::UnexpectedToken($next.location.clone(), $message.to_string()));
                    String::from("")
                }
            }
        }
    )
}

#[macro_export]
macro_rules! expect_string_literal {
    ($next:ident, $parser_context:ident, $message:expr) => (
        {
            let token = $next.token; 
            match token {
                Token::String(i) => i.no_quote().to_owned(), 
                _ => {
                    $parser_context.push_err(Error::UnexpectedToken($next.location.clone(), $message.to_string()));
                    String::from("")
                }
            }
        }
    )
}

fn cast_int_to_number(expr: &TypedExpr) -> TypedExpr {
    TypedExpr{expr: Expr::IntToNumber(Box::new(expr.clone())), r#type: Type::Number, is_const: true, loc: expr.loc}
}

fn cast_int_to_bigint(expr: &TypedExpr) -> TypedExpr {
    TypedExpr{expr: Expr::IntToBigInt(Box::new(expr.clone())), r#type: Type::BigInt, is_const: true, loc: expr.loc}
}

fn free_type_widen(expr: &TypedExpr, to: &Type) -> TypedExpr {
    TypedExpr{expr: Expr::FreeTypeWiden(Box::new(expr.clone())), r#type: to.clone(), is_const: expr.is_const, loc: expr.loc}
}

fn create_cast(want: &Type, got: &TypedExpr, cast: &TypeCast) -> Option<TypedExpr> {
    match cast {
        TypeCast::FreeWiden => Some(free_type_widen(got, want)),
        TypeCast::IntToBigIntWiden => Some(cast_int_to_bigint(got)),
        TypeCast::IntToNumberWiden => Some(cast_int_to_number(got)),
        TypeCast::None => None,
        TypeCast::NotNeeded => Some(got.clone()),
    }
}

fn try_create_cast(want: &Type, got: &TypedExpr, implicit: bool) -> Option<TypedExpr> {
    let type_cast = types::cast::try_cast(&got.r#type, want, implicit);
    create_cast(want, got, &type_cast)
}

/// Type to indicate if this is a speculative parse (i.e. it may fail gracefully) of a required parse (in which case we)
/// must succeed
#[derive(Debug, PartialEq)]
pub enum Commitment{
    Speculative,
    Committed
}

pub trait Importer{
    fn import(&mut self, import_path_name: &String, from_path_name: &String) -> Result<Imports, String>;
}

/// Running state of the parser. Used to collect the AST as we build it.
#[derive(Debug)]
struct ParserContext {
    pub global_decls: Vec<GlobalVariableDecl>,
    pub global_imports: Vec<GlobalVariableImport>,
    pub func_decls: Vec<Func>,
    pub func_imports: Vec<FuncDecl>,
    pub generic_func_decls: Vec<GenericFunc>,
    pub errors: Vec<Error>,
    pub type_map: HashMap<String, TypeDecl>,
    pub import_namespace_map: HashMap<String, String>,
    pub is_unsafe: bool,
    pub file_name: String,
}

impl ParserContext {
    fn new(is_unsafe: bool, file_name: &String) -> ParserContext {
        ParserContext{
            global_decls: vec![],
            global_imports: vec![],
            func_decls: vec![],
            func_imports: vec![],
            generic_func_decls: vec![],
            errors: vec![],
            type_map: HashMap::new(),
            import_namespace_map: HashMap::new(),
            is_unsafe: is_unsafe,
            file_name: file_name.clone()
        }
    }

    fn push_err(&mut self, err: Error) {
        self.errors.push(err);
    }

    pub fn get_fn_decl_from_decls(&self, name: &String) -> Option<FuncDecl> {
        self.func_decls.iter().find(|&x| x.decl.name == *name).map(|x| x.decl.clone())
    }

    pub fn get_fn_decl_from_imports(&self, name: &String) -> Option<FuncDecl> {
        self.func_imports.iter().find(|&x| x.name == *name).map(|x| x.clone())
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

