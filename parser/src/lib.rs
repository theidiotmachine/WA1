mod parser;
pub use parser::Parser;

pub mod prelude {
    pub use super::Parser;
}

use types::prelude::*;
use std::collections::HashMap;
use ast::prelude::*;
pub use errs::Error;
use ast::Exports;

/// assert that something from an inner call was ok. Usage `let sthing = self.parse_sthing(); assert_ok!(sthing);`
#[macro_export]
macro_rules! assert_ok {
    ($e:ident) => (if $e.is_err() { return Err($e.unwrap_err()) }; let $e = $e?;)
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
    ($self:ident, $parser_context:ident, $punct:path) => (
        let e_next = $self.next_item();
        match e_next {
            Err(e) => {
                $parser_context.push_err(e);
                return None;
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
    ($self:ident, $parser_context:ident, $keyword:path) => (
        let e_next = $self.next_item();
        match e_next {
            Err(e) => {
                $parser_context.push_err(e);
                return None;
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

/// Get the next token, returning if it is not ok. Usage: `let next = expect_next!(self, parser_context);`
#[macro_export]
macro_rules! expect_next {
    ($self:ident, $parser_context:ident) => (
        {
            let e_next = $self.next_item();
            match e_next {
                Err(e) => {
                    $parser_context.push_err(e);
                    return None;
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

#[derive(Debug, PartialEq)]
pub enum Commitment{
    Speculative,
    Commited
}

pub trait Importer{
    fn import(&mut self, path_name: &String) -> Exports;
}

#[derive(Debug)]
struct ParserContext {
    pub globals: Vec<GlobalVariableDecl>,
    pub global_var_map: HashMap<String, u32>,
    pub funcs: Vec<Func>,
    pub func_map: HashMap<String, u32>,
    pub errors: Vec<Error>,
    pub type_map: HashMap<String, TypeDecl>,
    pub import_namespace_map: HashMap<String, String>,
    pub is_unsafe: bool,
    pub is_pic: bool,
}

impl ParserContext {
    fn new(is_unsafe: bool, is_pic: bool) -> ParserContext {
        ParserContext{
            globals: vec![],
            funcs: vec![],
            errors: vec![],
            global_var_map: HashMap::new(),
            func_map: HashMap::new(),
            type_map: HashMap::new(),
            import_namespace_map: HashMap::new(),
            is_unsafe: is_unsafe,
            is_pic: is_pic,
        }
    }

    fn push_err(&mut self, err: Error) {
        self.errors.push(err);
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

