mod parser;
pub use parser::Parser;

mod tree_transform;

pub mod prelude {
    pub use super::{Parser, StartFuncType, UnsafeParseMode};
}

use types::prelude::*;
use std::collections::{HashMap, HashSet};
use ast::prelude::*;
pub use errs::Error;
use errs::prelude::*;
use ast::Imports;

#[macro_use]
extern crate lazy_static;

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
        if !$s.has_line_term {
            let next = $s.next_item()?;
            if !next.token.matches_punct(Punct::SemiColon) {
                return $s.expected_token_error(&next, &[&";"]);
            }
        }
    )
}

#[macro_export]
macro_rules! expect_semicolon {
    ($self:ident, $parser_context:ident) => (
        if !$self.has_line_term {
            let next = $self.peek_next_item(); 
            if next.token.matches_punct(Punct::SemiColon) || next.token.is_eof() {
                $self.skip_next_item();
            } else {
                $parser_context.push_err(Error::UnexpectedToken(next.location.clone(), format!("Expected ';' or line end")));
            }
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
        let next = $self.peek_next_item();
        if next.token.matches_punct($punct) {
            $self.skip_next_item();
        } else {
            $parser_context.push_err(Error::UnexpectedToken(next.location.clone(), format!("Expected '{}", $punct.to_string())));
        }
    )
}

#[macro_export]
macro_rules! expect_keyword {
    ($self:ident, $parser_context:ident, $keyword:path) => (
        let next = $self.peek_next_item();
        if next.token.matches_keyword($keyword) {
            $self.skip_next_item();
        } else {
            $parser_context.push_err(Error::UnexpectedToken(next.location.clone(), format!("Expected '{}", $keyword.to_string())));
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

fn try_create_cast(want: &Type, got: &TypedExpr, cast_type: CastType) -> Option<TypedExpr> {
    let type_cast = types::cast::try_cast(&got.r#type, want, cast_type);
    create_cast(want, got, &type_cast)
}

fn cast_typed_expr(want: &Type, got: Box<TypedExpr>, cast_type: CastType, parser_context: &mut ParserContext) -> TypedExpr {
    let type_cast = types::cast::try_cast(&got.r#type, want, cast_type);
    let loc = got.loc.clone();
    let got_is_const = got.is_const;
    match type_cast {
        TypeCast::FreeWiden => TypedExpr{expr: Expr::FreeTypeWiden(got), r#type: want.clone(), is_const: got_is_const, loc: loc},
        TypeCast::IntToBigIntWiden => TypedExpr{expr: Expr::IntToBigInt(got), r#type: Type::BigInt, is_const: true, loc: loc},
        TypeCast::IntToNumberWiden => TypedExpr{expr: Expr::IntToNumber(got), r#type: Type::Number, is_const: true, loc: loc},
        TypeCast::None => {
            match cast_type {
                CastType::Implicit => 
                    parser_context.push_err(Error::TypeFailure(loc, want.clone(), got.r#type.clone())),
                CastType::Explicit | CastType::GuardForce => 
                    parser_context.push_err(Error::CastFailure(loc, want.clone(), got.r#type.clone())),
                CastType::GenericForce => 
                    parser_context.push_err(Error::InternalError(loc, format!("can't cast from {} to {}", got.r#type, want))),
            }
            TypedExpr{expr: Expr::FreeTypeWiden(got), r#type: want.clone(), is_const: got_is_const, loc: loc}
        },
        TypeCast::NotNeeded => got.as_ref().clone(),
    }
}

/// Type to indicate if this is a speculative parse (i.e. it may fail gracefully) of a required parse (in which case we
/// must succeed)
#[derive(Debug, PartialEq)]
pub enum Commitment{
    Speculative,
    Committed
}

pub trait Importer{
    fn import(&mut self, import_path_name: &String, from_path_name: &String) -> Result<Imports, String>;
}

#[derive(Debug, Clone, PartialEq)]
enum ScopedVar{
    /// Actual local variable
    Local{
        internal_name: String,
        r#type: Type,
        constant: bool,
        guard_type: Option<Type>,
    },
    /// Closure reference. Looks like a local, actually a member of the function's closure
    ClosureRef{
        internal_name: String,
        r#type: Type,
        constant: bool,
        guard_type: Option<Type>,
    },
}

#[derive(Debug)]
struct Scope{
    pub var_names: HashMap<String, ScopedVar>,
}

impl<> Default for Scope<> {
    fn default() -> Self {
        Self {
            var_names: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct TypeScope{
    pub var_names: HashMap<String, TypeArg>,
}

impl<> Default for TypeScope<> {
    fn default() -> Self {
        Self {
            var_names: HashMap::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnsafeParseMode{
    Safe,
    Unsafe,
    Phase1,
}

/// Running state of the parser. Used to collect the AST as we build it.
#[derive(Debug)]
struct ParserContext {
    pub global_decls: Vec<GlobalVariableDecl>,
    pub global_imports: Vec<GlobalVariableImport>,
    pub func_decls: Vec<Func>,
    pub func_imports: Vec<FuncDecl>,
    pub generic_func_decls: Vec<GenericFunc>,
    pub generic_func_impls: HashSet<String>,
    pub errors: Vec<Error>,
    pub type_map: HashMap<String, TypeDecl>,
    pub import_namespace_map: HashMap<String, String>,
    pub unsafe_parse_mode: UnsafeParseMode,
    pub file_name: String,

    /// type variables
    type_var_stack: Vec<TypeScope>,

    /// Uniqueness counter
    counter: u64,

    /// local variables
    func_var_stack: Vec<Scope>,
    block_var_stack: Vec<Scope>,
}


impl ParserContext {
    fn new(unsafe_parse_mode: UnsafeParseMode, file_name: &String) -> ParserContext {
        ParserContext{
            global_decls: vec![],
            global_imports: vec![],
            func_decls: vec![],
            func_imports: vec![],
            generic_func_decls: vec![],
            generic_func_impls: HashSet::new(),
            errors: vec![],
            type_map: HashMap::new(),
            import_namespace_map: HashMap::new(),
            unsafe_parse_mode: unsafe_parse_mode,
            file_name: file_name.clone(),
            type_var_stack: vec![],
            counter: 0,
            block_var_stack: Vec::new(),
            func_var_stack: Vec::new(),
        }
    }

    fn push_err(&mut self, err: Error) {
        self.errors.push(err);
    }

    pub fn get_fn_decl_from_decls(&self, name: &String) -> Option<FuncDecl> {
        self.func_decls.iter().find(|&x| x.decl.name == *name).map(|x| x.decl.clone())
    }

    pub fn get_generic_fn_from_generics(&self, name: &String) -> Option<GenericFunc> {
        self.generic_func_decls.iter().find(|&x| x.func.decl.name == *name).map(|x| x.clone())
    }

    pub fn get_fn_decl_from_imports(&self, name: &String) -> Option<FuncDecl> {
        self.func_imports.iter().find(|&x| x.name == *name).map(|x| x.clone())
    }

    fn push_type_scope(&mut self, args: &Vec<TypeArg>) -> Vec<TypeArg> {
        let mut v: HashMap<String, TypeArg> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };

        let mut out: Vec<TypeArg> = vec![];
        
        for arg in args {
            let type_arg = TypeArg{name: self.get_unique_name(&arg.name), constraint: arg.constraint.clone()};
            out.push(type_arg.clone());
            v.insert(arg.name.clone(), type_arg);
        }
        self.type_var_stack.push(TypeScope{var_names: v});
        out
    }

    fn push_empty_type_scope(&mut self) {
        let v: HashMap<String, TypeArg> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };
        
        self.type_var_stack.push(TypeScope{var_names: v});
    }

    fn pop_type_scope(&mut self) {
        self.type_var_stack.pop();
    }

    fn get_scoped_type(&mut self, var_name: &String) -> Option<&TypeArg> {
        match self.type_var_stack.last() {
            None => None,
            Some(s) => {
                s.var_names.get(var_name)
            } 
        }
    }

    fn has_type_stack(&self) -> bool {
        !self.type_var_stack.is_empty()
    }

    fn get_unique_name(&mut self, name: &String) -> String {
        let counter = self.counter;
        self.counter += 1;
        format!("{}#{}", name, counter)
    }

    fn get_global_decl(&self, id: &String) -> Option<&GlobalVariableDecl> {
        self.global_decls.iter().find(|&x| x.name == id.to_string())
    }

    fn push_empty_block_scope(&mut self) {
        self.block_var_stack.push(Scope::default());
    }

    fn push_empty_func_scope(&mut self) {
        self.func_var_stack.push(Scope::default());
    }

    fn push_block_scope(&mut self) {
        let o_head = self.block_var_stack.last();
        match o_head {
            None => self.push_empty_block_scope(),
            Some(head) => {
                let mut new_var_names: HashMap<String, ScopedVar> = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    new_var_names.insert(key.clone(), val.clone());
                }

                self.block_var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    fn push_func_scope(&mut self) {
        let o_head = self.func_var_stack.last();
        match o_head {
            None => {
                self.push_empty_func_scope();
                self.push_empty_block_scope();
            },
            Some(head) => {
                let mut new_var_names: HashMap<String, ScopedVar> = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    let new_val = match val {
                        ScopedVar::Local{internal_name, r#type, constant, guard_type} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), constant: *constant, r#type: r#type.clone(), guard_type: guard_type.clone()},
                        ScopedVar::ClosureRef{internal_name, r#type, constant, guard_type} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), constant: *constant, r#type: r#type.clone(), guard_type: guard_type.clone()},
                    };
                    new_var_names.insert(key.clone(), new_val);
                }

                self.func_var_stack.push(Scope{var_names: new_var_names.clone()});

                self.block_var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    fn pop_block_scope(&mut self) {
        self.block_var_stack.pop();
    }

    fn pop_func_scope(&mut self) {
        self.block_var_stack.pop();
        self.func_var_stack.pop();
    }

    fn add_var(&mut self, var_name: &String, internal_var_name: &String, r#type: &Type, constant: bool)  -> () {
        let o_head = self.func_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), constant: constant, guard_type: None});
            }
        }

        let o_head = self.block_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), constant: constant, guard_type: None});
            }
        }
    }

    fn find_named_scoped_var_given_internal_name(&self, internal_name: &String) -> (String, ScopedVar) {
        let o_head = self.block_var_stack.last();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.iter().find(|(_, sv)| { 
                    match sv {
                        ScopedVar::Local{internal_name: this_internal_name, r#type: _, constant: _, guard_type: _} => this_internal_name == internal_name,
                        ScopedVar::ClosureRef{internal_name: this_internal_name, r#type: _, constant: _, guard_type: _} => this_internal_name == internal_name,
                    }
                }).map(|(n, sv)| (n.clone(), sv.clone())).unwrap()
            }
        }
    }

    /// Add a new var with a type guard. Returns the generated internal name, and the old unguarded type.
    fn add_guarded_var(&mut self, 
        var_name: &String, 
        guard_type: &Type
    ) -> (String, Type) {
        //find the var in the block_var_stack
        let (external_var_name, shadowed_var) = self.find_named_scoped_var_given_internal_name(var_name);

        let internal_var_name = self.get_unique_name(&external_var_name);

        let (new_var, unguarded_type) = match shadowed_var {
            ScopedVar::Local{internal_name: _, r#type, constant, guard_type: _} => {
                (ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), constant: constant, guard_type: Some(guard_type.clone())}, r#type.clone())
            },
            ScopedVar::ClosureRef{internal_name: _, r#type, constant, guard_type: _} => {
                (ScopedVar::ClosureRef{internal_name: internal_var_name.clone(), r#type: r#type.clone(), constant: constant, guard_type: Some(guard_type.clone())}, r#type.clone())
            }
        };

        let o_head = self.func_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(external_var_name.clone(), new_var.clone());
            }
        }

        let o_head = self.block_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(external_var_name.clone(), new_var);
            }
        }

        (internal_var_name, unguarded_type)
    }

    fn get_scoped_var(&self, var_name: &String) -> Option<&ScopedVar> {
        match self.block_var_stack.last() {
            None => None,
            Some(s) => {
                s.var_names.get(var_name)
            } 
        }
    }
}

impl ErrRecorder for ParserContext {
    fn push_err(&mut self, err: Error) {
        self.errors.push(err);
    }
}

#[derive(Debug)]
struct ParserFuncContext{
    pub local_vars: Vec<LocalVar>,
    pub local_var_map: HashMap<String, u32>,
    pub closure: Vec<ClosureRef>,
    pub given_func_return_type: Type,
    pub implied_func_return_type: Type,
    /// If we have entered a loop block
    pub in_iteration: bool,
}

impl ParserFuncContext{
    pub fn new() -> ParserFuncContext{
        ParserFuncContext{
            local_vars: vec![],
            local_var_map: HashMap::new(),
            closure: vec![],
            given_func_return_type: Type::Undeclared,
            implied_func_return_type: Type::Undeclared,
            in_iteration: false,
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

