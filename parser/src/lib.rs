use std::cmp;

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

fn patch_guard_type(
    o_old_guard_type: &Option<Type>,
    new_guard_type: &Type, 
    loc: &SourceLocation,
    parser_context: &mut ParserContext
) -> Type {
    match o_old_guard_type {
        None => new_guard_type.clone(),
        Some(old_guard_type) => {
            if old_guard_type == new_guard_type {
                parser_context.push_err(Error::TypeGuardReapply(loc.clone(), new_guard_type.clone()));
                new_guard_type.clone()
            } else {
                match new_guard_type{
                    Type::Int(lower_to, upper_to) => {
                        match old_guard_type {
                            Type::Int(lower_from, upper_from) => {
                                Type::Int(cmp::max(*lower_from, *lower_to), cmp::min(*upper_to, *upper_from))
                            },
                            _ => new_guard_type.clone()
                        }
                    },
                    _ => new_guard_type.clone()
                }
            }
        }
    }
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

#[derive(Debug, Clone, PartialEq, Copy)]
enum ParserPhase{
    ExportsPhase,
    MainPhase
}

fn cast_int_to_number(expr: &TypedExpr, to_mutability: Mutability) -> TypedExpr {
    TypedExpr{expr: Expr::IntToNumber(Box::new(expr.clone())), r#type: FullType{r#type: Type::Number, mutability: to_mutability}, loc: expr.loc}
}

fn int_widen(expr: &TypedExpr, to: &FullType) -> TypedExpr {
    TypedExpr{expr: Expr::IntWiden(Box::new(expr.clone())), r#type: to.clone(), loc: expr.loc}
}

fn free_upcast(expr: &TypedExpr, to: &FullType) -> TypedExpr {
    TypedExpr{expr: Expr::FreeUpcast(Box::new(expr.clone())), r#type: to.clone(), loc: expr.loc}
}

fn create_cast(want: &FullType, got: &TypedExpr, cast: &TypeCast) -> Option<TypedExpr> {
    match cast {
        TypeCast::FreeUpcast(_) => Some(free_upcast(got, want)),
        TypeCast::IntWiden(_,_) => Some(int_widen(got, want)),
        TypeCast::IntToNumberWiden => Some(cast_int_to_number(got, want.mutability)),
        TypeCast::None | TypeCast::ConstFail => None,
        TypeCast::NotNeeded => Some(got.clone()),
    }
}

fn try_create_cast(want: &FullType, got: &TypedExpr, cast_type: CastType) -> Option<TypedExpr> {
    let type_cast = types::cast::try_cast(&got.r#type, want, cast_type);
    create_cast(want, got, &type_cast)
}

fn cast_typed_expr(want: &FullType, got: Box<TypedExpr>, cast_type: CastType, parser_context: &mut ParserContext) -> TypedExpr {
    let type_cast = types::cast::try_cast(&got.r#type, want, cast_type);
    let loc = got.loc.clone();
    let got_mutability = got.r#type.mutability;
    match type_cast {
        TypeCast::FreeUpcast(_) => TypedExpr{expr: Expr::FreeUpcast(got), r#type: want.clone(), loc: loc},
        TypeCast::IntWiden(_,_) => TypedExpr{expr: Expr::IntWiden(got), r#type: want.clone(), loc: loc},
        TypeCast::IntToNumberWiden => TypedExpr{expr: Expr::IntToNumber(got), r#type: FullType::new(&Type::Number, got_mutability), loc: loc},
        TypeCast::None => {
            match cast_type {
                CastType::Implicit => 
                    parser_context.push_err(Error::TypeFailure(loc, want.clone(), got.r#type.clone())),
                CastType::Explicit  => 
                    parser_context.push_err(Error::CastFailure(loc, want.clone(), got.r#type.clone())),
            }
            TypedExpr{expr: Expr::FreeUpcast(got), r#type: want.clone(), loc: loc}
        },
        TypeCast::ConstFail => {
            parser_context.push_err(Error::ConstFailure(loc, want.clone(), got.r#type.clone()));
            TypedExpr{expr: Expr::FreeUpcast(got), r#type: want.clone(), loc: loc}
        },
        TypeCast::NotNeeded => {
            if cast_type == CastType::Explicit {
                parser_context.push_err(Error::CastNotNeeded(loc, want.r#type.clone()));
            }
            got.as_ref().clone()
        },
    }
}

fn guard_downcast_expr(want: &FullType, got: Box<TypedExpr>, parser_context: &mut ParserContext) -> TypedExpr {
    let type_cast = types::cast::try_guard_downcast_expr(&got.r#type, want);
    let loc = got.loc.clone();
    match type_cast {
        TypeGuardDowncast::FreeDowncast => TypedExpr{expr: Expr::FreeDowncast(got), r#type: want.clone(), loc: loc},
        TypeGuardDowncast::None => {
            parser_context.push_err(Error::CastFailure(loc, want.clone(), got.r#type.clone()));
            TypedExpr{expr: Expr::FreeDowncast(got), r#type: want.clone(), loc: loc}
        },
        TypeGuardDowncast::NotNeeded => got.as_ref().clone(),
    }
}

fn generic_wrap(want: &FullType, got: Box<TypedExpr>, parser_context: &mut ParserContext) -> TypedExpr {
    let generic_cast = types::cast::try_generic_wrap(&got.r#type, want);
    let loc = got.loc.clone();
    match generic_cast {
        GenericCast::FreeCast => TypedExpr{expr: Expr::FreeGenericCast(got), r#type: want.clone(), loc: loc},
        GenericCast::None => {
            parser_context.push_err(Error::InternalError(loc, format!("can't cast from {} to {}", got.r#type, want)));
            TypedExpr{expr: Expr::FreeUpcast(got), r#type: want.clone(), loc: loc}
        },
        GenericCast::NotNeeded => got.as_ref().clone(),
    }
}

fn generic_unwrap(want: &FullType, got: Box<TypedExpr>, parser_context: &mut ParserContext) -> TypedExpr {
    let generic_cast = types::cast::try_generic_unwrap(&got.r#type, want);
    let loc = got.loc.clone();
    match generic_cast {
        GenericCast::FreeCast => TypedExpr{expr: Expr::FreeGenericCast(got), r#type: want.clone(), loc: loc},
        GenericCast::None => {
            parser_context.push_err(Error::InternalError(loc, format!("can't cast from {} to {}", got.r#type, want)));
            TypedExpr{expr: Expr::FreeUpcast(got), r#type: want.clone(), loc: loc}
        },
        GenericCast::NotNeeded => got.as_ref().clone(),
    }
}

pub (crate) fn check_privacy(
    privacy: Privacy, 
    holding: &Type,
    member: &String,
    loc: &SourceLocation,
    parser_func_context: &mut ParserFuncContext,
    parser_context: &mut ParserContext,
) -> () {
    match &parser_func_context.this_type {
        Some(this_type) => {
            match privacy{
                Privacy::Public => {},
                Privacy::Protected => {
                    parser_context.push_err(Error::NotYetImplemented(loc.clone(), String::from("protected variables")))
                },
                Privacy::Private => {
                    if *holding != this_type.r#type {
                        parser_context.push_err(Error::EncapsulationFailure(loc.clone(), holding.clone(), member.clone()))
                    }
                }
            }
        },
        None => {
            if privacy != Privacy::Public {
                parser_context.push_err(Error::EncapsulationFailure(loc.clone(), holding.clone(), member.clone()))
            }
        }
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
        mutability: VariableMutability,
        internal_name: String,
        r#type: FullType,
        guard_type: Option<Type>,
    },
    /// Closure reference. Looks like a local, actually a member of the function's closure
    ClosureRef{
        mutability: VariableMutability,
        internal_name: String,
        r#type: FullType,
        guard_type: Option<Type>,
    },
}

impl ScopedVar{
    pub fn is_var(&self) -> bool {
        match self {
            ScopedVar::Local{ mutability, internal_name: _, r#type: _, guard_type: _} => {
                *mutability == VariableMutability::Variable
            },
            ScopedVar::ClosureRef{mutability, internal_name: _, r#type: _, guard_type: _} => {
                *mutability == VariableMutability::Variable
            },
        }
    }
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
    ExportsPhase,
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
    pub trait_map: HashMap<String, TraitDecl>,
    pub trait_impl_map: HashMap<(String, String), TraitImpl>,
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
        let mut trait_map = HashMap::new();
        let trait_name = String::from("IsAStruct");
        trait_map.insert(trait_name.clone(), TraitDecl{name: trait_name.clone(), member_funcs: vec![], export: true});
        ParserContext{
            global_decls: vec![],
            global_imports: vec![],
            func_decls: vec![],
            func_imports: vec![],
            generic_func_decls: vec![],
            generic_func_impls: HashSet::new(),
            errors: vec![],
            type_map: HashMap::new(),
            trait_map: trait_map,
            trait_impl_map: HashMap::new(),
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

    ///A trait type scope does not need a unique name: traits are global things. I think?
    fn push_trait_type_scope(&mut self, arg: &TypeArg) -> () {
        let mut v: HashMap<String, TypeArg> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };
   
        v.insert(arg.name.clone(), arg.clone());
        self.type_var_stack.push(TypeScope{var_names: v});
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
                        ScopedVar::Local{internal_name, r#type, guard_type, mutability} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), r#type: r#type.clone(), guard_type: guard_type.clone(), mutability: *mutability},
                        ScopedVar::ClosureRef{internal_name, r#type, guard_type, mutability} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), r#type: r#type.clone(), guard_type: guard_type.clone(), mutability: *mutability},
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

    fn add_var(&mut self, var_name: &String, internal_var_name: &String, r#type: &FullType, mutability: VariableMutability) -> () {
        let o_head = self.func_var_stack.last_mut();
        match o_head {
            None => {},
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability});
            }
        }

        let o_head = self.block_var_stack.last_mut();
        match o_head {
            None => {},
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability});
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
                        ScopedVar::Local{internal_name: this_internal_name, r#type: _, guard_type: _, mutability: _} => this_internal_name == internal_name,
                        ScopedVar::ClosureRef{internal_name: this_internal_name, r#type: _, guard_type: _, mutability: _} => this_internal_name == internal_name,
                    }
                }).map(|(n, sv)| (n.clone(), sv.clone())).unwrap()
            }
        }
    }

    fn patch_var(&mut self, 
        var_name: &String,
        new_var: &ScopedVar
    ) -> () {
        let o_head = self.func_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), new_var.clone());
            }
        }

        let o_head = self.block_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), new_var.clone());
            }
        }
    }

    /// Apply a type guard to an existing variable.
    fn guard_var(&mut self, 
        internal_var_name: &String, 
        guard_type: &Type,
        loc: &SourceLocation,
    ) -> () {
        //find the var in the block_var_stack
        let (var_name, shadowed_var) = self.find_named_scoped_var_given_internal_name(internal_var_name);

        let new_var = match shadowed_var {
            ScopedVar::Local{internal_name: _, r#type, guard_type: old_guard_type, mutability} => {
                ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(),
                    guard_type: Some(patch_guard_type(&old_guard_type, guard_type, loc, self)),
                    mutability: mutability
                }
            },
            ScopedVar::ClosureRef{internal_name: _, r#type, guard_type: old_guard_type, mutability} => {
                ScopedVar::ClosureRef{internal_name: internal_var_name.clone(), r#type: r#type.clone(), 
                    guard_type: Some(patch_guard_type(&old_guard_type, guard_type, loc, self)),
                    mutability: mutability
                }
            }
        };

        self.patch_var(&var_name, &new_var)
    }

    fn unguard_var(&mut self, 
        internal_var_name: &String, 
    ) -> () {
        let (var_name, shadowed_var) = self.find_named_scoped_var_given_internal_name(internal_var_name);

        let o_new_var = match shadowed_var {
            ScopedVar::Local{internal_name: _, r#type, guard_type: o_guard_type, mutability} => {
                match o_guard_type {
                    Some(_) => Some(ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability: mutability}),
                    None => None
                }
            },
            ScopedVar::ClosureRef{internal_name: _, r#type, guard_type: o_guard_type, mutability} => {
                match o_guard_type {
                    Some(_) => Some(ScopedVar::ClosureRef{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability: mutability}),
                    None => None
                }
            }
        };

        match o_new_var {
            Some(new_var) => self.patch_var(&var_name, &new_var),
            None => {}
        }
    }

    fn get_scoped_var(&self, var_name: &String) -> Option<&ScopedVar> {
        match self.block_var_stack.last() {
            None => None,
            Some(s) => {
                s.var_names.get(var_name)
            } 
        }
    }

    fn get_type_decl(&self, name: &String)-> Option<TypeDecl> {
        self.type_map.get(name).cloned()
    }

    fn append_type_decl_member_func(&mut self, type_name: &String, mf: &MemberFunc) -> () {
        let td = self.type_map.get_mut(type_name).unwrap();
        match td {
            TypeDecl::Type{name: _, inner: _, type_args: _, export: _, member_funcs, constructor: _, under_construction: _} => {
                member_funcs.push(mf.clone());
            },
            _ => unreachable!()
        }
    }

    fn get_global_var(&self, name: &String) -> Option<&GlobalVariableDecl> {
        self.global_decls.iter().find(|&x| x.name == *name)
    }

    fn type_implements_trait(&self, t: &Type, trait_name: &String) -> bool {
        self.trait_impl_map.contains_key(&(t.get_type_name(), trait_name.clone()))
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
    pub given_func_return_type: FullType,
    pub implied_func_return_type: FullType,
    /// If we have entered a loop block
    pub in_iteration: bool,
    pub this_type: Option<FullType>,
}

impl ParserFuncContext{
    pub fn new(this_type: &Option<FullType>) -> ParserFuncContext{
        ParserFuncContext{
            local_vars: vec![],
            local_var_map: HashMap::new(),
            closure: vec![],
            given_func_return_type: FullType::new(&Type::Undeclared, Mutability::Unknown),
            implied_func_return_type: FullType::new(&Type::Undeclared, Mutability::Unknown),
            in_iteration: false,
            this_type: this_type.clone(),
        }
    }
}

/// The result type for the Parser operations
type Res<T> = Result<T, Error>;

