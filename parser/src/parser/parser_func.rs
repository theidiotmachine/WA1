//!Module to parse function declarations and calls. This is a complicated file because
//! it tries to share a lot of code between the various types of functions that are supported.
//! These are
//! 1. simple functions. These have completely understood at initial parse time and have no hidden arguments.
//! We call them 'specific' (the opposite of generic).
//! ```
//! fn square(x: Int) => x * x`
//! ```
//! 2. generic functions. These are not understood until generic resolution time.
//! ```
//! fn id<T>(x: T) -> T x
//! ```
//! 3. Simple member functions. These have a hidden `this` argument which are understood at initial parse time.
//! 4. Generic member functions. These are member functions of a non generic type which are themselves generic.
//! ```
//! type A = Int {
//!     fn f<T>(x: T) -> x
//! }
//! ```
//! 5. Simple member functions of generic types. Even though they have not been declared as generic, actually
//! they are because the hidden `this` argument is generic
//! ```
//! type B<T> = C<T> {
//!     fn sz() -> Int this.sz
//! }
//! ```
//! 6. Generic member functions of generic types. Here, both the type and the function are generic, and the 
//! resolution needs to combine both sets of type arguments.
//! 7. Simple constructor functions. These are never member functions. In reality they are actually syntactical
//! sugar for type 1, simple functions.
//! 8. Generic constructor functions. If the type being generated is a generic, you need a generic function to 
//! construct it. In reality they are syntax sugar for type 2 functions.

use std::collections::{HashMap};
use std::convert::TryInto;

use crate::Parser;
use crate::ParserContext;
use crate::{ParserFuncContext, ParserPhase};

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
pub use errs::prelude::*;

use crate::tree_transform::{Transform, transform_expr, transform_lvalue_expr, transform_typed_expr, transform_func_decl};

use crate::{expect_keyword, expect_next, expect_ident, expect_punct, cast_typed_expr, generic_unwrap, generic_wrap, expect_semicolon};

struct SimpleErrRecorder{
    errs: Vec<Error>
}

impl SimpleErrRecorder{
    pub fn write_to_p_c(&self, parser_context: &mut ParserContext) -> () {
        for e in &self.errs {
            parser_context.push_err(e.clone());
        }
    }

    pub fn new() -> SimpleErrRecorder {
        SimpleErrRecorder{errs: vec![]}
    }
}

impl ErrRecorder for SimpleErrRecorder{
    fn push_err(&mut self, err: Error) -> () {
        self.errs.push(err);
    }
}

/// Take a generic func, and a set of values for the type variables, and instantiate a
/// resolved func decl. 
fn resolve_generic_func_decl<'a>(
    generic_func: &GenericFunc,
    resolved_name: &String,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> FuncDecl{
    let mut out_args: Vec<FuncArg> = vec![];
    for orig_arg in &generic_func.func.decl.args {
        let new_type = transform_type(&orig_arg.r#type.r#type, type_map, loc, parser_context);
        out_args.push(FuncArg{name: orig_arg.name.clone(), r#type: FullType::new(&new_type, orig_arg.r#type.mutability), mutability: orig_arg.mutability});
    }

    let out_return_type = transform_type(&generic_func.func.decl.return_type.r#type, type_map, loc, parser_context);

    let type_guard = if generic_func.func.decl.type_guard.is_some() {
        let mut err_recorder = SimpleErrRecorder::new();
        let t_g = generic_func.func.decl.type_guard.clone().unwrap();
        let mut t_g_b_s = vec![];
        for t_g_b in t_g.branches {
            let guard_type = transform_type(&t_g_b.guard_type, type_map, loc, parser_context); 
            let mut transformer = GenericFuncTypeTransformer::new(type_map.clone(), parser_context);
            t_g_b_s.push(TypeGuardBranch{
                literal: transform_typed_expr(&t_g_b.literal, &mut transformer, &mut err_recorder),
                guard_type: guard_type 
            })
        }
        err_recorder.write_to_p_c(parser_context);
        Some(TypeGuard{branches: t_g_b_s})
    } else {
        None
    };

    FuncDecl{args: out_args, export: generic_func.func.decl.export, name: resolved_name.clone(), 
        return_type: FullType::new(&out_return_type, generic_func.func.decl.return_type.mutability), 
        generic_impl: true, type_guard: type_guard, member_func: generic_func.func.decl.member_func
    }
}

fn transform_types(ts: &Vec<Type>,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> Vec<Type> {
    let mut out = vec![];
    for t in ts {
        out.push(transform_type(t, type_map, loc, parser_context));
    }
    out
}

fn transform_full_types(ts: &Vec<FullType>,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> Vec<FullType> {
    let mut out = vec![];
    for t in ts {
        out.push(FullType::new(&transform_type(&t.r#type, type_map, loc, parser_context), t.mutability));
    }
    out
}

/// Substitute all type variables for their concrete values
fn transform_type(t: &Type,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> Type {
    match t {
        Type::Any | Type::Bool | Type::FakeVoid | Type::FloatLiteral(_) 
        | Type::Int(_, _) | Type::ModuleLiteral(_) | Type::Never | Type::Number | Type::RealVoid | Type::String 
        | Type::StringLiteral(_) | Type::Undeclared | Type::Unknown | Type::UnsafePtr | Type::UnsafeNull
        | Type::UnsafeStruct{name: _}
            => t.clone(),
        Type::Array(t) => Type::Array(Box::new(transform_type(t, type_map, loc, parser_context))),
        Type::Func{func_type} => {
            Type::Func{func_type: Box::new(FuncType{
                out_type: FullType::new(&transform_type(&func_type.out_type.r#type, type_map, loc, parser_context), func_type.out_type.mutability),
                in_types: transform_full_types(&func_type.in_types, type_map, loc, parser_context)
            })}
        },
        Type::ObjectLiteral(oles) => {
            Type::ObjectLiteral(oles.iter().map(|(k, v)| (k.clone(), transform_type(&v, type_map, loc, parser_context))).collect())
        },
        Type::Option(t) => Type::Option(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::Some(t) => Type::Some(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::Tuple(ts) => Type::Tuple(transform_types(&ts, type_map, loc, parser_context)),
        Type::TypeLiteral(t) => Type::TypeLiteral(Box::new(FullType::new(&transform_type(&t.r#type, type_map, loc, parser_context), t.mutability))),
        Type::UnsafeArray(t) => Type::UnsafeArray(Box::new(transform_type(t, type_map, loc, parser_context))),
        Type::UnsafeOption(t) => Type::UnsafeOption(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::UnsafeSome(t) => Type::UnsafeSome(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::UserType{name, type_args, inner} => 
            Type::UserType{name: name.clone(), type_args: transform_types(&type_args, type_map, loc, parser_context), inner: Box::new(transform_type(&inner, type_map, loc, parser_context))},
        Type::VariableUsage{name, constraint: _} => {
            let o_n_t = type_map.get(name);
            match o_n_t {
                Some(n_t) => {
                    (*n_t).clone()
                },
                None => {
                    parser_context.push_err(Error::UnresolvedTypeArg(loc.clone(), name.clone()));
                    t.clone()
                }
            }
        },
    }
}

fn transform_generic_local_vars(
    generic_local_vars: &Vec<LocalVar>,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> Vec<LocalVar> {
    let mut out = vec![];
    for generic_local_var in generic_local_vars {
        let new_type = transform_type(&generic_local_var.r#type.r#type, type_map, loc, parser_context);
        out.push(LocalVar{
            internal_name: generic_local_var.internal_name.clone(),
            r#type: FullType::new(&new_type, generic_local_var.r#type.mutability),
            closure_source: generic_local_var.closure_source,
            arg: generic_local_var.arg,
            mutability: generic_local_var.mutability
        })
    }
    out
}

/// Because the return type of 'return' is 'never' we need to do some weird checks
fn get_body_return_type(
    body: &TypedExpr,
) -> FullType {
    match &body.expr {
        Expr::Block(exprs) => {
            let o_last = exprs.last();
            match o_last {
                Some(last) => get_body_return_type(&last),
                _ => FullType::new_const(&Type::RealVoid)
            }
        },
        Expr::Return(o_e) => {
            if o_e.is_none() {
                FullType::new_const(&Type::RealVoid)
            } else {
                o_e.as_ref().as_ref().unwrap().r#type.clone()
            }
        },
        _ => body.r#type.clone()
    }
}

///Given a generic func, a set of arguments and a set of types that the generic function
/// is being resolved with, generate an actual function call.
fn generate_generic_func_call(
    func_name: &String,
    generic_func: &GenericFunc,
    args: Vec<TypedExpr>,
    resolved_types: &Vec<Type>,
    resolved_func_decl: &FuncDecl,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> TypedExpr {
    //can this actually be resolved? Are all the types non-variable?
    let mut unresolved_type_args = vec![];

    let mut idx = 0;
    for t in resolved_types {
        if t.is_type_variable() {
            unresolved_type_args.push(generic_func.type_args[idx].name.clone());
        }
        idx += 1;
    }

    //if we still have unresolved types
    if unresolved_type_args.len() > 0 {
        //if we're in a generic
        if parser_context.has_type_stack() {
            return TypedExpr{expr: Expr::UnresolvedGenericFuncCall{
                name: func_name.clone(), unresolved_func_decl: resolved_func_decl.clone(), args: args, 
                unresolved_types: resolved_types.clone()
            }, r#type: resolved_func_decl.return_type.clone(), loc: loc.clone()}
        } else {
            //something went wrong
            for unresolved_type_arg in unresolved_type_args {
                parser_context.push_err(Error::UnresolvedTypeArg(loc.clone(), unresolved_type_arg))
            }
        }
    }

    //runtime types are the actual types that we use with our generic function.
    let mut runtime_type_args = vec![];
    let mut mangled_type_names = vec![];
    for resolved_type in resolved_types {
        let o_runtime_type = get_runtime_type_for_generic(&resolved_type);
        match o_runtime_type {
            Some(runtime_type) => {
                mangled_type_names.push(runtime_type.get_mangled_name());
                runtime_type_args.push(runtime_type);
            },
            None => {
                if *resolved_type != Type::Undeclared {
                    parser_context.push_err(Error::NotYetImplemented(loc.clone(), format!("type {} not supported as generic argument", resolved_type)));
                }
                runtime_type_args.push(Type::Undeclared);
                mangled_type_names.push(Type::Undeclared.get_mangled_name());
            }
        }
    }

    let mut runtime_type_map: HashMap<String, Type> = HashMap::new();
    let mut i = 0;
    for type_arg in &generic_func.type_args {
        runtime_type_map.insert(type_arg.name.clone(), runtime_type_args[i].clone());
        i += 1;
    }

    //generate the name we actually call it, see if we have it already
    let runtime_name = format!("{}<{}>", generic_func.func.decl.name, mangled_type_names.join(","));
    let runtime_func_decl = if !parser_context.generic_func_impls.contains(&runtime_name) {
        generate_runtime_generic_func(generic_func, &runtime_name, &runtime_type_map, &loc, parser_context)
    } else {
        resolve_generic_func_decl(generic_func, &runtime_name, &runtime_type_map, &loc, parser_context)
    };

    //now cast the arguments to the runtime types
    let mut runtime_args = vec![];
    let mut idx = 0;
    let runtime_arg_types = runtime_func_decl.get_arg_types();
    for arg in args {
        let cast_expr = generic_wrap(&(runtime_arg_types[idx]), Box::new(arg), parser_context);
        runtime_args.push(cast_expr);
        idx += 1;
    }

    let runtime_call = TypedExpr{expr: Expr::StaticFuncCall(runtime_name.clone(), resolved_func_decl.clone(), runtime_args), 
        r#type: runtime_func_decl.return_type.clone(), loc: loc.clone()};
    generic_unwrap(&resolved_func_decl.return_type, Box::new(runtime_call), parser_context)
}

///Given a set of type arguments that have deduced from a set of function arguments, check them, and generate
/// a function call.
fn generate_generic_func_call_deduced_types(
    this_expr: &Option<TypedExpr>,
    func_name: &String,
    generic_func: &GenericFunc,
    resolved_type_map: &HashMap<String, Type>,
    first_pass_args: &Vec<TypedExpr>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> TypedExpr {
    //Walk over the type args, and generate the final resolved type arg list from the type map.
    //We check type constraints at this point too.
    let mut resolved_types = vec![];
    let mut idx = 0;
    for type_arg in &generic_func.type_args {
        let resolved_type = resolved_type_map.get(&type_arg.name).unwrap();
        resolved_types.push(resolved_type.clone());

        if *resolved_type == Type::Undeclared {
            parser_context.push_err(Error::FailedGenericDeduction(loc.clone(), type_arg.name.clone(), Type::Undeclared));
        }

        if !matches_type_constraint(resolved_type, &(generic_func.type_args[idx].constraint)) {
            parser_context.push_err(Error::FailedTypeArgConstraint(loc.clone()));
        }

        idx += 1;
    }

    //generate a func_decl by substituting type vars
    let resolved_func_decl: FuncDecl = resolve_generic_func_decl(generic_func, &generic_func.func.decl.name, &resolved_type_map, loc, parser_context);

    //type check the first pass args by casting, to get the final args
    let mut args = vec![];
    if this_expr.is_some() {
        args.push(this_expr.as_ref().unwrap().clone());
    }
    idx = 0;
    for first_pass_arg in first_pass_args {
        let cast = cast_typed_expr(&resolved_func_decl.args[idx].r#type, Box::new(first_pass_arg.clone()), CastType::Implicit, parser_context);
        args.push(cast);
        idx += 1;
    }

    //now we can actually generate the generic call
    generate_generic_func_call(func_name, generic_func, args, &resolved_types, &resolved_func_decl, loc, parser_context)
}

struct GenericFuncTypeTransformer<'a>{
    pub type_map: HashMap<String, Type>,
    pub parser_context: &'a mut ParserContext
}

impl<'a> GenericFuncTypeTransformer<'a>{
    pub fn new(type_map: HashMap<String, Type>, parser_context: &mut ParserContext) -> GenericFuncTypeTransformer {
        GenericFuncTypeTransformer{type_map: type_map, parser_context: parser_context}
    }
} 

impl<'a> Transform for GenericFuncTypeTransformer<'a>{
    fn transform_typed_expr(&mut self, typed_expr: &TypedExpr, parser_context: &mut dyn ErrRecorder,) -> Option<TypedExpr> {
        let new_type = transform_type(&typed_expr.r#type.r#type, &self.type_map, &typed_expr.loc, parser_context);
        Some(TypedExpr{expr: transform_expr(&typed_expr.expr, self, &typed_expr.loc, parser_context), loc: typed_expr.loc, r#type: FullType::new(&new_type, typed_expr.r#type.mutability)})
    }
    fn transform_expr(&mut self, expr: &Expr, loc: &SourceLocation, parser_context: &mut dyn ErrRecorder) -> Option<Expr> {
        match expr {
            Expr::SizeOf(t) => {
                let new_type = transform_type(&t, &self.type_map, loc, parser_context);
                Some(Expr::SizeOf(new_type))
            },
            Expr::TypeLiteral(t) => {
                let new_type = transform_type(&t.r#type, &self.type_map, loc, parser_context);
                Some(Expr::TypeLiteral(FullType::new(&new_type, t.mutability)))
            },
            Expr::VariableInit{internal_name, init} => {
                Some(Expr::VariableInit{
                    internal_name: internal_name.clone(),
                    init: Box::new(init.as_ref().as_ref().map(|te| transform_typed_expr(&te, self, parser_context))),
                })
            },
            Expr::UnresolvedGenericFuncCall{name, unresolved_func_decl, args, unresolved_types} => {
                let generic_func = self.parser_context.get_generic_fn_from_generics(name).unwrap();
                let mut resolved_args = vec![];
                for arg in args {
                    resolved_args.push(transform_typed_expr(&arg, self, parser_context));
                }
                let mut resolved_types = vec![];
                for unresolved_type in unresolved_types {
                    resolved_types.push(transform_type(&unresolved_type, &self.type_map, loc, parser_context));
                }
                let resolved_func_decl = transform_func_decl(&unresolved_func_decl, self, loc, parser_context);
                Some(generate_generic_func_call(name, &generic_func, resolved_args, &resolved_types, &resolved_func_decl,
                    loc, self.parser_context
                ).expr)
            },
            _ => None
        }
    }
    fn transform_typed_lvalue_expr(&mut self, typed_lvalue_expr: &TypedLValueExpr, parser_context: &mut dyn ErrRecorder) -> Option<TypedLValueExpr> {
        let new_type = transform_type(&typed_lvalue_expr.r#type, &self.type_map, &typed_lvalue_expr.loc, parser_context);
        Some(TypedLValueExpr{expr: transform_lvalue_expr(&typed_lvalue_expr.expr, self, &typed_lvalue_expr.loc, parser_context), loc: typed_lvalue_expr.loc, r#type: new_type})
    }
    fn transform_lvalue_expr(&mut self, _lvalue_expr: &LValueExpr, _loc: &SourceLocation, _parser_context: &mut dyn ErrRecorder) -> Option<LValueExpr> {
        None
    }
    fn transform_func_decl(&mut self, func_decl: &FuncDecl, loc: &SourceLocation, parser_context: &mut dyn ErrRecorder) -> Option<FuncDecl> {
        let o_type_guard = func_decl.type_guard.as_ref().map(|old_type_guard| {
            let mut branches = vec![];
            for old_branch in &old_type_guard.branches {
                branches.push(TypeGuardBranch{
                    literal: transform_typed_expr(&old_branch.literal, self, parser_context),
                    guard_type: transform_type(&old_branch.guard_type, &self.type_map, loc, parser_context),
                })
            }
            TypeGuard{branches}
        });

        Some(FuncDecl{
            name: func_decl.name.clone(), return_type: FullType::new(&transform_type(&func_decl.return_type.r#type, &self.type_map, loc, parser_context), func_decl.return_type.mutability),
            args: func_decl.args.iter().map(|a| FuncArg{name: a.name.clone(), r#type: FullType::new(&transform_type(&a.r#type.r#type, &self.type_map, loc, parser_context), a.r#type.mutability), mutability: a.mutability}).collect(),
            export: func_decl.export, generic_impl: func_decl.generic_impl, type_guard: o_type_guard, member_func: func_decl.member_func,
        })
    }
}

fn transform_generic_body(
    generic_body: &Option<TypedExpr>,
    type_map: &HashMap<String, Type>,
    parser_context: &mut ParserContext,
) -> Option<TypedExpr> {
    match generic_body {
        None => None,
        Some(typed_expr) => {
            let mut transformer = GenericFuncTypeTransformer{type_map: type_map.clone(), parser_context: parser_context};
            let mut err_recorder = SimpleErrRecorder::new();
            let out = transform_typed_expr(typed_expr, &mut transformer, &mut err_recorder);
            err_recorder.write_to_p_c(parser_context);
            Some(out)
        }
    }
}

fn transform_generic_runtime_func(
    generic_func: &GenericFunc,
    runtime_func_decl: &FuncDecl,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> Func {
    let local_vars = transform_generic_local_vars(&generic_func.func.local_vars, &type_map, loc, parser_context);
    let body = transform_generic_body(&generic_func.func.body, &type_map, parser_context);
    Func{decl: runtime_func_decl.clone(), local_var_map: generic_func.func.local_var_map.clone(), closure: generic_func.func.closure.clone(), local_vars: local_vars, body: body}
}

fn generate_runtime_generic_func(
    generic_func: &GenericFunc,
    runtime_name: &String,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> FuncDecl {
    //1 generate the func decl
    let runtime_func_decl = resolve_generic_func_decl(generic_func, runtime_name, type_map, loc, parser_context);

    //2 generate the func
    let runtime_func = transform_generic_runtime_func(generic_func, &runtime_func_decl, type_map, loc, parser_context);

    //3 write it into the parser context
    parser_context.func_decls.push(runtime_func);
    parser_context.generic_func_impls.insert(runtime_name.clone());

    runtime_func_decl
}

fn deduce_generic_types(
    wanted: &Vec<Type>, 
    got: &Vec<Type>, 
    type_map: &mut HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> () {
    let l = if wanted.len() > got.len() {
        parser_context.push_err(Error::NotEnoughArgs(loc.clone()));
        got.len()
    } else if wanted.len() < got.len() {
        parser_context.push_err(Error::TooManyArgs(loc.clone()));
        wanted.len()    
    } else {
        wanted.len()
    };

    let mut i = 0;
    while i < l {
        deduce_generic_type(&wanted[i], &got[i], type_map, loc, parser_context);
        i += 1;
    }
}

/// Given a type in our generic declaration, and the type of an expression that fits into it, 
/// deduce what type any type variables are and fill them into the type_map.
/// So for example, if we had `fn a<T>(b: Array<T>) -> void {...}` and called `a([1])` 
/// we would expect `wanted` to be Type::Array(Type::VariableUsage('T')), `got` to be `Type::ArrayLiteral(Type::IntLiteral)`.
fn deduce_generic_type(
    wanted: &Type, got: &Type, type_map: &mut HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> () {
    match wanted {
        Type::Any | Type::Bool | Type::FakeVoid | Type::FloatLiteral(_) 
            | Type::Int(_, _) | Type::ModuleLiteral(_) | Type::Never | Type::Number | Type::RealVoid | Type::String 
            | Type::StringLiteral(_) | Type::Undeclared | Type::Unknown | Type::UnsafePtr | Type::UnsafeNull
            | Type::UnsafeStruct{name: _}
                => {},

        Type::Array(w) => match got{
            Type::Array(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
            _ => {},
        }
        
        Type::Func{func_type: w} => match got{
            Type::Func{func_type: g} => {
                deduce_generic_type(&w.out_type.r#type, &g.out_type.r#type, type_map, loc, parser_context);
                deduce_generic_types(&w.in_types.iter().map(|t| t.r#type.clone()).collect(), &g.in_types.iter().map(|t| t.r#type.clone()).collect(), type_map, loc, parser_context);
            },
            _ => {}
        },

        Type::ObjectLiteral(w) => match got {
            Type::ObjectLiteral(g) => {
                for w_ole in w {
                    let g_ole = g.get(w_ole.0);
                    if g_ole.is_some() {
                        deduce_generic_type(w_ole.1, g_ole.unwrap(), type_map, loc, parser_context);
                    } else {
                        parser_context.push_err(Error::ObjectMissingMember(loc.clone(), w_ole.0.clone()))
                    }
                }
            },
            _ => {},
        },

        Type::Option(w) => match got {
            //ew
            Type::Option(g) | Type::Some(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
            _ => {},
        },

        Type::Some(w) => match got {
            Type::Some(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
            _ => {},
        },

        Type::Tuple(w) => match got{
            Type::Tuple(g) => deduce_generic_types(w, g, type_map, loc, parser_context),
            _ => {},
        },

        Type::TypeLiteral(w) => match got{
            Type::TypeLiteral(g) => deduce_generic_type(&w.r#type, &g.r#type, type_map, loc, parser_context),
            _ => {},
        }
        Type::UnsafeArray(w) => match got{
            Type::UnsafeArray(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
            _ => {},
        }
        Type::UnsafeOption(w) => match got {
            //ugh this is horrible
            Type::UnsafeOption(g) | Type::UnsafeSome(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
            _ => {},
        },
        Type::UnsafeSome(w) => match got {
            Type::UnsafeSome(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
            _ => {},
        },

        Type::UserType{name: w_name, type_args: w, inner: _} => match got{
            Type::UserType{name: g_name, type_args: g, inner: _} => {
                if w_name == g_name {
                    deduce_generic_types(w, g, type_map, loc, parser_context)
                }
            },
            _ => {},
        },

        Type::VariableUsage{name, constraint: _} => {
            type_map.insert(name.clone(), got.clone());
        },
    }
}

///Given a set of function arguments, and the corresponding generic function argument types (which may well contain type variables), guess the type arguments.  
fn deduce_generic_function_call_args(
    args: &Vec<TypedExpr>,
    unresolved_arg_types: &Vec<Type>,
    type_map: &mut HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> () {
    let mut idx = 0;
    let l = if unresolved_arg_types.len() > args.len() {
        parser_context.push_err(Error::NotEnoughArgs(loc.clone()));
        args.len()
    } else if unresolved_arg_types.len() < args.len() {
        parser_context.push_err(Error::TooManyArgs(args[unresolved_arg_types.len()].loc.clone()));
        unresolved_arg_types.len()
    } else {
        args.len()
    };

    loop {
        if idx >= l {
            break;
        }
        let expr = &args[idx];
        
        let arg_type = &unresolved_arg_types[idx];
        deduce_generic_type(arg_type, &expr.r#type.r#type, type_map, &expr.loc, parser_context);
        
        idx += 1;
    }
}

/// Helper to create a type map, which is a map from type arg name to type. Starts off all undeclared
fn new_type_map(type_args: &Vec<TypeArg>) -> HashMap<String, Type> {
    let mut resolved_type_map: HashMap<String, Type> = HashMap::new();
    for type_arg in type_args {
        resolved_type_map.insert(type_arg.name.clone(), Type::Undeclared);
    }
    resolved_type_map
}

fn cast_function_call_args(
    args: &Vec<TypedExpr>,
    arg_types: &Vec<FullType>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) ->Vec<TypedExpr> {
    
    let mut out: Vec<TypedExpr> = vec![];
    let arg_types_len = arg_types.len();
    let l = if arg_types_len > args.len() {
        parser_context.push_err(Error::NotEnoughArgs(loc.clone()));
        args.len()
    } else if arg_types_len < args.len() {
        parser_context.push_err(Error::TooManyArgs(args[arg_types.len()].loc.clone()));
        arg_types.len()
    } else {
        args.len()
    };

    let mut idx = 0;
    loop {
        if idx >= l {
            return out;
        }
        let arg_type = &arg_types[idx];
        let cast = cast_typed_expr(arg_type, Box::new(args[idx].clone()), CastType::Implicit, parser_context);
        out.push(cast);
        idx += 1;
    }
}

impl<'a> Parser<'a> {
    /// If we are expecting a no arg function, call this.
    pub(crate) fn parse_empty_function_call_args(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> () {
        let next = self.peek_next_item();
        if !next.token.matches_punct(Punct::OpenParen) {
            parser_context.errors.push(self.expected_token_error_raw(&next, &[&"("]));
            return;
        }
        self.parse_function_call_args(&None, &vec![], parser_func_context, parser_context);
    }

    pub (crate) fn parse_function_call_args_unchecked(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> (Vec<TypedExpr>, SourceLocation) {
        let mut out: Vec<TypedExpr> = vec![];
        let mut loc_out = self.next_position();
        self.skip_next_item();
        loop{
            let lookahead_item = self.peek_next_item();
            let lookahead = lookahead_item.token;
            match lookahead {
                Token::Punct(Punct::CloseParen) => {
                    loc_out.extend_right(&self.next_position());
                    self.skip_next_item();
                    return (out, loc_out);
                },
                _ => {}
            }
        
            let expr = self.parse_expr(parser_func_context, parser_context);
            out.push(expr);
            
            let lookahead_item = self.peek_next_item();
            let lookahead = lookahead_item.token;
         
            match lookahead {
                Token::Punct(Punct::CloseParen) => {},
                Token::Punct(Punct::Comma) => {
                    self.skip_next_item();
                },
                _ => {
                    parser_context.errors.push(Error::UnexpectedToken(lookahead_item.location.clone(), String::from("need expr or comma")));
                    self.skip_next_item();
                }
            }
        }
    }

    pub(crate) fn parse_function_call_args(&mut self,
        this_expr: &Option<TypedExpr>,
        arg_types: &Vec<FullType>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Vec<TypedExpr> {
        let (mut unchecked_args, loc) = self.parse_function_call_args_unchecked(parser_func_context, parser_context);
        if this_expr.is_some() {
            unchecked_args.insert(0, this_expr.as_ref().unwrap().clone());
        }
        cast_function_call_args(&unchecked_args, arg_types, &loc, parser_context)
    }

    fn parse_function_decl_arg(&mut self,
        parser_context: &mut ParserContext,
    ) -> Option<FuncArg> {
        let mut next = expect_next!(self, parser_context, None);
        let token = &next.token;
        let mut mutability = VariableMutability::Constant;
        if token.matches_keyword(Keyword::Var) {
            mutability = VariableMutability::Variable;
            next = expect_next!(self, parser_context, None);
        }

        let id = expect_ident!(next, parser_context, "Expecting variable name to be an identifier");
        let name = id.to_string();
        let next = self.peek_next_item();
        let token = &next.token;
        match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                let arg_type = self.parse_full_type(parser_context);
                
                let next = self.peek_next_item();
                let token = &next.token;
                if token.matches_punct(Punct::Comma) {
                    self.skip_next_item();
                }
                Some(FuncArg{name: name, r#type: arg_type, mutability})
            },
            Token::Punct(Punct::Comma) => {
                self.skip_next_item();
                Some(FuncArg{name: name, r#type: FullType::new(&Type::Undeclared, Mutability::Unknown), mutability})
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(next.location, String::from("expecting ')' or ',' in arg list")));
                self.skip_next_item();
                None
            }
        }
    }

    fn parse_function_decl_args(&mut self,
        this_type: &Option<FullType>,
        parser_context: &mut ParserContext,
    ) -> Vec<FuncArg> {
        expect_punct!(self, parser_context, Punct::OpenParen);
        let mut args: Vec<FuncArg> = if this_type.is_some() { vec![FuncArg{name: String::from("this"), r#type: this_type.clone().unwrap(), mutability: VariableMutability::Constant}]} else { Vec::new() };
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::Punct(Punct::CloseParen) => { self.skip_next_item(); return args; },
                Token::Ident(_) | Token::Keyword(Keyword::Var) => {
                    let arg = self.parse_function_decl_arg(parser_context);
                    if arg.is_some() {
                        args.push(arg.unwrap());
                    }
                },
                _ => {
                    parser_context.push_err(Error::UnexpectedToken(next.location, String::from("expecting ')' in arg list")));
                    self.skip_next_item();
                }
            }
        }
    }

    /// If you don't have a return value at the end of function, you get errors at run time. Also, it seems
    /// polite to make sure the return type is in fact correct.
    fn check_void_return_value(&mut self, 
        body: &TypedExpr,
        body_return_type: &FullType,
        parser_func_context_inner: &ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> () {
        match parser_func_context_inner.given_func_return_type.r#type {
            Type::RealVoid => {},
            Type::Undeclared => {
                match parser_func_context_inner.implied_func_return_type.r#type {
                    Type::RealVoid => {},
                    _ => if body_return_type.r#type == Type::RealVoid {
                        parser_context.push_err(Error::NoValueReturned(body.loc.clone()))
                    }
                }
            },
            _ => if body_return_type.r#type == Type::RealVoid {
                parser_context.push_err(Error::NoValueReturned(body.loc.clone()))
            }
        }
    }

    ///Parse a func body to produce a full Func
    fn parse_func_body_internal(&mut self,
        func_decl: &mut FuncDecl,
        this_type: &Option<FullType>,
        parser_context: &mut ParserContext,
    ) -> Func {
        let mut parser_func_context_inner = ParserFuncContext::new(this_type);
        
        parser_context.push_func_scope();

        self.register_params(this_type, &func_decl.args, &mut parser_func_context_inner, parser_context);

        parser_func_context_inner.given_func_return_type = func_decl.return_type.clone();
        
        let mut decl = func_decl.clone();
        let block = self.parse_block(false, &mut parser_func_context_inner, parser_context);
        //get a guess of the return type of the body based on the last elem
        let body_return_type = get_body_return_type(&block);
        //if we never figured out a return type, then use this. It means we never encountered a return statement.
        if parser_func_context_inner.given_func_return_type.r#type == Type::Undeclared && parser_func_context_inner.implied_func_return_type.r#type == Type::Undeclared {
            parser_func_context_inner.implied_func_return_type = body_return_type.clone();
        }
        //do some special checks on this
        self.check_void_return_value(&block, &body_return_type, &parser_func_context_inner, parser_context);
        //fix up the decl to our implied value
        if decl.return_type.r#type == Type::Undeclared {
            decl.return_type = parser_func_context_inner.implied_func_return_type.clone();
            func_decl.return_type = parser_func_context_inner.implied_func_return_type.clone();
        }
        //and cast. we special case void because we drop unused values.
        let o_block = if decl.return_type.r#type == Type::RealVoid {
            Some(block)
        } else {
            Some(cast_typed_expr(&decl.return_type, Box::new(block), CastType::Implicit, parser_context))
        };

        parser_context.pop_func_scope();

        Func{
            decl: decl,
            local_vars: parser_func_context_inner.local_vars, closure: parser_func_context_inner.closure, 
            local_var_map: parser_func_context_inner.local_var_map, body: o_block,
        }
    }

    fn parse_type_guard(&mut self,
        func_return_type: &FullType,
        parser_context: &mut ParserContext,
    ) -> Option<TypeGuard> {
        self.skip_next_item();

        expect_punct!(self, parser_context, Punct::OpenBrace);

        let mut branches: Vec<TypeGuardBranch> = vec![];

        loop {
            let mut fake_parser_func_context = ParserFuncContext{
                closure: vec![], given_func_return_type: FullType::new(&Type::Undeclared, Mutability::Unknown), 
                implied_func_return_type: FullType::new(&Type::Undeclared, Mutability::Unknown),
                local_vars: vec![],
                local_var_map: HashMap::new(),
                in_iteration: false,
                this_type: None,
            };

            //parse the type literal that defines this branch of the type guard
            let expr = self.parse_expr(&mut fake_parser_func_context, parser_context);
            let mut ok = true;
            if !expr.expr.is_literal() {
                ok = false;
                parser_context.push_err(Error::TypeGuardExpectingLiteral(expr.loc.clone()));    
            }

            //and its type matches the type guard type
            if expr.r#type.r#type != func_return_type.r#type {
                parser_context.push_err(Error::TypeFailure(expr.loc.clone(), func_return_type.clone(), expr.r#type.clone()));
            }

            //now, parse the type associated with this branch
            expect_punct!(self, parser_context, Punct::FatArrow);
            let guard_type = self.parse_type(parser_context);

            if ok {
                branches.push(TypeGuardBranch{literal: expr, guard_type: guard_type});
            }
            
            expect_semicolon!(self, parser_context);
            let next = self.peek_next_item();
            
            if next.token.matches_punct(Punct::CloseBrace) {
                self.skip_next_item();
                break;
            }
        }

        Some(TypeGuard{branches: branches})
    }

    ///The core of the function parsing code.
    fn parse_func_decl_internal(&mut self,
        export: bool,
        phase: ParserPhase,
        prefix: &String,
        this_type: &Option<FullType>,
        this_type_args: &Vec<TypeArg>,
        constructor: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> (Option<TypedExpr>, String, String, Vec<TypeArg>, FuncType) {
        if constructor {
            expect_keyword!(self, parser_context, Keyword::Constructor);
        } else {
            expect_keyword!(self, parser_context, Keyword::Fn);
        }

        let loc = self.peek_next_location();

        //First, parse the name.
        let (mangled_name, name)= if constructor {
            let mut s = prefix.clone();
            s.push_str("constructor");
            (s, String::from("constructor"))
        } else {
            let next = self.peek_next_item();
            let token = next.token; 
            match token {
                Token::Ident(i) => {
                    self.skip_next_item();
                    let mut s = prefix.clone();
                    s.push_str(i.as_str());
                    (s, String::from(i.as_str()))
                }, 
                _ => {
                    let unique = parser_context.get_unique_name(&String::from("fn"));
                    (unique.clone(), unique)
                }
            }
        };
        
        //Generic? Constructors can't be explicit generics. But even if it doesn't have
        //angle brackets, it still could be if the 'this' arg is a generic type
        let mut generic = false;
        let mut type_scope_pushed = false;
        let num_this_type_args = this_type_args.len();
        let type_args = if constructor {
            if num_this_type_args > 0 {
                generic = true;
            }
            this_type_args.clone()
        } else {
            let next = self.peek_next_item();
            let token = &next.token;
            if token.matches_punct(Punct::LessThan) {
                let type_args = self.parse_type_decl_args(parser_context);
                let mut type_args = parser_context.push_type_scope(&type_args);
                type_scope_pushed = true;
                generic = true;
                let mut o = this_type_args.clone();
                o.append(& mut type_args);
                o
            } else {
                if num_this_type_args > 0 {
                    generic = true;
                }
                this_type_args.clone()
            }
        };

        //now get the args
        let arg_list = self.parse_function_decl_args(this_type, parser_context);

        //and the return type
        let next = self.peek_next_item();
        let token = &next.token;
        let (return_type, type_guard) = if token.matches_punct(Punct::ThinArrow) {
            self.skip_next_item();
            let t = self.parse_full_type(parser_context);

            let next = self.peek_next_item();
            let token = &next.token;
            let type_guard = if token.matches_keyword(Keyword::UnsafeTypeGuard) {
                self.parse_type_guard(&t, parser_context)
            } else {
                None
            };

            (t, type_guard)
        } else if token.matches_punct(Punct::FatArrow) {
            if export {
                parser_context.push_err(Error::ExportedFunctionFatArrow(next.location.clone()));
            }
            self.skip_next_item();
            (FullType::new(&Type::Undeclared, Mutability::Unknown), None)
        } else {
            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("Expecting '->' or '=>'")));
            self.skip_next_item();
            (FullType::new(&Type::Undeclared, Mutability::Unknown), None)
        };

        //here's the decl
        let mut func_decl = FuncDecl{
            name: mangled_name.to_string(), return_type: return_type.clone(), args: arg_list, export, generic_impl: generic, type_guard: type_guard, member_func: this_type.is_some(),
        };

        //now parse the body if we need to
        let func = if generic || phase == ParserPhase::MainPhase{
            self.parse_func_body_internal(&mut func_decl, this_type, parser_context)
        } else {
            Func{
                decl: func_decl,
                local_vars: vec![], closure: vec![], 
                local_var_map: HashMap::new(), body: None,
            }
        };

        //deal with the results
        if generic {
            if type_scope_pushed {
                parser_context.pop_type_scope();
            }
            if func.closure.len() > 0 {
                parser_context.push_err(Error::NoClosureInGenerics(loc))
            }
            let func_type = func.decl.get_func_type();
            parser_context.generic_func_decls.push(GenericFunc{func: func, type_args: type_args.clone(), num_this_type_args: num_this_type_args.try_into().unwrap()});
            (None, mangled_name, name, type_args, func_type)
        } else {
            let func_closure = func.closure.clone();
            // now we have a closure of an inner function, we need to capture it. So look at every element
            for cr in &func_closure {
                // is it in our local vars?
                let o_local_var = parser_func_context_outer.local_vars.iter_mut().find(|lv| lv.internal_name == cr.internal_name);
                if o_local_var.is_some() {
                    // yes it is, so mark that local variable as a closure source
                    o_local_var.unwrap().closure_source = true;
                } else {
                    //not a local variable, so it's in the closure of this function, so make sure it's captured
                    parser_func_context_outer.closure.push(cr.clone())
                }
            }
            let func_type = func.decl.get_func_type();
            parser_context.func_decls.push(func); 
            (Some(TypedExpr{
                expr: Expr::FuncDecl(FuncObjectCreation{name: mangled_name.clone(), closure: func_closure}), r#type: FullType::new_const(&Type::Func{func_type: Box::new(func_type.clone())}), loc: loc}), 
                mangled_name, name, vec![], func_type
            )
        }
    }

    pub(crate) fn parse_function_decl_main_phase(&mut self, 
        export: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<TypedExpr> {
        self.parse_func_decl_internal(export, ParserPhase::MainPhase, &String::from(""), &None, &vec![], false, parser_func_context_outer, parser_context).0
    }
    
    pub(crate) fn parse_named_function_decl_export_phase(&mut self, 
        export: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> () {
        self.parse_func_decl_internal(export, ParserPhase::ExportsPhase, &String::from(""), &None, &vec![], false, parser_func_context_outer, parser_context);
    }

    pub (crate) fn parse_constructor_decl(&mut self, 
        prefix: &String,
        this_type_args: &Vec<TypeArg>,
        export: bool,
        phase: ParserPhase,
        parser_context: &mut ParserContext,
    ) -> MemberFunc {
        let mut fake_parser_func_context = ParserFuncContext::new(&None);
        let (_, mangled_name, name, type_args, func_type) = self.parse_func_decl_internal(export, phase, prefix, &None, this_type_args, true, &mut fake_parser_func_context, parser_context);
        MemberFunc{mangled_name: mangled_name, type_args, func_type, name: name, privacy: Privacy::Public}
    }

    pub(crate) fn parse_member_function_decl(&mut self, 
        prefix: &String,
        this_type: &FullType,
        this_type_args: &Vec<TypeArg>,
        privacy: Privacy,
        export: bool,
        phase: ParserPhase,
        parser_context: &mut ParserContext,
    ) -> MemberFunc {
        let mut fake_parser_func_context = ParserFuncContext::new(&None);
        let (_, mangled_name, name, type_args, func_type) = self.parse_func_decl_internal(export, phase, prefix, &Some(this_type.clone()), this_type_args, false, &mut fake_parser_func_context, parser_context);
        MemberFunc{mangled_name, type_args, func_type, name, privacy}
    }

    fn parse_generic_func_call_given_resolved_types(&mut self,
        this_expr: &Option<TypedExpr>,
        func_name: &String,
        generic_func: &GenericFunc,
        resolved_types: &Vec<Type>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        //generate the map from type arg to concrete type
        let mut resolved_type_map: HashMap<String, Type> = HashMap::new();
        let mut i = 0;
        for type_arg in &generic_func.type_args {
            resolved_type_map.insert(type_arg.name.clone(), resolved_types[i].clone());
            i += 1;
        }

        //resolve the func decl
        let loc = self.peek_next_location();
        let resolved_func_decl: FuncDecl = resolve_generic_func_decl(generic_func, &generic_func.func.decl.name, &resolved_type_map, &loc, parser_context);

        //now parse the args
        let mut args = self.parse_function_call_args(this_expr, &resolved_func_decl.get_arg_types(), parser_func_context, parser_context);
        if this_expr.is_some() {
            args.insert(0, this_expr.as_ref().unwrap().clone());
        }         
        generate_generic_func_call(func_name, generic_func, args, &resolved_types, &resolved_func_decl, &loc, parser_context)
    }

    fn parse_generic_func_call_implicit_given_partial_type_map(&mut self,
        this_expr: &Option<TypedExpr>,
        func_name: &String,
        type_map: &mut HashMap<String, Type>,
        generic_func: &GenericFunc,
        name_loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        //generate a first pass args. This means accepting the arg types as they are
        let (first_pass_args, unchecked_args_loc) = self.parse_function_call_args_unchecked(parser_func_context, parser_context);

        //now run the deduction
        deduce_generic_function_call_args(&first_pass_args, &generic_func.func.decl.get_arg_types().iter().map(|t| t.r#type.clone()).collect(), type_map, &unchecked_args_loc, parser_context);

        let mut loc = name_loc.clone();
        loc.extend_right(&unchecked_args_loc);

        generate_generic_func_call_deduced_types(this_expr, func_name, generic_func, &type_map, &first_pass_args, &loc, parser_context)
    }

    ///Generic function call where because we know the this type, we know some of the type args. May be explicit or implicit.
    fn parse_generic_func_call_given_partial_types(&mut self,
        this_expr: &Option<TypedExpr>,
        func_name: &String,
        types: &Vec<Type>,
        generic_func: &GenericFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        if types.len() == generic_func.type_args.len() {
            self.parse_generic_func_call_given_resolved_types(this_expr, func_name, generic_func, &types, parser_func_context, parser_context)
        } else {
            let next = self.peek_next_item();
            let token = &next.token;
            //peek to see if we are explicitly given the rest
            if token.matches_punct(Punct::LessThan) {
                let split = types.len();
                let remaining_type_args = generic_func.type_args[split..].to_vec();
                let mut resolved_types = self.parse_type_args(&remaining_type_args, parser_context);
                let mut final_types = types.clone();
                final_types.append(&mut resolved_types);
        
                self.parse_generic_func_call_given_resolved_types(this_expr, func_name, generic_func, &final_types, parser_func_context, parser_context)

            } else {
                let mut resolved_type_map = new_type_map(&generic_func.type_args);
                let mut i = 0;
                loop {
                    if i == types.len() || i == generic_func.type_args.len() {
                        break;
                    }
                    resolved_type_map.insert(generic_func.type_args[i].name.clone(), types[i].clone());
                    i += 1;
                }

                let loc = self.peek_next_location();
                self.parse_generic_func_call_implicit_given_partial_type_map(this_expr, func_name, &mut resolved_type_map, generic_func, &loc, parser_func_context, parser_context)
            }
        }
    }

    fn parse_generic_func_call_explicit_types(&mut self,
        func_name: &String,
        generic_func: &GenericFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        //parse the args
        let resolved_types = self.parse_type_args(&generic_func.type_args, parser_context);
        
        self.parse_generic_func_call_given_resolved_types(&None, func_name, generic_func, &resolved_types, parser_func_context, parser_context)
    }

    //Parse the function call of a generic function, where we don't know the type arguments.
    fn parse_generic_func_call_implicit_types(&mut self,
        func_name: &String,
        generic_func: &GenericFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();

        //generate an empty type map
        let mut resolved_type_map = new_type_map(&generic_func.type_args);

        //and pass to the partial type map call
        self.parse_generic_func_call_implicit_given_partial_type_map(&None, func_name, &mut resolved_type_map, generic_func, &loc, parser_func_context, parser_context)
    }

    fn parse_generic_func_call(
        &mut self,
        func_name: &String,
        generic_func: &GenericFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_punct(Punct::LessThan) {
            self.parse_generic_func_call_explicit_types(func_name, generic_func, parser_func_context, parser_context) 
        } else {
            self.parse_generic_func_call_implicit_types(func_name, generic_func, parser_func_context, parser_context) 
        }
    }

    pub(crate) fn try_parse_generic_func_call(
        &mut self,
        func_name: &String,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<TypedExpr> {
        //is it actually a generic func?
        let o_g_f = parser_context.get_generic_fn_from_generics(func_name);
        match o_g_f {
            Some(g_f) => Some(self.parse_generic_func_call(func_name, &g_f, parser_func_context, parser_context)),
            None => return None
        }
    }

    pub (crate) fn parse_specific_func_call(&mut self,
        this_expr: &Option<TypedExpr>,
        func_name: &String,
        func_decl: &FuncDecl,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let arg_types = func_decl.get_arg_types();
        let mut args = self.parse_function_call_args(this_expr, &arg_types, parser_func_context, parser_context);
        let func_return_type = func_decl.return_type.clone();
        let loc = SourceLocation::new(loc.start.clone(), args.last().map(|a| a.loc.end.clone()).unwrap_or(loc.end.clone()));
        if this_expr.is_some() {
            args.insert(0, this_expr.as_ref().unwrap().clone());
        }
        TypedExpr{expr: Expr::StaticFuncCall(func_name.clone(), func_decl.clone(), args), r#type: func_return_type, loc: loc}
    }

    pub (crate) fn try_parse_func_call(&mut self,
        module_name: &Option<String>,
        func_name: &String,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<TypedExpr> {
        match module_name{
            None => {
                let o_f_d = parser_context.get_fn_decl_from_decls(&func_name);
                match &o_f_d {
                    Some(f_d) => {
                        Some(self.parse_specific_func_call(&None, &func_name, &f_d, &loc, parser_func_context, parser_context))
                    },
                    None => {
                        self.try_parse_generic_func_call(func_name, parser_func_context, parser_context)
                    }
                }
            },
            Some(m) => {
                let full_name = format!("{}.{}", m, func_name);
                let o_f_d = parser_context.get_fn_decl_from_imports(&full_name);
                match &o_f_d {
                    Some(f_d) => {
                        Some(self.parse_specific_func_call(&None, &full_name, &f_d, &loc, parser_func_context, parser_context))
                    },
                    None => {
                        self.try_parse_generic_func_call(&full_name, parser_func_context, parser_context)
                    }
                }
            }
        }
    }

    pub (crate) fn parse_constructor_call(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> (TypedExpr, FullType) {
        let type_to_construct = self.parse_full_type(parser_context);
        let loc = self.peek_next_location();
        match &type_to_construct.r#type {
            Type::UserType{name, type_args, inner: _} => {
                let prefix = format!("!{}__", name);
                let mut func_name = prefix.clone();
                func_name.push_str("constructor");
                let o_f_d = parser_context.get_fn_decl_from_decls(&func_name);
                match &o_f_d {
                    Some(f_d) => {
                        (self.parse_specific_func_call(&None, &func_name, &f_d, &loc, parser_func_context, parser_context), type_to_construct.clone())
                    },
                    None => {
                        let o_g_f = parser_context.get_generic_fn_from_generics(&func_name);
                        match o_g_f {
                            Some(g_f) => {
                                (self.parse_generic_func_call_given_resolved_types(&None, &func_name, &g_f, &type_args, parser_func_context, parser_context), type_to_construct)
                            },
                            None => {
                                parser_context.push_err(Error::FuncNotRecognized(loc.clone(), func_name.clone()));
                                (TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::Undeclared), loc: loc}, type_to_construct)
                            }
                        }
                    }
                }
            },
            _ => {
                parser_context.push_err(Error::CantConstruct(loc.clone(), type_to_construct.r#type.clone()));
                (TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::Undeclared), loc: loc}, type_to_construct)
            }
        }
    }

    pub (crate) fn parse_member_function_call(&mut self,
        this_expr: &TypedExpr,
        type_args: &Vec<Type>,
        member_func: &MemberFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let mangled_name = &member_func.mangled_name;
        let loc = self.peek_next_location();

        let o_f_d = parser_context.get_fn_decl_from_decls(&mangled_name);
        match &o_f_d {
            Some(f_d) => {
                self.parse_specific_func_call(&Some(this_expr.clone()), &mangled_name, &f_d, &loc, parser_func_context, parser_context)
            },
            None => {
                let o_g_f = parser_context.get_generic_fn_from_generics(&mangled_name);
                match o_g_f {
                    Some(g_f) => {
                        self.parse_generic_func_call_given_partial_types(&Some(this_expr.clone()), &mangled_name, type_args, &g_f, parser_func_context, parser_context)
                    },
                    None => {
                        parser_context.push_err(Error::FuncNotRecognized(loc.clone(), member_func.name.clone()));
                        TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::Undeclared), loc: loc}
                    }
                }
            }
        }
    }
}