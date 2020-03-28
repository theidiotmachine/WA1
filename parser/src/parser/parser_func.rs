use std::collections::{HashMap};

use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
pub use errs::prelude::*;

use crate::tree_transform::{Transform, transform_expr, transform_lvalue_expr, transform_typed_expr, transform_func_decl};

use crate::{expect_keyword, expect_next, expect_ident, expect_punct, cast_typed_expr, expect_semicolon};

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
        let new_type = transform_type(&orig_arg.r#type, type_map, loc, parser_context);
        out_args.push(FuncArg{name: orig_arg.name.clone(), r#type: new_type});
    }

    let out_return_type = transform_type(&generic_func.func.decl.return_type, type_map, loc, parser_context);

    let mut transformer = GenericFuncTypeTransformer::new(type_map.clone(), parser_context);
    let mut err_recorder = SimpleErrRecorder::new();

    let type_guard = if generic_func.func.decl.type_guard.is_some() {
        let t_g = generic_func.func.decl.type_guard.clone().unwrap();
        let mut t_g_b_s = vec![];
        for t_g_b in t_g.branches {
            t_g_b_s.push(TypeGuardBranch{
                literal: transform_typed_expr(&t_g_b.literal, &mut transformer, &mut err_recorder),
                cast_fn_id: t_g_b.cast_fn_id.clone(),
            })
        }
        err_recorder.write_to_p_c(parser_context);
        Some(TypeGuard{branches: t_g_b_s})
    } else {
        None
    };

    FuncDecl{args: out_args, export: generic_func.func.decl.export, name: resolved_name.clone(), return_type: out_return_type, generic_impl: true, type_guard: type_guard}
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

/// Substitute all type variables for their concrete values
fn transform_type(t: &Type,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> Type {
    match t {
        Type::Any | Type::BigInt | Type::BigIntLiteral(_) | Type::Boolean | Type::FakeVoid | Type::FloatLiteral(_) 
        | Type::Int | Type::IntLiteral(_) | Type::ModuleLiteral(_) | Type::Never | Type::Number | Type::RealVoid | Type::String 
        | Type::StringLiteral(_) | Type::Undeclared | Type::Unknown | Type::UnsafePtr | Type::UnsafeNull | Type::UnsafeSizeT
        | Type::UnsafeStruct{name: _} | Type::UserClass{name: _}
            => t.clone(),
        Type::Array(t) => Type::Array(Box::new(transform_type(t, type_map, loc, parser_context))),
        Type::Func{func_type} => {
            Type::Func{func_type: Box::new(FuncType{
                out_type: transform_type(&func_type.out_type, type_map, loc, parser_context),
                in_types: transform_types(&func_type.in_types, type_map, loc, parser_context)
            })}
        },
        Type::ObjectLiteral(oles) => {
            Type::ObjectLiteral(oles.iter().map(|(k, v)| (k.clone(), transform_type(&v, type_map, loc, parser_context))).collect())
        },
        Type::Option(t) => Type::Option(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::Some(t) => Type::Some(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::Tuple(ts) => Type::Tuple(transform_types(&ts, type_map, loc, parser_context)),
        Type::TypeLiteral(t) => Type::TypeLiteral(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::UnsafeArray(t) => Type::UnsafeArray(Box::new(transform_type(t, type_map, loc, parser_context))),
        Type::UnsafeOption(t) => Type::UnsafeOption(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::UnsafeSome(t) => Type::UnsafeSome(Box::new(transform_type(&t, type_map, loc, parser_context))),
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
        let new_type = transform_type(&generic_local_var.r#type, type_map, loc, parser_context);
        out.push(LocalVar{
            internal_name: generic_local_var.internal_name.clone(),
            r#type: new_type,
            closure_source: generic_local_var.closure_source,
            arg: generic_local_var.arg
        })
    }
    out
}

/// Because the return type of 'return' is 'never' we need to do some weird checks
fn get_body_return_type(
    body: &TypedExpr,
) -> Type {
    match &body.expr {
        Expr::Block(exprs) => {
            let o_last = exprs.last();
            match o_last {
                Some(last) => get_body_return_type(&last),
                _ => Type::RealVoid
            }
        },
        Expr::Return(o_e) => {
            if o_e.is_none() {
                Type::RealVoid
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
            }, r#type: resolved_func_decl.return_type.clone(), is_const: true, loc: loc.clone()}
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
                parser_context.push_err(Error::NotYetImplemented(loc.clone(), format!("type {} not supported as generic argument", resolved_type)));
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
        let cast_expr = cast_typed_expr(&(runtime_arg_types[idx]), Box::new(arg), CastType::GenericForce, parser_context);
        runtime_args.push(cast_expr);
        idx += 1;
    }

    let runtime_call = TypedExpr{expr: Expr::StaticFuncCall(runtime_name.clone(), resolved_func_decl.clone(), runtime_args), 
        r#type: runtime_func_decl.return_type.clone(), is_const: true, loc: loc.clone()};
    cast_typed_expr(&resolved_func_decl.return_type, Box::new(runtime_call), CastType::GenericForce, parser_context)
}

///Given a set of type arguments that have deduced from a set of function arguments, check them, and generate
/// a function call.
fn generate_generic_func_call_deduced_types(
    func_name: &String,
    generic_func: &GenericFunc,
    resolved_type_map: &HashMap<String, Type>,
    first_pass_args: &Vec<TypedExpr>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> TypedExpr {
    //generate the resolved types
    let mut resolved_types = vec![];
    let mut idx = 0;
    for type_arg in &generic_func.type_args {
        let resolved_type = resolved_type_map.get(&type_arg.name).unwrap();
        let loc = first_pass_args[idx].loc;
        resolved_types.push(resolved_type.clone());

        if *resolved_type == Type::Undeclared {
            parser_context.push_err(Error::FailedGenericDeduction(loc, type_arg.name.clone()));
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
    idx = 0;
    for first_pass_arg in first_pass_args {
        let cast = cast_typed_expr(&resolved_func_decl.args[idx].r#type, Box::new(first_pass_arg.clone()), CastType::Implicit, parser_context);
        args.push(cast);
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
        let new_type = transform_type(&typed_expr.r#type, &self.type_map, &typed_expr.loc, parser_context);
        Some(TypedExpr{expr: transform_expr(&typed_expr.expr, self, &typed_expr.loc, parser_context), is_const: typed_expr.is_const, loc: typed_expr.loc, r#type: new_type})
    }
    fn transform_expr(&mut self, expr: &Expr, loc: &SourceLocation, parser_context: &mut dyn ErrRecorder) -> Option<Expr> {
        match expr {
            Expr::SizeOf(t) => {
                let new_type = transform_type(&t, &self.type_map, loc, parser_context);
                Some(Expr::SizeOf(new_type))
            },
            Expr::TypeLiteral(t) => {
                let new_type = transform_type(&t, &self.type_map, loc, parser_context);
                Some(Expr::TypeLiteral(new_type))
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
                    cast_fn_id: old_branch.cast_fn_id.clone(),
                })
            }
            TypeGuard{branches}
        });

        Some(FuncDecl{
            name: func_decl.name.clone(), return_type: transform_type(&func_decl.return_type, &self.type_map, loc, parser_context),
            args: func_decl.args.iter().map(|a| FuncArg{name: a.name.clone(), r#type: transform_type(&a.r#type, &self.type_map, loc, parser_context)}).collect(),
            export: func_decl.export, generic_impl: func_decl.generic_impl, type_guard: o_type_guard
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
/// we would expect wanted to be Type::Array(Type::VariableUsage('T')), got to be `Type::ArrayLiteral(Type::IntLiteral)`.
fn deduce_generic_type(
    wanted: &Type, got: &Type, type_map: &mut HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> () {
    match wanted {
        Type::Any | Type::BigInt | Type::BigIntLiteral(_) | Type::Boolean | Type::FakeVoid | Type::FloatLiteral(_) 
            | Type::Int | Type::IntLiteral(_) | Type::ModuleLiteral(_) | Type::Never | Type::Number | Type::RealVoid | Type::String 
            | Type::StringLiteral(_) | Type::Undeclared | Type::Unknown | Type::UnsafePtr | Type::UnsafeNull | Type::UnsafeSizeT
            | Type::UnsafeStruct{name: _} | Type::UserClass{name: _}
                => {},

        Type::Array(w) => match got{
            Type::Array(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
            _ => {},
        }
        
        Type::Func{func_type: w} => match got{
            Type::Func{func_type: g} => {
                deduce_generic_type(&w.out_type, &g.out_type, type_map, loc, parser_context);
                deduce_generic_types(&w.in_types, &g.in_types, type_map, loc, parser_context);
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
            Type::TypeLiteral(g) => deduce_generic_type(w, g, type_map, loc, parser_context),
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
        parser_context.errors.push(Error::NotEnoughArgs(loc.clone()));
        args.len()
    } else if unresolved_arg_types.len() < args.len() {
        parser_context.errors.push(Error::TooManyArgs(args[unresolved_arg_types.len()].loc.clone()));
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
        deduce_generic_type(arg_type, &expr.r#type, type_map, &expr.loc, parser_context);
        
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
    arg_types: &Vec<Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) ->Vec<TypedExpr> {
    let mut idx = 0;
    let mut out: Vec<TypedExpr> = vec![];
    let l = if arg_types.len() > args.len() {
        parser_context.errors.push(Error::NotEnoughArgs(loc.clone()));
        args.len()
    } else if arg_types.len() < args.len() {
        parser_context.errors.push(Error::TooManyArgs(args[arg_types.len()].loc.clone()));
        arg_types.len()
    } else {
        args.len()
    };

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

fn generate_static_func_call(
    full_name: &String,
    func_decl: &FuncDecl,
    unchecked_args: &Vec<TypedExpr>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> TypedExpr {
    let arg_types = func_decl.get_arg_types();
    let args = cast_function_call_args(unchecked_args, &arg_types, loc, parser_context);
    let func_return_type = func_decl.return_type.clone();
    TypedExpr{expr: Expr::StaticFuncCall(full_name.clone(), func_decl.clone(), args), r#type: func_return_type, is_const: true, loc: loc.clone()}
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
        self.parse_function_call_args(&vec![], parser_func_context, parser_context);
    }

    fn parse_function_call_args_unchecked(&mut self,
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
        arg_types: &Vec<Type>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Vec<TypedExpr> {
        let (unchecked_args, loc) = self.parse_function_call_args_unchecked(parser_func_context, parser_context);
        cast_function_call_args(&unchecked_args, arg_types, &loc, parser_context)
    }

    fn parse_function_decl_arg(&mut self,
        parser_context: &mut ParserContext,
    ) -> Option<FuncArg> {
        let next = expect_next!(self, parser_context, None);
        let id = expect_ident!(next, parser_context, "Expecting variable name to be an identifier");
        let name = id.to_string();
        let next = self.peek_next_item();
        let token = &next.token;
        match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                let arg_type = self.parse_type(parser_context);
                
                let next = self.peek_next_item();
                let token = &next.token;
                if token.matches_punct(Punct::Comma) {
                    self.skip_next_item();
                }
                Some(FuncArg{name: name, r#type: arg_type})
            },
            Token::Punct(Punct::Comma) => {
                self.skip_next_item();
                Some(FuncArg{name: name, r#type: Type::Undeclared})
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(next.location, String::from("expecting ')' or ',' in arg list")));
                self.skip_next_item();
                None
            }
        }
    }

    fn parse_function_decl_args(&mut self,
        parser_context: &mut ParserContext,
    ) -> Vec<FuncArg> {
        expect_punct!(self, parser_context, Punct::OpenParen);
        let mut args: Vec<FuncArg> = Vec::new();
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::Punct(Punct::CloseParen) => { self.skip_next_item(); return args; },
                Token::Ident(_) => {
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
        body_return_type: &Type,
        parser_func_context_inner: &ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> () {
        match parser_func_context_inner.given_func_return_type {
            Type::RealVoid => {},
            Type::Undeclared => {
                match parser_func_context_inner.implied_func_return_type {
                    Type::RealVoid => {},
                    _ => if *body_return_type == Type::RealVoid {
                        parser_context.push_err(Error::NoValueReturned(body.loc.clone()))
                    }
                }
            },
            _ => if *body_return_type == Type::RealVoid {
                parser_context.push_err(Error::NoValueReturned(body.loc.clone()))
            }
        }
    }

    ///Parse a func body to produce a full Func
    fn parse_func_body_internal(&mut self,
        func_decl: &FuncDecl,
        parser_context: &mut ParserContext,
    ) -> Func {
        let mut parser_func_context_inner = ParserFuncContext::new();
        
        parser_context.push_func_scope();

        self.register_params(&func_decl.args, &mut parser_func_context_inner, parser_context);

        parser_func_context_inner.given_func_return_type = func_decl.return_type.clone();
        
        let mut decl = func_decl.clone();
        let block = self.parse_block(false, &mut parser_func_context_inner, parser_context);
        //get a guess of the return type of the body based on the last elem
        let body_return_type = get_body_return_type(&block);
        //if we never figured out a return type, then use this. It means we never encountered a return statement.
        if parser_func_context_inner.given_func_return_type == Type::Undeclared && parser_func_context_inner.implied_func_return_type == Type::Undeclared {
            parser_func_context_inner.implied_func_return_type = body_return_type.clone();
        }
        //do some special checks on this
        self.check_void_return_value(&block, &body_return_type, &parser_func_context_inner, parser_context);
        //fix up the decl to our implied value
        if decl.return_type == Type::Undeclared {
            decl.return_type = parser_func_context_inner.implied_func_return_type.clone()
        }
        //and cast. we special case void because we drop unused values.
        let o_block = if decl.return_type == Type::RealVoid {
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
        arg_list: &Vec<FuncArg>, 
        func_return_type: &Type,
        parser_context: &mut ParserContext,
    ) -> Option<TypeGuard> {
        self.skip_next_item();

        expect_punct!(self, parser_context, Punct::OpenBrace);

        let mut branches: Vec<TypeGuardBranch> = vec![];

        loop {
            let mut fake_parser_func_context = ParserFuncContext{
                closure: vec![], given_func_return_type: Type::Undeclared, implied_func_return_type: Type::Undeclared,
                local_vars: vec![],
                local_var_map: HashMap::new(),
                in_iteration: false,
            };

            //parse the type literal that defines this branch of the type guard
            let expr = self.parse_expr(&mut fake_parser_func_context, parser_context);
            let mut ok = true;
            if !expr.expr.is_literal() {
                ok = false;
                parser_context.push_err(Error::TypeGuardExpectingLiteral(expr.loc.clone()));    
            }

            //and its type matches the type guard type
            if expr.r#type != *func_return_type {
                parser_context.push_err(Error::TypeFailure(expr.loc.clone(), func_return_type.clone(), expr.r#type.clone()));
            }

            //now, parse the func associated with this branch
            expect_punct!(self, parser_context, Punct::FatArrow);
            let mut fake_parser_func_context = ParserFuncContext{
                closure: vec![], given_func_return_type: Type::Undeclared, implied_func_return_type: Type::Undeclared,
                local_vars: vec![],
                local_var_map: HashMap::new(),
                in_iteration: false,
            };
            let (_, cast_fn_id) = self.main_parse_function_decl(false, &mut fake_parser_func_context, parser_context);

            if ok {
                branches.push(TypeGuardBranch{literal: expr, cast_fn_id: cast_fn_id});
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


    fn parse_func_decl_internal(&mut self,
        export: bool,
        main_parse: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> (Option<TypedExpr>, String) {
        expect_keyword!(self, parser_context, Keyword::Fn);

        let loc = self.peek_next_location();

        //First, parse the header
        let next = self.peek_next_item();
        let token = next.token; 
        let id = match token {
            Token::Ident(i) => {
                self.skip_next_item();
                i.to_string()
            }, 
            _ => {
                parser_context.get_unique_name(&String::from("fn"))
            }
        };

        let next = self.peek_next_item();
        let token = &next.token;

        //generic?
        let mut generic = false;
        let type_args = if token.matches_punct(Punct::LessThan) {
            let type_args = self.parse_type_decl_args(parser_context);
            let type_args = parser_context.push_type_scope(&type_args);
            generic = true;
            type_args
        } else {
            vec![]
        };

        //now get the args
        let arg_list = self.parse_function_decl_args(parser_context);

        //and the return type
        let next = self.peek_next_item();
        let token = &next.token;
        let (return_type, type_guard) = if token.matches_punct(Punct::ThinArrow) {
            self.skip_next_item();
            let t = self.parse_type(parser_context);

            let next = self.peek_next_item();
            let token = &next.token;
            let type_guard = if token.matches_keyword(Keyword::UnsafeTypeGuard) {
                if generic {
                    parser_context.push_err(Error::NotYetImplemented(next.location.clone(), String::from("Generic type guards")));
                }
                self.parse_type_guard(&arg_list, &t, parser_context)
            } else {
                None
            };

            (t, type_guard)
        } else if token.matches_punct(Punct::FatArrow) {
            self.skip_next_item();
            (Type::Undeclared, None)
        } else {
            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("Expecting '->' or '=>'")));
            (Type::Undeclared, None)
        };

        //here's the decl
        let func_decl = FuncDecl{
            name: id.to_string(), return_type: return_type.clone(), args: arg_list, export, generic_impl: generic, type_guard: type_guard
        };

        //now parse the body if we need to
        let func = if generic || main_parse{
            self.parse_func_body_internal(&func_decl, parser_context)
        } else {
            Func{
                decl: func_decl,
                local_vars: vec![], closure: vec![], 
                local_var_map: HashMap::new(), body: None,
            }
        };

        //deal with the results
        if generic {
            parser_context.pop_type_scope();
            if func.closure.len() > 0 {
                parser_context.push_err(Error::NoClosureInGenerics(loc))
            }
            parser_context.generic_func_decls.push(GenericFunc{func: func, type_args: type_args});
            (None, id)
        } else {
            let name = func.decl.name.clone();
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
            (Some(TypedExpr{expr: Expr::FuncDecl(FuncObjectCreation{name: name, closure: func_closure}), is_const: true, r#type: Type::Func{func_type: Box::new(func_type)}, loc: loc}), id)
        }
    }

    pub(crate) fn main_parse_function_decl(&mut self, 
        export: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> (Option<TypedExpr>, String) {
        self.parse_func_decl_internal(export, true, parser_func_context_outer, parser_context)
    }
    
    pub(crate) fn export_parse_named_function_decl(&mut self, 
        export: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> () {
        self.parse_func_decl_internal(export, false, parser_func_context_outer, parser_context);
    }

    fn parse_generic_func_call_explicit_types(
        &mut self,
        func_name: &String,
        generic_func: &GenericFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        //skip '<'
        self.skip_next_item();
        let mut loc = self.peek_next_location();
        let mut idx = 0;
        let mut resolved_types = vec![];
        let num = generic_func.type_args.len();

        //parse the explicit types
        while idx < num {
            let arg_type = self.parse_type(parser_context);
            if !matches_type_constraint(&arg_type, &(generic_func.type_args[idx].constraint)) {
                parser_context.push_err(Error::FailedTypeArgConstraint(loc.clone()));
            }
            resolved_types.push(arg_type);
            let next = self.peek_next_item();
            let token = &next.token;
            if token.matches_punct(Punct::Comma) {
                self.skip_next_item();
                loc = self.peek_next_location();
                idx += 1;
            } else if token.matches_punct(Punct::GreaterThan) {
                self.skip_next_item();
                if idx == num - 1 {
                    idx += 1;
                } else {
                    parser_context.push_err(Error::MissingTypeArgs(next.location.clone()));
                    break;
                }
            }
        }

        while idx < num {
            resolved_types.push(Type::Undeclared);
            idx += 1;
        }

        //here 'runtime' means the type the generic will compile to; 'resolved' means the type you would notionally expect
        let mut resolved_type_map: HashMap<String, Type> = HashMap::new();
        let mut i = 0;
        for type_arg in &generic_func.type_args {
            resolved_type_map.insert(type_arg.name.clone(), resolved_types[i].clone());
            i += 1;
        }

        //resolve the func decl
        let resolved_func_decl: FuncDecl = resolve_generic_func_decl(generic_func, &generic_func.func.decl.name, &resolved_type_map, &loc, parser_context);

        //now parse the args
        let args = self.parse_function_call_args(&resolved_func_decl.get_arg_types(), parser_func_context, parser_context);
        
        generate_generic_func_call(func_name, generic_func, args, &resolved_types, &resolved_func_decl, &loc, parser_context)
    }

    //Parse the function call of a generic function, where we don't know the type arguments.
    fn parse_generic_func_call_implicit_types(
        &mut self,
        func_name: &String,
        generic_func: &GenericFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();

        //generate a first pass args and a type map. This means accepting the arg types as they are
        let mut resolved_type_map = new_type_map(&generic_func.type_args);
        let (first_pass_args, unchecked_args_loc) = self.parse_function_call_args_unchecked(parser_func_context, parser_context);

        //now run the deduction
        deduce_generic_function_call_args(&first_pass_args, &generic_func.func.decl.get_arg_types(), &mut resolved_type_map, &unchecked_args_loc, parser_context);

        generate_generic_func_call_deduced_types(func_name, generic_func, &resolved_type_map, &first_pass_args, &loc, parser_context)
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

    pub (crate) fn parse_static_func_call(
        &mut self,
        func_name: &String,
        func_decl: &FuncDecl,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let arg_types = func_decl.get_arg_types();
        let args = self.parse_function_call_args(&arg_types, parser_func_context, parser_context);
        let func_return_type = func_decl.return_type.clone();
        let loc = SourceLocation::new(loc.start.clone(), args.last().map(|a| a.loc.end.clone()).unwrap_or(loc.end.clone()));
        TypedExpr{expr: Expr::StaticFuncCall(func_name.clone(), func_decl.clone(), args), r#type: func_return_type, is_const: true, loc: loc}
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
                        Some(self.parse_static_func_call(&func_name, &f_d, &loc, parser_func_context, parser_context))
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
                        Some(self.parse_static_func_call(&full_name, &f_d, &loc, parser_func_context, parser_context))
                    },
                    None => {
                        self.try_parse_generic_func_call(&full_name, parser_func_context, parser_context)
                    }
                }
            }
        }
    }

    pub (crate) fn generate_func_call(
        &mut self,
        full_name: &String,
        unchecked_args: &Vec<TypedExpr>,
        loc: &SourceLocation,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let o_f_d = parser_context.get_fn_decl_from_decls(&full_name);
        match &o_f_d {
            Some(func_decl) => {
                generate_static_func_call(full_name, func_decl, unchecked_args, loc, parser_context)
            }
            None => {
                let o_f_d = parser_context.get_fn_decl_from_imports(&full_name);
                match &o_f_d {
                    Some(func_decl) => {
                        generate_static_func_call(full_name, func_decl, unchecked_args, loc, parser_context)
                    }
                    None => {
                        let o_g_f = parser_context.get_generic_fn_from_generics(full_name);
                        match o_g_f {
                            Some(generic_func) => {
                                let mut resolved_type_map = new_type_map(&generic_func.type_args);
                                deduce_generic_function_call_args(
                                    &unchecked_args, &generic_func.func.decl.get_arg_types(), &mut resolved_type_map, loc, parser_context);
                        
                                generate_generic_func_call_deduced_types(full_name, &generic_func, &resolved_type_map, &unchecked_args, loc, parser_context)
                            },
                            None => {
                                parser_context.push_err(Error::FuncNotRecognized(loc.clone(), full_name.clone()));
                                TypedExpr{expr: Expr::NoOp, is_const: true, loc: loc.clone(), r#type: Type::Undeclared}
                            }
                        }
                    }
                }
            }
        }
    }
}