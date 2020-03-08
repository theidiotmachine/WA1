use std::collections::{HashMap};

use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
pub use errs::prelude::*;

use crate::tree_transform::{Transform, transform_expr, transform_lvalue_expr, transform_typed_expr};

use crate::{expect_keyword, expect_next, expect_ident, expect_punct, cast_typed_expr};

/// Take a generic func, and a set of values for the type variables, and instantiate a
/// resolved func decl. 
fn resolve_generic_func_decl(
    generic_func: &GenericFunc,
    resolved_name: &String,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> FuncDecl{
    //let orig_type_args = &generic_func.type_args;
    let mut out_args: Vec<FuncArg> = vec![];
    for orig_arg in &generic_func.func.decl.args {
        let new_type = transform_type(&orig_arg.r#type, type_map, loc, parser_context);
        out_args.push(FuncArg{name: orig_arg.name.clone(), r#type: new_type});
    }

    let out_return_type = transform_type(&generic_func.func.decl.return_type, type_map, loc, parser_context);

    FuncDecl{args: out_args, export: generic_func.func.decl.export, name: resolved_name.clone(), return_type: out_return_type, generic_impl: true}
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
    generic_local_vars: &Vec<VariableDecl>,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut ParserContext,
) -> Vec<VariableDecl> {
    let mut out = vec![];
    for generic_local_var in generic_local_vars {
        let new_type = transform_type(&generic_local_var.r#type, type_map, loc, parser_context);
        out.push(VariableDecl{
            internal_name: generic_local_var.internal_name.clone(),
            orig_name: generic_local_var.orig_name.clone(),
            r#type: new_type,
            constant: generic_local_var.constant,
            init: generic_local_var.init.clone(),
            closure_source: generic_local_var.closure_source,
            arg: generic_local_var.arg
        })
    }
    out
}

struct GenericFuncTypeTransformer{
    pub type_map: HashMap<String, Type>,
}

impl Transform for GenericFuncTypeTransformer{
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
            Expr::VariableDecl(vd) => {
                let new_type = transform_type(&vd.r#type, &self.type_map, loc, parser_context);
                Some(Expr::VariableDecl(Box::new(VariableDecl{
                    internal_name: vd.internal_name.clone(),
                    orig_name: vd.orig_name.clone(),
                    r#type: new_type,
                    constant: vd.constant,
                    init: vd.init.as_ref().map(|te| transform_typed_expr(&te, self, parser_context)),
                    closure_source: vd.closure_source,
                    arg: vd.arg
                })))
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
}

fn transform_generic_body(
    generic_body: &Option<TypedExpr>,
    type_map: &HashMap<String, Type>,
    parser_context: &mut ParserContext,
) -> Option<TypedExpr> {
    match generic_body {
        None => None,
        Some(typed_expr) => {
            let mut transformer = GenericFuncTypeTransformer{type_map: type_map.clone()};
            Some(transform_typed_expr(typed_expr, &mut transformer, parser_context))
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

    pub(crate) fn parse_function_call_args(&mut self,
        arg_types: &Vec<Type>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Vec<TypedExpr> {
        let mut out: Vec<TypedExpr> = vec![];
        self.skip_next_item();

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        match lookahead {
            Token::Punct(p) => match p {
                Punct::CloseParen => { 
                    self.skip_next_item();
                    if arg_types.len() != 0 {
                        parser_context.errors.push(Error::NotEnoughArgs(lookahead_item.location.clone()));
                    }
                    return out;
                },
                _ => {}
            },
            _ => {}
        }

        loop{
            let expr = self.parse_expr(parser_func_context, parser_context);
            //remove this when parse_expr uses TSMGO error handling
            if expr.is_err() { return out; }; let expr = expr.unwrap();
            
            if out.len() == arg_types.len() {
                parser_context.errors.push(Error::TooManyArgs(expr.loc.clone()));
            } else {
                let arg_type = &arg_types[out.len()];
                let cast = cast_typed_expr(arg_type, Box::new(expr), true, parser_context);
                out.push(cast);
            }

            let lookahead_item = self.peek_next_item();
            let lookahead = lookahead_item.token;
         
            match lookahead {
                Token::Punct(p) => match p {
                    Punct::CloseParen => { 
                        self.skip_next_item();
                        if arg_types.len() != out.len() {
                            parser_context.errors.push(Error::NotEnoughArgs(lookahead_item.location.clone()));
                        }
                        return out;
                    },
                    Punct::Comma => {
                        self.skip_next_item();
                    },
                    _ => {
                        parser_context.errors.push(Error::UnexpectedToken(lookahead_item.location.clone(), String::from("need expr or comma")));
                        self.skip_next_item();
                    }
                },
                _ => {
                    parser_context.errors.push(Error::UnexpectedToken(lookahead_item.location.clone(), String::from("need expr or comma")));
                    self.skip_next_item();
                }
            }
        }
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
        let err_ret = vec![];
        expect_punct!(self, parser_context, Punct::OpenParen, err_ret);
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
    fn check_return_value(&mut self, 
        block: &TypedExpr,
        parser_func_context_inner: &ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> () {
        if parser_func_context_inner.func_return_type != Type::RealVoid {
            match &block.expr {
                Expr::Block(exprs) => {
                    let o_last = exprs.last();
                    match o_last {
                        Some(last) => {
                            match &last.expr {
                                Expr::Return(o_e) => {
                                    if o_e.is_none() {
                                        parser_context.errors.push(Error::NoValueReturned(block.loc.clone()))
                                    }
                                },
                                _ => {
                                    if last.r#type == Type::RealVoid {
                                        parser_context.errors.push(Error::NoValueReturned(block.loc.clone()));
                                    }
                                },
                            }
                        },
                        _ => parser_context.errors.push(Error::NoValueReturned(block.loc.clone()))
                    }
                },
                Expr::Return(o_e) => {
                    if o_e.is_none() {
                        parser_context.errors.push(Error::NoValueReturned(block.loc.clone()))
                    }
                },
                _ => {
                    if block.r#type == Type::RealVoid {
                        parser_context.errors.push(Error::NoValueReturned(block.loc.clone()));
                    }
                },
            }
        }
    }

    ///Parse a func body to produce a full Func
    fn parse_func_body_internal(&mut self,
        func_decl: &FuncDecl,
        parser_context: &mut ParserContext,
    ) -> Func {
        let mut parser_func_context_inner = ParserFuncContext::new();

        self.context.push_func_scope();

        self.register_params(&func_decl.args, &mut parser_func_context_inner);

        parser_func_context_inner.func_return_type = func_decl.return_type.clone();

        let old_in_iteration = self.context.in_iteration;
        self.context.in_iteration = false;
        
        let r_block = self.parse_block(false, &mut parser_func_context_inner, parser_context);
        let o_block = if r_block.is_err() { 
            parser_context.push_err(r_block.unwrap_err());
            None
        } else {
            let block = r_block.unwrap();
            self.check_return_value(&block, &parser_func_context_inner, parser_context);
            Some(block)
        };

        self.context.pop_func_scope();
        self.context.in_iteration = old_in_iteration;

        Func{
            decl: func_decl.clone(),
            local_vars: parser_func_context_inner.local_vars, closure: parser_func_context_inner.closure, 
            local_var_map: parser_func_context_inner.local_var_map, body: o_block,
        }
    }

    fn parse_func_decl_internal(&mut self,
        export: bool,
        main_parse: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<TypedExpr> {
        let err_ret = None;
        expect_keyword!(self, parser_context, Keyword::Function, err_ret);

        let loc = self.peek_next_location();

        //First, parse the header
        let next = expect_next!(self, parser_context, err_ret);
        let id = expect_ident!(next, parser_context, "Expecting function name to be an identifier");

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
        expect_punct!(self, parser_context, Punct::Colon, err_ret);
        let return_type = self.parse_type(parser_context);

        //here's the decl
        let func_decl = FuncDecl{
            name: id.to_string(), return_type: return_type.clone(), args: arg_list, export, generic_impl: generic
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
            None
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
            Some(TypedExpr{expr: Expr::FuncDecl(FuncObjectCreation{name: name, closure: func_closure}), is_const: true, r#type: Type::Func{func_type: Box::new(func_type)}, loc: loc})
        }
    }

    pub(crate) fn main_parse_named_function_decl(&mut self, 
        export: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<TypedExpr> {
        self.parse_func_decl_internal(export, true, parser_func_context_outer, parser_context)
    }
    
    pub(crate) fn export_parse_named_function_decl(&mut self, 
        export: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> () {
        self.parse_func_decl_internal(export, false, parser_func_context_outer, parser_context);
    }

    fn parse_generic_func_call(
        &mut self,
        generic_func: &GenericFunc,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {

        //let's parse the types. At some point we want inference.
        let next = self.peek_next_item();
        let token = &next.token;

        let resolved_types: Vec<Type> = if token.matches_punct(Punct::LessThan) {
            self.skip_next_item();
            let mut loc = self.peek_next_location();
            let mut idx = 0;
            let mut resolved_types = vec![];
            let num = generic_func.type_args.len();
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

            resolved_types

        } else {
            parser_context.push_err(Error::NotYetImplemented(next.location.clone(), String::from("implicit type args")));
            let mut resolved_types = vec![];
            for _ in &generic_func.type_args {
                resolved_types.push(Type::Undeclared);
            }
            resolved_types
        };

        let mut runtime_type_args = vec![];
        let mut mangled_type_names = vec![];
        for resolved_type in &resolved_types {
            let o_runtime_type = get_runtime_type_for_generic(&resolved_type);
            match o_runtime_type {
                Some(runtime_type) => {
                    mangled_type_names.push(runtime_type.get_mangled_name());
                    runtime_type_args.push(runtime_type);
                },
                None => {
                    parser_context.push_err(Error::NotYetImplemented(next.location.clone(), format!("type {} not supported as generic argument", resolved_type)));
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

        let mut resolved_type_map: HashMap<String, Type> = HashMap::new();
        let mut i = 0;
        for type_arg in &generic_func.type_args {
            resolved_type_map.insert(type_arg.name.clone(), resolved_types[i].clone());
            i += 1;
        }

        let runtime_name = format!("{}<{}>", generic_func.func.decl.name, mangled_type_names.join(","));

        let resolved_func_decl: FuncDecl = resolve_generic_func_decl(generic_func, &generic_func.func.decl.name, &resolved_type_map, &next.location, parser_context);

        let runtime_func_decl = if !parser_context.generic_func_impls.contains(&runtime_name) {
            generate_runtime_generic_func(generic_func, &runtime_name, &runtime_type_map, &next.location, parser_context)
        } else {
            resolve_generic_func_decl(generic_func, &runtime_name, &runtime_type_map, &next.location, parser_context)
        };

        let args = self.parse_function_call_args(&resolved_func_decl.get_arg_types(), parser_func_context, parser_context);

        let mut runtime_args = vec![];
        let mut idx = 0;
        let runtime_arg_types = runtime_func_decl.get_arg_types();
        for arg in args {
            let cast_expr = cast_typed_expr(&(runtime_arg_types[idx]), Box::new(arg), false, parser_context);
            runtime_args.push(cast_expr);
            idx += 1;
        }

        let runtime_call = TypedExpr{expr: Expr::StaticFuncCall(runtime_name.clone(), runtime_args), r#type: runtime_func_decl.return_type.clone(), is_const: true, 
            loc: next.location.clone()};
        cast_typed_expr(&resolved_func_decl.return_type, Box::new(runtime_call), false, parser_context)
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
            Some(g_f) => Some(self.parse_generic_func_call(&g_f, parser_func_context, parser_context)),
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
        TypedExpr{expr: Expr::StaticFuncCall(func_name.clone(), args), r#type: func_return_type, is_const: true, loc: loc}
    }
}