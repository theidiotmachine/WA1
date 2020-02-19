use std::collections::{HashMap};

use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::Res;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;

use crate::assert_ok;
use crate::try_create_cast;
use crate::assert_next;
use crate::{assert_ident, assert_punct, expect_keyword, expect_next, expect_ident, expect_ok, expect_punct};

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
        let args = self.parse_function_call_args(&vec![], parser_func_context, parser_context);
        if args.is_err() { 
            return parser_context.errors.push(args.unwrap_err());
        }
    }

    pub(crate) fn parse_function_call_args(&mut self,
        arg_types: &Vec<Type>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<Vec<TypedExpr>> {
        let mut out: Vec<TypedExpr> = vec![];
        self.skip_next_item();

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        match lookahead {
            Token::Punct(p) => match p {
                Punct::CloseParen => { 
                    self.skip_next_item();
                    if arg_types.len() != 0 {
                        parser_context.errors.push(Error::NotEnoughArgs);        
                    }
                    return Ok(out);
                },
                _ => {}
            },
            _ => {}
        }

        loop{
            let expr = self.parse_expr(parser_func_context, parser_context);
            assert_ok!(expr);
            
            if out.len() == arg_types.len() {
                parser_context.errors.push(Error::TooManyArgs);
            } else {
                let arg_type = &arg_types[out.len()];
                if *arg_type != expr.r#type {
                    let expr_type = expr.r#type.clone();
                    let o_cast = try_create_cast(arg_type, &expr, true);
                    match o_cast {
                        None => parser_context.errors.push(Error::TypeFailure(expr.loc.clone(), arg_type.clone(), expr_type)),
                        Some(new_expr) => out.push(new_expr)
                    }
                } else {
                    out.push(expr);
                }
            }

            let lookahead_item = self.peek_next_item();
            let lookahead = lookahead_item.token;
         
            match lookahead {
                Token::Punct(p) => match p {
                    Punct::CloseParen => { 
                        self.skip_next_item();
                        if arg_types.len() != out.len() {
                            parser_context.errors.push(Error::NotEnoughArgs);        
                        }
                        return Ok(out);
                    },
                    Punct::Comma => {
                        self.skip_next_item();
                    },
                    _ => return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "need expr or comma")
                },
                _ => return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "need expr or comma")
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
            self.context.push_type_scope(&type_args);
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
            name: id.to_string(), return_type: return_type.clone(), args: arg_list, export, 
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
            self.context.pop_type_scope();
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
            Some(TypedExpr{expr: Expr::FuncDecl(FuncObjectCreation{name: name, closure: func_closure}), is_const: true, r#type: Type::Func{func_type: Box::new(func_type), type_args: vec![]}, loc: loc})
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
}