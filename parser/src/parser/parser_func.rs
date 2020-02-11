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
use crate::{assert_ident, assert_punct};

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

    pub(crate) fn parse_function_decl(&mut self,
        export: bool,
        parser_context: &mut ParserContext,
    ) -> Res<FuncDecl> {
        self.expect_keyword(Keyword::Function)?;
        let next = assert_next!(self, "Expecting function name");
        let id = assert_ident!(next, "Expecting function name to be an identifier");

        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_punct(Punct::LessThan) {
            let type_args = self.parse_type_decl_args();
            assert_ok!(type_args);
            self.context.push_func_type_scope(&type_args);
            parser_context.errors.push(Error::NotYetImplemented(next.location.clone(), String::from("generic functions")));
        } else {
            self.context.push_empty_func_type_scope();
        }

        let arg_list = self.parse_function_decl_args(parser_context);
        assert_ok!(arg_list);

        assert_punct!(self, Punct::Colon);
        let return_type = self.parse_type(parser_context);
        assert_ok!(return_type);

        Ok(FuncDecl{
            name: id.to_string(), return_type: return_type.clone(), args: arg_list, export, 
        })
    }
}