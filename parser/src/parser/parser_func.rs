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

impl<'a> Parser<'a> {
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
                    let o_cast = try_create_cast(arg_type, &expr);
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
}