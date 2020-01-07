use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::Res;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use crate::assert_punct;
use crate::assert_ok;

impl<'a> Parser<'a> {
    pub(crate) fn parse_mem_grow(&mut self,    
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let next = self.next_item()?;
        if !next.token.matches_number_str("0") {
            return Err(Error::NotYetImplemented(loc.clone(), String::from("first arg to mem size")));
        }
        assert_punct!(self, Punct::Comma);
        
        let expr = self.parse_expr(parser_func_context, parser_context);
        assert_ok!(expr);

        let loc_after = self.peek_next_location();
        loc.end = loc_after.end.clone();

        assert_punct!(self, Punct::CloseParen);
        Ok(TypedExpr{expr: Expr::Intrinsic(Intrinsic::MemoryGrow(Box::new(expr))), is_const: true, r#type: Type::UnsafeSizeT, loc: loc})
    }

    pub(crate) fn parse_mem_size(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let loc = self.peek_next_location();
        self.parse_empty_function_call_args(parser_func_context, parser_context);
        Ok(TypedExpr{expr: Expr::Intrinsic(Intrinsic::MemorySize), is_const: true, r#type: Type::UnsafeSizeT, loc: loc})
    }

    pub(crate) fn parse_trap(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let loc = self.peek_next_location();
        self.parse_empty_function_call_args(parser_func_context, parser_context);
        Ok(TypedExpr{expr: Expr::Intrinsic(Intrinsic::Trap), is_const: true, r#type: Type::Never, loc: loc})
    }

    pub(crate) fn parse_sizeof(&mut self,    
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let loc = self.peek_next_location();
        self.skip_next_item();
        let expr = self.parse_expr(parser_func_context, parser_context);
        assert_punct!(self, Punct::CloseParen);

        assert_ok!(expr);
        match expr.r#type {
            Type::TypeLiteral(t) => {
                Ok(TypedExpr{expr: Expr::SizeOf((*t).clone()), is_const: true, r#type: Type::UnsafeSizeT, loc: loc})
            },
            _ => {Err(Error::NotYetImplemented(loc, String::from("__sizeof only works on __structs")))}
        }
    }

    pub(crate) fn parse_unsafe_static(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let loc = self.peek_next_location();
        
        self.skip_next_item();

        let type_to_construct = self.parse_type(parser_context);
        assert_ok!(type_to_construct);

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        match lookahead {
            Token::Punct(p) => {
                match p {
                    Punct::OpenBrace => {
                        let out = self.parse_object_literal_to_vec(&type_to_construct, &loc, parser_func_context, parser_context);
                        assert_ok!(out);
                        Ok(TypedExpr{expr: Expr::ConstructStaticFromObjectLiteral(type_to_construct.clone(), out), is_const: true, loc: loc, r#type: type_to_construct.clone()})
                    },
                    Punct::OpenBracket => {
                        let out = self.parse_array_literal_to_vec(&type_to_construct, &loc, parser_func_context, parser_context);
                        assert_ok!(out);
                        Ok(TypedExpr{expr: Expr::ConstructStaticFromArrayLiteral(type_to_construct.clone(), out), is_const: true, loc: loc, r#type: type_to_construct.clone()})
                    },
                    _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "{"),
                }
            },
            _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "{"),
        }
    }
}
