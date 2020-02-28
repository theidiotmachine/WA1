use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use crate::expect_ident;

impl<'a> Parser<'a> {
    pub(crate) fn parse_int_component(&mut self,
        lhs: &TypedExpr,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let next_item = self.next_item();
        if next_item.is_err() { 
            parser_context.push_err(next_item.unwrap_err());
            return lhs.clone();
        } let next_item = next_item.unwrap();

        let id = expect_ident!(next_item, parser_context, "Expecting int component to be an identifier");
        
        let component = id.to_string();

        match component.as_ref() {
            "countTrailingZeros" => {
                self.parse_empty_function_call_args(parser_func_context, parser_context);
                TypedExpr{expr: Expr::Intrinsic(Intrinsic::I32Ctz(Box::new(lhs.clone()))), r#type: Type::Int, is_const: true, loc: next_item.location.clone()}
            },
            "shiftLeft" => {
                let args = self.parse_function_call_args(&vec![Type::Int], parser_func_context, parser_context);
                TypedExpr{expr: Expr::Intrinsic(Intrinsic::I32ShL(Box::new(lhs.clone()), Box::new(args[0].clone()))), r#type: Type::UnsafeSizeT, is_const: true, loc: next_item.location.clone()}
            },
            "shiftRight" => {
                let args = self.parse_function_call_args(&vec![Type::Int], parser_func_context, parser_context);
                TypedExpr{expr: Expr::Intrinsic(Intrinsic::I32ShRS(Box::new(lhs.clone()), Box::new(args[0].clone()))), r#type: Type::UnsafeSizeT, is_const: true, loc: next_item.location.clone()}
            },
            _ => {
                parser_context.push_err(Error::ObjectHasNoMember(next_item.location.clone(), component));
                lhs.clone()
            }
        }
    }
}