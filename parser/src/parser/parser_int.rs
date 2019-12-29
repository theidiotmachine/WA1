use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::Res;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use crate::assert_ident;
use crate::assert_ok;
use crate::assert_peek_punct;

impl<'a> Parser<'a> {
    pub(crate) fn parse_int_component(&mut self,
        lhs: &TypedExpr,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let next_item = self.next_item();
        assert_ok!(next_item);
        let id = assert_ident!(next_item, "Expecting int component to be an identifier");
        
        let component = id.to_string();

        match component.as_ref() {
            "countTrailingZeros" => {
                assert_peek_punct!(self, Punct::OpenParen);
                let args = self.parse_function_call_args(&vec![], parser_func_context, parser_context);
                if args.is_err() { 
                    return Err(args.unwrap_err()) 
                }
                Ok(TypedExpr{expr: Expr::Intrinsic(Intrinsic::I32Ctz(Box::new(lhs.clone()))), r#type: Type::Int, is_const: true, loc: next_item.location.clone()})
            },
            _ => {
                Err(Error::ObjectHasNoMember(next_item.location.clone(), component))
            }
        }
    }
}