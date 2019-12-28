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

        //assert_punct!(self, Punct::OpenParen);
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
        Ok(TypedExpr{expr: Expr::Intrinsic(Intrinsic::MemoryGrow(Box::new(expr))), is_const: true, r#type: Type::Ptr(PtrAlign::Align0), loc: loc})
    }

    pub(crate) fn parse_mem_size(&mut self) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let loc_after = self.peek_next_location();
        loc.end = loc_after.end.clone();

        assert_punct!(self, Punct::CloseParen);
        
        Ok(TypedExpr{expr: Expr::Intrinsic(Intrinsic::MemorySize), is_const: true, r#type: Type::Ptr(PtrAlign::Align0), loc: loc})
    }

    pub(crate) fn parse_trap(&mut self) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();
        let loc_after = self.peek_next_location();
        loc.end = loc_after.end.clone();

        assert_punct!(self, Punct::CloseParen);
        
        Ok(TypedExpr{expr: Expr::Intrinsic(Intrinsic::Trap), is_const: true, r#type: Type::Never, loc: loc})
    }
}
