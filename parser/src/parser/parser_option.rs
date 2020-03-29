use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::Res;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use errs::prelude::*;
use crate::assert_punct;

impl<'a> Parser<'a> {
    ///Helper function to create a 'null' expr
    pub(crate) fn create_null(
        loc: &SourceLocation,
    ) -> TypedExpr {
        TypedExpr{expr:Expr::Null, r#type: Type::Option(Box::new(Type::Never)), is_const: true, loc: loc.clone()}
    }

    /// Parse 'Some(x)'. Because options are built in at a fairly low level, this is a parser function, not a library function.
    pub(crate) fn parse_some(&mut self, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr>{
        self.skip_next_item();
        assert_punct!(self, Punct::OpenParen);
        let inner = self.parse_expr(parser_func_context, parser_context);
        assert_punct!(self, Punct::CloseParen);
        match inner.r#type {
            Type::UnsafeStruct{name: _} => {

            },
            _ => {
                parser_context.errors.push(Error::NotYetImplemented(inner.loc, String::from("Options on anything other than a __struct")));
            }
        }

        let inner_loc = inner.loc.clone();
        let inner_type = inner.r#type.clone();
        Ok(TypedExpr{expr: Expr::FreeTypeWiden(Box::new(inner)), r#type: Type::Option(Box::new(inner_type)), loc: inner_loc, is_const: true})
    }
}