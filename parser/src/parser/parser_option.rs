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

impl<'a> Parser<'a> {
    ///Helper function to create a 'null' expr
    pub(crate) fn create_null(
        loc: &SourceLocation,
    ) -> TypedExpr {
        TypedExpr{expr:Expr::Null, r#type: Type::Option(Box::new(Type::Never)), is_const: true, loc: loc.clone()}
    }

    pub(crate) fn parse_option_component(&mut self,
        lhs: &TypedExpr,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let next_item = self.next_item();
        assert_ok!(next_item);
        let id = assert_ident!(next_item, "Expecting int component to be an identifier");
        
        let component = id.to_string();
        let loc = next_item.location;

        match component.as_ref() {
            "isDefined" => {
                self.parse_empty_function_call_args(parser_func_context, parser_context);
                Ok(TypedExpr{
                    expr: Expr::BinaryOperator(BinaryOperatorApplication{lhs: Box::new(lhs.clone()), op: BinaryOperator::NotEqual, rhs: Box::new(Parser::create_null(&loc))}),
                    r#type: Type::Boolean, is_const: true, loc: loc.clone()
                })
            },
            "isEmpty" => {
                self.parse_empty_function_call_args(parser_func_context, parser_context);
                Ok(TypedExpr{
                    expr: Expr::BinaryOperator(BinaryOperatorApplication{lhs: Box::new(lhs.clone()), op: BinaryOperator::Equal, rhs: Box::new(Parser::create_null(&loc))}),
                    r#type: Type::Boolean, is_const: true, loc: loc.clone()
                })
            },
            _ => {
                Err(Error::ObjectHasNoMember(next_item.location.clone(), component))
            }
        }
    }
}