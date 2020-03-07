use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;

use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
pub use errs::prelude::*;


use lazy_static;

lazy_static!{
    static ref INT_METHODS: Vec<FuncDecl> = vec![
        FuncDecl{name: String::from("countLeadingZeros"), return_type: Type::Int, args: vec![], export: false, generic_impl: false},
        FuncDecl{name: String::from("countTrailingZeros"), return_type: Type::Int, args: vec![], export: false, generic_impl: false},
        FuncDecl{name: String::from("shiftLeft"), return_type: Type::Int, args: vec![FuncArg{name: String::from("n"), r#type: Type::Int}], export: false, generic_impl: false},
        FuncDecl{name: String::from("shiftRight"), return_type: Type::Int, args: vec![FuncArg{name: String::from("n"), r#type: Type::Int}], export: false, generic_impl: false},
    ];
}

fn get_func_decl(name: &String) -> Option<&FuncDecl> {
    INT_METHODS.iter().find(|x| x.name == *name)
}

impl<'a> Parser<'a> {
    pub (crate) fn parse_int_component(&mut self,
        holding: &TypedExpr,
        id: &String,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let o_func_decl = get_func_decl(id);
        match o_func_decl {
            Some(func_decl) => {
                let arg_types = func_decl.get_arg_types();
                let args = self.parse_function_call_args(&arg_types, parser_func_context, parser_context);
                TypedExpr{expr: Expr::MemberFuncCall(Box::new(holding.clone()), id.clone(), args), r#type: func_decl.return_type.clone(), is_const: true, loc: loc.clone()}
            },
            None => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), Type::Int.clone(), id.clone()));
                TypedExpr{expr: Expr::MemberFuncCall(Box::new(holding.clone()), id.clone(), vec![]), r#type: Type::Int, is_const: true, loc: loc.clone()}
            }
        }
    }
}