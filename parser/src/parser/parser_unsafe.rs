use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::Res;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use crate::{assert_punct, assert_ok, assert_semicolon, assert_next, assert_ident};

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
        if !parser_context.is_unsafe {
            parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
        }
        
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

    pub (crate) fn parse_struct_decl(&mut self,    
        export: bool,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        if !parser_context.is_unsafe {
            parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
        }
        self.skip_next_item();

        let next = assert_next!(self, "Expecting struct name");
        let id = assert_ident!(next, "Expecting struct name to be an identifier");

        if parser_context.type_map.contains_key(&id.to_string()) {
            parser_context.errors.push(Error::DuplicateTypeName(id.to_string()))
        }

        self.context.push_empty_func_type_scope();

        let mut members: Vec<StructMember> = vec![];

        assert_punct!(self, Punct::OpenBrace);

        parser_context.type_map.insert(id.to_string(), TypeDecl::Struct{struct_type: StructType{members: vec![]}, under_construction: true, export: export, name: id.to_string()});

        loop {
            let lookahead_item = self.peek_next_item();
            loc.end = lookahead_item.location.end.clone();
            let lookahead = lookahead_item.token;
         
            match lookahead {
                Token::Punct(ref p) => {
                    match p {
                        Punct::CloseBrace => {
                            self.skip_next_item();
                            break;
                        },
                        _ => {
                            return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "expecting '}'")
                        }
                    }
                },
                Token::Ident(ref i) => {
                    self.skip_next_item();
                    assert_punct!(self, Punct::Colon);
                    let member_type = self.parse_type(parser_context);
                    assert_ok!(member_type);
                    assert_semicolon!(self);
                    members.push(StructMember{name: i.to_string(), r#type: member_type.clone()});
                },
                _ => {
                    return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "expecting '}' or member")
                }
            }
        }

        self.context.pop_type_scope();

        parser_context.type_map.insert(id.to_string(), TypeDecl::Struct{struct_type: StructType{members: members}, under_construction: false, export: export, name: id.to_string()});

        Ok(TypedExpr{expr: Expr::StructDecl(id.to_string()), is_const: true, r#type: Type::RealVoid, loc: loc})
    }
}
