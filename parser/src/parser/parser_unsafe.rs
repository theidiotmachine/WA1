use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::Res;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::{Error};
use errs::prelude::*;
use crate::{assert_punct, assert_ok, assert_semicolon, assert_next, assert_ident, expect_punct, cast_typed_expr};

use lazy_static;

lazy_static!{
    static ref UNSAFE_SIZE_T_METHODS: Vec<FuncDecl> = vec![
        FuncDecl{name: String::from("countLeadingZeros"), return_type: Type::Int, args: vec![], export: false, generic_impl: false, type_guard: None,},
        FuncDecl{name: String::from("countTrailingZeros"), return_type: Type::Int, args: vec![], export: false, generic_impl: false, type_guard: None,},
        FuncDecl{name: String::from("shiftLeft"), return_type: Type::UnsafeSizeT, args: vec![FuncArg{name: String::from("n"), r#type: Type::Int}], export: false, generic_impl: false, type_guard: None,},
        FuncDecl{name: String::from("shiftRight"), return_type: Type::UnsafeSizeT, args: vec![FuncArg{name: String::from("n"), r#type: Type::Int}], export: false, generic_impl: false, type_guard: None,},
    ];
}

fn get_func_decl(name: &String) -> Option<&FuncDecl> {
    UNSAFE_SIZE_T_METHODS.iter().find(|x| x.name == *name)
}

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
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        self.parse_empty_function_call_args(parser_func_context, parser_context);
        TypedExpr{expr: Expr::Intrinsic(Intrinsic::MemorySize), is_const: true, r#type: Type::UnsafeSizeT, loc: loc}
    }

    pub(crate) fn parse_trap(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        self.parse_empty_function_call_args(parser_func_context, parser_context);
        TypedExpr{expr: Expr::Intrinsic(Intrinsic::Trap), is_const: true, r#type: Type::Never, loc: loc}
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

        parser_context.push_empty_type_scope();

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
                    assert_semicolon!(self);
                    members.push(StructMember{name: i.to_string(), r#type: member_type.clone()});
                },
                _ => {
                    return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "expecting '}' or member")
                }
            }
        }

        parser_context.pop_type_scope();

        parser_context.type_map.insert(id.to_string(), TypeDecl::Struct{struct_type: StructType{members: members}, under_construction: false, export: export, name: id.to_string()});

        Ok(TypedExpr{expr: Expr::StructDecl(id.to_string()), is_const: true, r#type: Type::RealVoid, loc: loc})
    }

    ///Helper function to create a '__null' expr
    pub(crate) fn create_unsafe_null(
        loc: &SourceLocation,
    ) -> TypedExpr {
        TypedExpr{expr:Expr::UnsafeNull, r#type: Type::UnsafeNull, is_const: true, loc: loc.clone()}
    }

    /// Parse '__Some(x)'. Because options are built in at a fairly low level, this is a parser function, not a library function.
    pub(crate) fn parse_unsafe_some(&mut self, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr>{
        let loc = self.peek_next_location();
        if !parser_context.is_unsafe {
            parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
        }

        self.skip_next_item();
        assert_punct!(self, Punct::OpenParen);
        let inner = self.parse_expr(parser_func_context, parser_context);
        assert_ok!(inner);
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
        Ok(TypedExpr{expr: Expr::FreeTypeWiden(Box::new(inner)), r#type: Type::UnsafeSome(Box::new(inner_type)), loc: inner_loc, is_const: true})
    }

    pub(crate) fn parse_unsafe_option_component(&mut self,
        lhs: &TypedExpr,
        component: &String,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        match component.as_ref() {
            "isSome" => {
                self.parse_empty_function_call_args(parser_func_context, parser_context);
                TypedExpr{
                    expr: Expr::BinaryOperator{lhs: Box::new(lhs.clone()), op: BinaryOperator::NotEqual, rhs: Box::new(Parser::create_unsafe_null(&loc))},
                    r#type: Type::Boolean, is_const: true, loc: loc.clone()
                }
            },
            "isNone" => {
                self.parse_empty_function_call_args(parser_func_context, parser_context);
                TypedExpr{
                    expr: Expr::BinaryOperator{lhs: Box::new(lhs.clone()), op: BinaryOperator::Equal, rhs: Box::new(Parser::create_unsafe_null(&loc))},
                    r#type: Type::Boolean, is_const: true, loc: loc.clone()
                }
            },
            "unwrap" => {
                let o = self.try_parse_generic_func_call(&String::from("__Option_unwrap"), parser_func_context, parser_context);
                match o {
                    Some(out) => out,
                    None => {
                        panic!()        
                    }
                }
            },
            _ => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), lhs.r#type.clone(), component.clone()));
                lhs.clone()
            }
        }
    }

    pub(crate) fn parse_unsafe_size_t_component(&mut self,
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
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), Type::UnsafeSizeT.clone(), id.clone()));
                TypedExpr{expr: Expr::MemberFuncCall(Box::new(holding.clone()), id.clone(), vec![]), r#type: Type::UnsafeSizeT, is_const: true, loc: loc.clone()}
            }
        }
    }

    pub(crate) fn parse_unsafe_array_dynamic_component(&mut self,
        lhs: &TypedExpr,
        inner_type: &Type,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        let r_expr = self.parse_expr(parser_func_context, parser_context);
        if r_expr.is_err() { 
            parser_context.push_err(r_expr.unwrap_err());
            return lhs.clone();
        } 
        let expr = r_expr.unwrap();
        let lhs_clone = lhs.clone();
        let cast_expr = cast_typed_expr(&Type::Int, Box::new(expr), true, parser_context);
        expect_punct!(self, parser_context, Punct::CloseBracket);
        TypedExpr{
            expr: Expr::DynamicMember(Box::new(lhs_clone), Box::new(cast_expr)),
            r#type: inner_type.clone(),
            is_const: lhs.is_const,
            loc: loc,
        }
    }
}
