use crate::Parser;
use crate::{ParserContext, UnsafeParseMode};
use crate::ParserFuncContext;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::{Error};
use errs::prelude::*;
use crate::{expect_semicolon, expect_ident, expect_punct, cast_typed_expr};
use crate::create_cast;

use lazy_static;

lazy_static!{
    static ref PTR_PUOM_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_mut(&Type::UnsafePtr), in_types: vec![FullType::new_mut(&Type::UnsafePtr), FullType::new_const(&INT_U_32)]},
    ];

    static ref PTR_PUO_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_const(&Type::UnsafePtr), in_types: vec![FullType::new_const(&Type::UnsafePtr), FullType::new_const(&INT_U_32)]},
    ];

    static ref PTR_MUOM_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_mut(&Type::UnsafePtr), in_types: vec![FullType::new_mut(&Type::UnsafePtr), FullType::new_const(&INT_U_32)]},
    ];
}

impl<'a> Parser<'a> {
    pub(crate) fn parse_mem_grow(&mut self,    
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let next = self.peek_next_item();
        if !next.token.matches_number_str("0") {
            parser_context.push_err(Error::NotYetImplemented(loc.clone(), String::from("first arg to mem size")));
        }
        self.skip_next_item();
        expect_punct!(self, parser_context, Punct::Comma);
        
        let expr = self.parse_expr(parser_func_context, parser_context);

        let loc_after = self.peek_next_location();
        loc.end = loc_after.end.clone();

        expect_punct!(self, parser_context, Punct::CloseParen);
        TypedExpr{expr: Expr::Intrinsic(Intrinsic::MemoryGrow(Box::new(expr))), r#type: FullType::new(&SIZE_T, Mutability::Const), loc: loc}
    }

    pub(crate) fn parse_mem_size(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        self.parse_empty_function_call_args(parser_func_context, parser_context);
        TypedExpr{expr: Expr::Intrinsic(Intrinsic::MemorySize), r#type: FullType::new(&SIZE_T, Mutability::Const), loc: loc}
    }

    pub(crate) fn parse_trap(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        self.parse_empty_function_call_args(parser_func_context, parser_context);
        TypedExpr{expr: Expr::Intrinsic(Intrinsic::Trap), r#type: FullType::new(&Type::Never, Mutability::Const), loc: loc}
    }

    pub(crate) fn parse_sizeof(&mut self,    
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        self.skip_next_item();
        let expr = self.parse_expr(parser_func_context, parser_context);
        expect_punct!(self, parser_context, Punct::CloseParen);

        match expr.r#type.r#type {
            Type::TypeLiteral(t) => {
                TypedExpr{expr: Expr::SizeOf((t.r#type).clone()), r#type: FullType::new(&SIZE_T, Mutability::Const), loc: loc}
            },
            _ => {
                parser_context.push_err(Error::NotYetImplemented(loc, String::from("__sizeof only works on __structs")));
                TypedExpr{expr: Expr::NoOp, r#type: FullType::new(&SIZE_T, Mutability::Const), loc: loc}
            }
        }
    }

    pub(crate) fn parse_unsafe_static(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
            parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
        }
        
        self.skip_next_item();

        let type_to_construct = self.parse_full_type(parser_context);

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        match lookahead {
            Token::Punct(Punct::OpenBrace) => {
                let out = self.parse_object_literal_to_vec(&type_to_construct.r#type, &loc, parser_func_context, parser_context);
                TypedExpr{expr: Expr::ConstructStaticFromObjectLiteral(type_to_construct.r#type.clone(), out), loc: loc, r#type: type_to_construct}
            },
            Token::Punct(Punct::OpenBracket) => {
                let out = self.parse_array_literal_to_vec(&type_to_construct.r#type, &loc, parser_func_context, parser_context);
                TypedExpr{expr: Expr::ConstructStaticFromArrayLiteral(type_to_construct.r#type.clone(), out), loc: loc, r#type: type_to_construct}
            },
            _ => {
                let out = self.parse_object_literal_to_vec(&type_to_construct.r#type, &loc, parser_func_context, parser_context);
                TypedExpr{expr: Expr::ConstructStaticFromObjectLiteral(type_to_construct.r#type.clone(), out), loc: loc, r#type: type_to_construct}
            }
        }
    }

    pub (crate) fn parse_struct_decl(&mut self,    
        export: bool,
        parser_context: &mut ParserContext,
    ) -> () {
        let mut loc = self.peek_next_location();
        if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
            parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
        }
        self.skip_next_item();

        let e_next = self.next_item();
        let next = match e_next {
            Err(e) => {
                parser_context.push_err(e);
                return ;
            },
            Ok(next) => {
                next
            }
        };

        let id = expect_ident!(next, parser_context, "Expecting __struct name to be an identifier");

        if parser_context.type_map.contains_key(&id.to_string()) {
            parser_context.push_err(Error::DuplicateTypeName(next.location.clone(), id.clone()))
        }

        parser_context.push_empty_type_scope();

        let mut members: Vec<StructMember> = vec![];

        expect_punct!(self, parser_context, Punct::OpenBrace);

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
                            self.skip_next_item();
                            parser_context.push_err(self.unexpected_token_error_raw(lookahead_item.span, &lookahead_item.location, "expecting '}'"));
                        }
                    }
                },
                Token::Ident(ref i) => {
                    self.skip_next_item();
                    expect_punct!(self, parser_context, Punct::Colon);
                    let member_type = self.parse_type(parser_context);
                    expect_semicolon!(self, parser_context);
                    members.push(StructMember{name: i.to_string(), r#type: member_type.clone()});
                },
                _ => {
                    self.skip_next_item();
                    parser_context.push_err(self.unexpected_token_error_raw(lookahead_item.span, &lookahead_item.location, "expecting '}' or member"));
                }
            }
        }

        parser_context.pop_type_scope();

        parser_context.type_map.insert(id.to_string(), TypeDecl::Struct{struct_type: StructType{members: members}, under_construction: false, export: export, name: id.to_string()});
    }

    ///Helper function to create a '__null' expr
    pub(crate) fn create_unsafe_null(
        loc: &SourceLocation,
    ) -> TypedExpr {
        TypedExpr{expr:Expr::UnsafeNull, r#type: FullType::new(&Type::UnsafeNull, Mutability::Const), loc: loc.clone()}
    }

    /// Parse '__Some(x)'. Because options are built in at a fairly low level, this is a parser function, not a library function.
    pub(crate) fn parse_unsafe_some(&mut self, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
            parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
        }

        self.skip_next_item();
        expect_punct!(self, parser_context, Punct::OpenParen);
        let inner = self.parse_expr(parser_func_context, parser_context);
        expect_punct!(self, parser_context, Punct::CloseParen);
        match inner.r#type.r#type {
            Type::UnsafeStruct{name: _} => {

            },
            _ => {
                parser_context.errors.push(Error::NotYetImplemented(inner.loc, String::from("Options on anything other than a __struct")));
            }
        }

        let inner_loc = inner.loc.clone();
        let inner_type = inner.r#type.clone();
        TypedExpr{expr: Expr::UnsafeSome(Box::new(inner)), r#type: FullType::new(&Type::UnsafeSome(Box::new(inner_type.r#type)), inner_type.mutability), loc: inner_loc}
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
                    r#type: FullType::new(&Type::Bool, Mutability::Const), loc: loc.clone()
                }
            },
            "isNone" => {
                self.parse_empty_function_call_args(parser_func_context, parser_context);
                TypedExpr{
                    expr: Expr::BinaryOperator{lhs: Box::new(lhs.clone()), op: BinaryOperator::Equal, rhs: Box::new(Parser::create_unsafe_null(&loc))},
                    r#type: FullType::new(&Type::Bool, Mutability::Const), loc: loc.clone()
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
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), lhs.r#type.r#type.clone(), component.clone()));
                lhs.clone()
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
        let expr = self.parse_expr(parser_func_context, parser_context);
        let lhs_clone = lhs.clone();
        let cast_expr = cast_typed_expr(&FullType::new_const(&Type::Int(0, U_32_MAX)), Box::new(expr), CastType::Implicit, parser_context);
        expect_punct!(self, parser_context, Punct::CloseBracket);
        TypedExpr{
            expr: Expr::DynamicMember(Box::new(lhs_clone), Box::new(cast_expr)),
            r#type: FullType::new(inner_type, lhs.r#type.mutability),
            loc: loc,
        }
    }

    pub (crate) fn parse_unsafe_ptr_component(&mut self,
        holding: &TypedExpr,
        id: &String,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let func_types: &Vec<FuncType> = match id.as_str() {
            "plusUOffsetMut" => &PTR_PUOM_METHODS,
            "plusUOffset" => &PTR_PUO_METHODS,
            "minusUOffsetMut" => &PTR_MUOM_METHODS,
            _ => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), holding.r#type.r#type.clone(), id.clone()));
                return TypedExpr{expr: Expr::MemberFuncCall(Box::new(holding.clone()), id.clone(), vec![]), r#type: holding.r#type.clone(), loc: loc.clone()};
            }
        };
    
        let mut arg_types: Vec<FullType> = vec![holding.r#type.clone()];
        let (args, _) = self.parse_function_call_args_unchecked(parser_func_context, parser_context);
        for arg in &args {
            arg_types.push(arg.r#type.clone());    
        }
        let t = get_type_casts_for_function_set(&func_types, &arg_types);

        match t {
            None => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), holding.r#type.r#type.clone(), id.clone()));
                TypedExpr{expr: Expr::MemberFuncCall(Box::new(holding.clone()), id.clone(), vec![]), r#type: holding.r#type.clone(), loc: loc.clone()}
            },
            Some(FuncCallTypeCast{func_type, arg_type_casts}) => {
                let out_self_type_cast = arg_type_casts.get(0).unwrap().clone();
                let out_self_type = get_type_from_type_cast(&out_self_type_cast, &func_type.in_types.get(0).unwrap());
                let o_cast = create_cast(&out_self_type, holding, &out_self_type_cast);

                let mut out_args = vec![];
                let mut i = 1;
                while i < arg_type_casts.len() {
                    let out_type_cast = arg_type_casts.get(i).unwrap().clone();
                    let out_type = get_type_from_type_cast(&out_type_cast, &func_type.in_types.get(i).unwrap());
                    let o_cast = create_cast(&out_type, &args[i-1], &out_type_cast);
                    out_args.push(o_cast.unwrap().clone());
                    i += 1;
                }

                TypedExpr{expr: Expr::MemberFuncCall(Box::new(o_cast.unwrap().clone()), id.clone(), out_args), r#type: func_type.out_type.clone(), loc: loc.clone()}
            }
        }
    }
}
