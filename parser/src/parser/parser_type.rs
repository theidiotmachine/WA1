use crate::Parser;
use crate::{ParserContext, UnsafeParseMode, ParserPhase, ParserFuncContext};

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use errs::prelude::*;
use crate::{Commitment, expect_punct, expect_next, expect_ident, check_privacy};

impl<'a> Parser<'a> {
    pub (crate) fn parse_type_args(&mut self,
        type_args: &Vec<TypeArg>,
        parser_context: &mut ParserContext,
    ) -> Vec<Type> {
        //skip '<'
        self.skip_next_item();
        let mut loc = self.peek_next_location();
        let mut idx = 0;
        let mut resolved_types = vec![];
        let num = type_args.len();

        //parse the explicit types
        while idx < num {
            let arg_type = self.parse_type(parser_context);
            if !matches_type_constraint(&arg_type, &(type_args[idx].constraint)) {
                parser_context.push_err(Error::FailedTypeArgConstraint(loc.clone()));
            }
            resolved_types.push(arg_type);
            let next = self.peek_next_item();
            let token = &next.token;
            if token.matches_punct(Punct::Comma) {
                self.skip_next_item();
                loc = self.peek_next_location();
                idx += 1;
            } else if token.matches_punct(Punct::GreaterThan) {
                self.skip_next_item();
                if idx == num - 1 {
                    idx += 1;
                } else {
                    parser_context.push_err(Error::MissingTypeArgs(next.location.clone()));
                    break;
                }
            }
        }

        while idx < num {
            resolved_types.push(Type::Undeclared);
            idx += 1;
        }

        resolved_types
    }

    pub(crate) fn parse_type_from_ident(&mut self, 
        ident: &String,
        commitment: Commitment,
        parser_context: &mut ParserContext,
    ) -> Option<Type> {
        let o_type = parser_context.get_type_decl(ident);
        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_punct(Punct::LessThan) {
            if commitment == Commitment::Committed {
                parser_context.push_err(Error::NotYetImplemented(next.location.clone(), String::from("type args")));
                Some(Type::Undeclared)
            } else {
                None
            }
        } else {
            match o_type {
                None => {
                    let o_scoped_type = parser_context.get_scoped_type(ident);
                    match o_scoped_type {
                        Some(scoped_type) => {
                            Some(Type::VariableUsage{name: scoped_type.name.clone(), constraint: scoped_type.constraint.clone()})
                        },
                        None => {
                            if commitment == Commitment::Committed {
                                parser_context.push_err(Error::InvalidTypeName(next.location.clone(), ident.clone()));
                                Some(Type::Undeclared)
                            } else {
                                None
                            }
                        }
                    }
                },
                Some(user_type) => {
                    match user_type {
                        TypeDecl::Struct{name: _, struct_type: _, under_construction: _, export: _} => Some(Type::UnsafeStruct{name: ident.clone()}),
                        TypeDecl::Alias{name: _, of, export: _} => Some(of.clone()),
                        TypeDecl::Type{name: _, inner: _, type_args, export: _, member_funcs: _, constructor: _, under_construction: _} => {
                            if type_args.len() > 0 {
                                let types = self.parse_type_args(&type_args, parser_context);
                                Some(Type::UserType{name: ident.clone(), type_args: types})
                            } else {
                                Some(Type::UserType{name: ident.clone(), type_args: vec![]})
                            }
                        },
                    }
                }
            }
        }
    }

    pub(crate) fn parse_type_from_keyword(&mut self, 
        keyword: &Keyword,
        loc: &SourceLocation,
        parser_context: &mut ParserContext,
    ) -> Type {
        match keyword {
            Keyword::Void => Type::RealVoid,
            Keyword::Bool => Type::Bool,
            Keyword::Unknown => Type::Unknown,
            Keyword::Never => Type::Never,
            Keyword::Number => Type::Number,
            Keyword::String => Type::String,
            Keyword::Array => {
                expect_punct!(self, parser_context, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                expect_punct!(self, parser_context, Punct::GreaterThan);
                Type::Array(Box::new(inner))
            },
            Keyword::UnsafeArray => {
                if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
                }
                expect_punct!(self, parser_context, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                expect_punct!(self, parser_context, Punct::GreaterThan);
                Type::UnsafeArray(Box::new(inner))
            },
            Keyword::Int => {
                let next = self.peek_next_item();
                if next.token.matches_punct(Punct::LessThan) {
                    let err_ret = Type::Int(S_32_MIN, S_32_MAX);
                    self.skip_next_item();
                    let next = expect_next!(self, parser_context, err_ret);
                    let token = next.token;
                    let lower = match token {
                        Token::Number(n) => {
                            match n.kind() {
                                NumberKind::Hex | NumberKind::DecI | NumberKind::Bin | NumberKind::Oct => {
                                    n.parse_i128().unwrap()
                                },
                                NumberKind::DecF => {
                                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                                    S_32_MIN
                                },
                            }
                        },
                        Token::Punct(Punct::Dash) => {
                            let next = expect_next!(self, parser_context, err_ret);
                            let token = next.token;
                            match token {
                                Token::Number(n) => {
                                    match n.kind() {
                                        NumberKind::Hex | NumberKind::DecI | NumberKind::Bin | NumberKind::Oct => {
                                            -1 * n.parse_i128().unwrap()
                                        },
                                        NumberKind::DecF => {
                                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                                            S_32_MIN
                                        },
                                    }
                                },
                                _ => {
                                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                                    S_32_MIN
                                }
                            }
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                            S_32_MIN
                        }
                    };
                    expect_punct!(self, parser_context, Punct::Comma);
                    let next = expect_next!(self, parser_context, err_ret);
                    let token = next.token;
                    let upper = match token {
                        Token::Number(n) => {
                            match n.kind() {
                                NumberKind::Hex | NumberKind::DecI | NumberKind::Bin | NumberKind::Oct => {
                                    n.parse_i128().unwrap()
                                },
                                NumberKind::DecF => {
                                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                                    S_32_MAX
                                },
                            }
                        },
                        Token::Punct(Punct::Dash) => {
                            let next = expect_next!(self, parser_context, err_ret);
                            let token = next.token;
                            match token {
                                Token::Number(n) => {
                                    match n.kind() {
                                        NumberKind::Hex | NumberKind::DecI | NumberKind::Bin | NumberKind::Oct => {
                                            -1 * n.parse_i128().unwrap()
                                        },
                                        NumberKind::DecF => {
                                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                                            S_32_MAX
                                        },
                                    }
                                },
                                _ => {
                                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                                    S_32_MAX
                                }
                            }
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), String::from("expecting integer constant")));
                            S_32_MAX
                        }
                    };
                    expect_punct!(self, parser_context, Punct::GreaterThan);
                    Type::Int(lower, upper)
                } else {
                    Type::Int(S_32_MIN, S_32_MAX)
                }
            },
            //Keyword::Tuple => Ok(Type::Tuple),
            //Keyword::Object => Ok(Type::Object),
            Keyword::Any => Type::Any,
            Keyword::UnsafePtr => {
                if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
                }
                Type::UnsafePtr
            },
            Keyword::Option => {
                expect_punct!(self, parser_context, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                expect_punct!(self, parser_context, Punct::GreaterThan);
                Type::Option(Box::new(inner))
            },
            Keyword::UnsafeOption => {
                if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
                }
                expect_punct!(self, parser_context, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                expect_punct!(self, parser_context, Punct::GreaterThan);
                Type::UnsafeOption(Box::new(inner))
            },
            Keyword::UnsafeSome => {
                if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
                }
                expect_punct!(self, parser_context, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                expect_punct!(self, parser_context, Punct::GreaterThan);
                Type::UnsafeSome(Box::new(inner))
            },
            
            _ => {
                parser_context.push_err(Error::InvalidTypeName(loc.clone(), keyword.as_str().to_owned()));
                Type::Undeclared
            }
        }
    }

    ///Parse a type, e.g. Int
    pub(crate) fn parse_type(&mut self, 
        parser_context: &mut ParserContext,
    ) -> Type {
        let err_ret = Type::Undeclared;
        let next = expect_next!(self, parser_context, err_ret);
        let token = next.token;
        match token {
            Token::Keyword(keyword) => self.parse_type_from_keyword(&keyword, &next.location, parser_context),
            Token::Ident(ident) => {
                self.parse_type_from_ident(&ident.to_string(), Commitment::Committed, parser_context).unwrap()
            },
            Token::Number(n) => {
                match n.kind() {
                    NumberKind::Hex | NumberKind::DecI | NumberKind::Bin | NumberKind::Oct => {
                        let number_i128 = n.parse_i128().unwrap();
                        return Type::Int(number_i128.into(), number_i128.into());
                    },
                    NumberKind::DecF => {
                        let number = n.parse_f64();
                        return Type::FloatLiteral(number.unwrap());
                    },
                }
            },

            Token::String(s) => Type::StringLiteral(s.to_string()),

            Token::UnsafeNull => {
                if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
                    parser_context.push_err(Error::UnsafeCodeNotAllowed(next.location.clone()));
                }
                Type::UnsafeNull
            },

            _ => {
                parser_context.push_err(Error::InvalidType(next.location.clone()));
                Type::Undeclared
            }
        }
    }

    pub(crate) fn parse_alias(&mut self, 
        export: bool,
        parser_context: &mut ParserContext,
    ) {
        self.skip_next_item();
        let next = self.peek_next_item();
        let id = expect_ident!(next, parser_context, "alias must have name");
        self.skip_next_item();
        expect_punct!(self, parser_context, Punct::Equal);
        let t = self.parse_type(parser_context);
        parser_context.type_map.insert(id.clone(), TypeDecl::Alias{name: id, of: t, export});
    }


    pub(crate) fn parse_type_decl(&mut self, 
        export: bool,
        phase: ParserPhase,
        parser_context: &mut ParserContext,
    ) -> () {
        self.skip_next_item();
        let next = self.peek_next_item();
        let id = expect_ident!(next, parser_context, "type must have name");
        self.skip_next_item();
        
        let mut next = self.peek_next_item();
        let mut token = &next.token;

        let mut generic = false;
        let type_args = if token.matches_punct(Punct::LessThan) {
            let type_args = self.parse_type_decl_args(parser_context);
            let type_args = parser_context.push_type_scope(&type_args);
            generic = true;
            next = self.peek_next_item();
            token = &next.token;
            type_args
        } else {
            vec![]
        };

        if token.matches_punct(Punct::Equal) {
            self.skip_next_item();
        } else {
            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), format!("Expected '{}", Punct::Equal.to_string())));
        }

        let inner = self.parse_type(parser_context);
        
        let this_type = Type::UserType{
            name: id.clone(), 
            type_args: type_args.iter().map(|type_arg| Type::VariableUsage{name: type_arg.name.clone(), constraint: type_arg.constraint.clone()}).collect()
        };

        let mut member_funcs = vec![
            MemberFunc{
                type_args: vec![], func_type: FuncType{out_type: inner.clone(), in_types: vec![this_type.clone()]}, 
                mangled_name: String::from("__getInner"), name: String::from("getInner"), privacy: Privacy::Private,
            },
            MemberFunc{
                type_args: vec![], func_type: FuncType{out_type: inner.clone(), in_types: vec![this_type.clone()]}, 
                mangled_name: String::from("__getInnerMut"), name: String::from("getInnerMut"), privacy: Privacy::Private,
            },
            MemberFunc{
                type_args: vec![], func_type: FuncType{out_type: Type::RealVoid, in_types: vec![inner.clone()]}, 
                mangled_name: String::from("__setInner"), name: String::from("setInner"), privacy: Privacy::Private,
            }
        ];
        parser_context.type_map.insert(id.clone(), TypeDecl::Type{name: id.clone(), inner: inner.clone(), type_args: type_args.clone(), export, 
            member_funcs: member_funcs.clone(), 
            constructor: None, under_construction: true});

        expect_punct!(self, parser_context, Punct::OpenBrace);

        let mut next = self.peek_next_item();
        let mut token = &next.token;
        
        let mut constructor: Option<MemberFunc> = None;
        let prefix = format!("!{}__", id);
        while !token.matches_punct(Punct::CloseBrace) {
            if token.is_eof() {
                parser_context.push_err(Error::UnexpectedEoF(next.location.clone(), String::from("expecting '}'")));
                break;
            }

            match token {
                Token::Keyword(Keyword::Fn) => {
                    let mf = self.parse_member_function_decl(&prefix, &this_type, &type_args, Privacy::Public, export, phase, parser_context);
                    parser_context.append_member_func(&id, &mf);
                    member_funcs.push(mf);
                },
                Token::Keyword(Keyword::Constructor) => {
                    if constructor.is_some() {
                        parser_context.push_err(Error::OnlyOneConstructor(next.location.clone()))
                    }
                    let constructor_member_func = self.parse_constructor_decl(&prefix, &type_args, export, phase, parser_context);
                    if constructor_member_func.func_type.out_type != inner {
                        parser_context.push_err(Error::TypeFailure(next.location.clone(), inner.clone(), constructor_member_func.func_type.out_type.clone()))
                    }
                    
                    constructor = Some(constructor_member_func);
                },
                Token::Keyword(Keyword::Private) => {
                    self.skip_next_item();
                    next = self.peek_next_item();
                    token = &next.token;
                    match token {
                        Token::Keyword(Keyword::Fn) => {
                            let mf = self.parse_member_function_decl(&prefix, &this_type, &type_args, Privacy::Private, export, phase, parser_context);
                            parser_context.append_member_func(&id, &mf);
                            member_funcs.push(mf);
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                            self.skip_next_item();
                        }
                    }
                }
                _ => {
                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                    self.skip_next_item();
                }
            }

            next = self.peek_next_item();
            token = &next.token;    
        }
        self.skip_next_item();

        if generic {
            parser_context.pop_type_scope();
        }

        parser_context.type_map.insert(id.clone(), TypeDecl::Type{name: id, inner, type_args, export, member_funcs: member_funcs, constructor: constructor, under_construction: false});
    }

    pub (crate) fn parse_type_member_function_call(&mut self, 
        this_expr: &TypedExpr,
        type_name: &String, 
        type_args: &Vec<Type>,
        func_name: &String,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let user_type = parser_context.get_type_decl(type_name);
        match user_type{
            Some(TypeDecl::Type{name: _, inner, type_args: _, export: _, member_funcs, constructor: _, under_construction: _}) => {
                let boo = member_funcs.clone();
                let o_member_func = boo.iter().find(|&x| x.name == *func_name);
                match o_member_func {
                    Some(member_func) => {
                        check_privacy(member_func.privacy, &this_expr.r#type, &member_func.name, &loc, parser_func_context, parser_context);
        
                        let mangled_name = &member_func.mangled_name;
                        match mangled_name.as_str(){
                            "__getInner" => {
                                self.parse_empty_function_call_args(parser_func_context, parser_context);
                                TypedExpr{expr: Expr::FreeUserTypeUnwrap(Box::new(this_expr.clone())), r#type: inner.clone(), is_const: true, loc: loc.clone()}
                            },
                            "__getInnerMut" => {
                                self.parse_empty_function_call_args(parser_func_context, parser_context);
                                TypedExpr{expr: Expr::FreeUserTypeUnwrap(Box::new(this_expr.clone())), r#type: inner.clone(), is_const: false, loc: loc.clone()}
                            },
                            "__setInner" => {
                                let args = self.parse_function_call_args(&None, &vec![inner.clone()], parser_func_context, parser_context);
                                let o_l_value = this_expr.as_l_value();
                                match o_l_value {
                                    None => {
                                        parser_context.push_err(Error::NotAnLValue(loc.clone()));
                                        TypedExpr{expr: Expr::NoOp, r#type: Type::Undeclared, is_const: true, loc: loc.clone()}
                                    },
                                    Some(l_value) => {
                                        TypedExpr{expr: Expr::Assignment(Box::new(this_expr.clone()), l_value, Box::new(args[0].clone())), 
                                            r#type: Type::RealVoid, is_const: true, loc: loc.clone()}
                                    }
                                }
                            },
                            _ => {
                                self.parse_member_function_call(this_expr, &type_args, &member_func, parser_func_context, parser_context)
                            }
                        }  
                    },
                    None => {
                        parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), this_expr.r#type.clone(), func_name.clone()));
                        TypedExpr{expr: Expr::NoOp, r#type: Type::Undeclared, loc: loc.clone(), is_const: true}
                    }
                }
            },
            _ => unreachable!()
        }        
    }
}