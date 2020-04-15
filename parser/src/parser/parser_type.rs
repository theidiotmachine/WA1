use crate::Parser;
use crate::{ParserContext, UnsafeParseMode};

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use errs::prelude::*;
use crate::{Commitment, expect_punct, expect_next};

impl<'a> Parser<'a> {
    pub(crate) fn parse_type_from_ident(&mut self, 
        ident: &String,
        commitment: Commitment,
        parser_context: &mut ParserContext,
    ) -> Option<Type> {
        let o_type = parser_context.type_map.get(ident);
        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_punct(Punct::LessThan) {
            if commitment == Commitment::Committed {
                parser_context.errors.push(Error::NotYetImplemented(next.location.clone(), String::from("type args")));
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
                        TypeDecl::Class{name: _, class_type: _, export: _} => Some(Type::UserClass{name: ident.clone()}),
                        TypeDecl::Struct{name: _, struct_type: _, under_construction: _, export: _} => Some(Type::UnsafeStruct{name: ident.clone()}),
                        TypeDecl::Alias{name: _, of, export: _} => Some(of.clone())
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
            Keyword::Boolean => Type::Boolean,
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
                parser_context.push_err(Error::InvalidType(self.current_position));
                Type::Undeclared
            }
        }
    }
}