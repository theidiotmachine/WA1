use std::collections::{HashMap};

use crate::Parser;
use crate::{ParserContext, UnsafeParseMode, ParserPhase, ParserFuncContext};

use wa1_lexer::prelude::*;
use wa1_ast::prelude::*;
use wa1_types::prelude::*;
pub use wa1_errs::Error;
use wa1_errs::prelude::*;
use crate::{Commitment, expect_punct, expect_next, expect_ident, check_privacy, expect_keyword};
use crate::parser::parser_int::get_int_member_funcs;
use crate::parser::parser_unsafe::get_unsafe_ptr_member_funcs;
use crate::parser::parser_op::{BinaryOperatorData, Association};

pub (crate) fn matches_type_constraint(
    arg_type: &Type,
    constraint: &TypeConstraint,
    parser_context: &ParserContext,
) -> bool {
    match constraint {
        TypeConstraint::None => true,
        TypeConstraint::Intersection(inner) => {
            inner.iter().all(|x| matches_type_constraint(arg_type, x, parser_context))
        },
        TypeConstraint::Union(inner) => {
            inner.iter().any(|x| matches_type_constraint(arg_type, x, parser_context))
        },
        TypeConstraint::Trait(t) => {
            parser_context.type_implements_trait(arg_type, t)
        }
    }
}

fn constraint_has_member_funcs(
    constraint: &TypeConstraint,
    parser_context: &ParserContext,
) -> bool {
    match constraint{
        TypeConstraint::None => false,
        TypeConstraint::Union(inner) => inner.iter().any(|x| constraint_has_member_funcs(x, parser_context)),
        //this is actually not true, but it's a good first approximation
        TypeConstraint::Intersection(inner) => inner.iter().all(|x| constraint_has_member_funcs(x, parser_context)),
        TypeConstraint::Trait(t) => {
            let o_trait_decl = parser_context.trait_map.get(t);
            match o_trait_decl {
                Some(trait_decl) => trait_decl.member_funcs.len() > 0,
                None => false
            }
        }
    }
}

fn get_member_funcs_for_type_constraint(
    constraint: &TypeConstraint,
    parser_context: &ParserContext,
) -> Vec<TraitMemberFunc> {
    match constraint {
        TypeConstraint::None => vec![],
        TypeConstraint::Trait(t) => {
            let o_trait_decl = parser_context.trait_map.get(t);
            match o_trait_decl {
                Some(trait_decl) => trait_decl.member_funcs.clone(),
                None => vec![]
            }
        },
        //ugh this is horrible, it's like O(n^3) or something. I can't use a hashset because I can't figure out how to hash Type though
        TypeConstraint::Union(ts) => {
            let mut out: Vec<TraitMemberFunc> = vec![];
            for t in ts {
                let mfs = get_member_funcs_for_type_constraint(t, parser_context);
                for mf in mfs {
                    if !out.iter().any(|x| *x == mf) {
                        out.push(mf)
                    }
                }
            }
            out
        },
        //you thought union was bad this is actually worse
        TypeConstraint::Intersection(ts) => {
            let mut out: Vec<TraitMemberFunc> = vec![];
            for t in ts {
                let mfs = get_member_funcs_for_type_constraint(t, parser_context);
                for mf in mfs {
                    let o_idx = out.iter().position(|x| *x == mf);
                    match o_idx {
                        Some(idx) => {
                            out.remove(idx);
                        },
                        None => {
                            out.push(mf)
                        }
                    }
                }
            }
            out
        }
    }
}

pub (crate) fn get_runtime_type_for_generic(
    arg_type: &Type,
    constraint: &TypeConstraint,
    parser_context: &ParserContext,
) -> Option<Type> {
    if *constraint != TypeConstraint::None && constraint_has_member_funcs(constraint, parser_context) {
        return Some(arg_type.clone());
    }
    match arg_type {
        Type::Bool => Some(Type::Int(0, U_32_MAX)),
        Type::FloatLiteral(_) => Some(Type::Number),
        Type::Int(lower, upper) => {
            let bittage = get_bittage(*lower, *upper);
            match bittage{
                Bittage::S32 | Bittage::U32 => Some(Type::Int(0, U_32_MAX)),
                Bittage::S64 | Bittage::U64 => Some(Type::Int(0, U_64_MAX)),
                Bittage::OOR => None
            }
        },
        Type::Number => Some(Type::Number),
        Type::RealVoid => Some(Type::RealVoid),
        Type::UnsafeOption(_) => Some(Type::Int(0, PTR_MAX)),
        Type::UnsafePtr => Some(Type::Int(0, PTR_MAX)),
        Type::UnsafeStruct{name: _} => Some(Type::Int(0, PTR_MAX)),
        _ => None
    }
}

fn substitute_trait_type(trait_name: &String, this_type: &Type, t: &Type) -> Type {
    match t {
        Type::Any | Type::Bool | Type::FakeVoid | Type::FloatLiteral(_) | Type::Int(_,_) | Type::ModuleLiteral(_) | Type::Never | Type::Number 
            | Type::ObjectLiteral(_) | Type::RealVoid | Type::String | Type::StringLiteral(_) | Type::Undeclared | Type::Unknown |Type::UnsafeNull 
            | Type::UnsafePtr | Type::UnsafeStruct{name: _}
            => t.clone(),
        Type::Array(inner) => Type::Array(Box::new(substitute_trait_type(trait_name, this_type, inner))),
        Type::Func{func_type} => Type::Func{func_type: Box::new(substitute_trait_func_type(trait_name, this_type, func_type))},
        Type::Option(inner) => Type::Option(Box::new(substitute_trait_type(trait_name, this_type, inner))),
        Type::Some(inner) => Type::Some(Box::new(substitute_trait_type(trait_name, this_type, inner))),
        Type::Tuple(inner) => Type::Tuple(inner.iter().map(|x| substitute_trait_type(trait_name, this_type, &x)).collect()),
        Type::TypeLiteral(inner) => Type::TypeLiteral(Box::new(FullType::new(&substitute_trait_type(trait_name, this_type, &inner.r#type), inner.mutability))),
        Type::UnsafeArray(inner) => Type::UnsafeArray(Box::new(substitute_trait_type(trait_name, this_type, inner))),
        Type::UnsafeOption(inner) => Type::UnsafeOption(Box::new(substitute_trait_type(trait_name, this_type, inner))),
        Type::UnsafeSome(inner) => Type::UnsafeSome(Box::new(substitute_trait_type(trait_name, this_type, inner))),
        Type::UserType{name, type_args, inner} => 
            Type::UserType{name: name.clone(), type_args: type_args.iter().map(|x| substitute_trait_type(trait_name, this_type, &x)).collect(), inner: inner.clone()},
        Type::VariableUsage{name, constraint: _} => {
            if name == trait_name {
                this_type.clone()
            } else {
                t.clone()
            }
        }
    }
}

pub (crate) fn substitute_trait_func_type(trait_name: &String, this_type: &Type, func_type: &FuncType) -> FuncType {
    let out_type = FullType::new(&substitute_trait_type(trait_name, this_type, &func_type.out_type.r#type), func_type.out_type.mutability);
    let in_types = func_type.in_types.iter().map(|x| FullType::new(&substitute_trait_type(trait_name, this_type, &x.r#type), x.mutability)).collect();
    FuncType{out_type, in_types}
}

pub fn get_member_funcs(t: &Type, type_map: &HashMap<String, TypeDecl>) -> Vec<MemberFunc> {
    match t {
        Type::Any => vec![],
        Type::Array(_) => vec![],
        Type::Bool => vec![],
        Type::FakeVoid => vec![],
        Type::FloatLiteral(_) => vec![],
        Type::Func{func_type: _} => vec![],
        Type::Int(lower, upper) => get_int_member_funcs(*lower, *upper),
        Type::ModuleLiteral(_) => vec![],
        Type::Never => vec![],
        Type::Number => vec![],
        Type::ObjectLiteral(_) => vec![],
        Type::Option(_) => vec![],
        Type::RealVoid => vec![],
        Type::Some(_) => vec![],
        Type::String => vec![],
        Type::StringLiteral(_) => vec![],
        Type::Tuple(_) => vec![],
        Type::TypeLiteral(_) => vec![],
        Type::Undeclared => vec![],
        Type::Unknown => vec![],
        Type::UnsafeArray(_) => vec![],
        Type::UnsafeOption(_) => vec![],
        Type::UnsafeNull => vec![],
        Type::UnsafePtr => get_unsafe_ptr_member_funcs(),
        Type::UnsafeSome(_) => vec![],
        Type::UnsafeStruct{name: _} => vec![],
        Type::UserType{name, type_args: _, inner: _} => {
            let type_decl = type_map.get(name).unwrap();
            type_decl.get_member_funcs()
        },
         Type::VariableUsage{name: _, constraint: _} => vec![],
    }
}

fn get_type_decl_binary_operator_data<'a>(token: &Token<&'a str>) -> Option<BinaryOperatorData> {
    match token {
        Token::Punct(Punct::Pipe) => Some(BinaryOperatorData{precedence: 7, association: Association::Left, is_member: false}),
        Token::Punct(Punct::Ampersand) => Some(BinaryOperatorData{precedence: 9, association: Association::Left, is_member: false}),
        _ => None
    }
}

impl<'a> Parser<'a> {
    fn parse_type_decl_constraint_primary(&mut self,
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        let err_out = TypeConstraint::None;
        let next = expect_next!(self, parser_context, err_out);
        let loc = next.location.clone();
        let token = &next.token;
        match token {
            Token::Ident(id) => {
                let o_trait_decl = parser_context.trait_map.get(&id.to_string());
                match o_trait_decl {
                    Some(_) => TypeConstraint::Trait(id.to_string()),
                    None => {
                        parser_context.push_err(Error::UnrecognizedTypeArgConstraint(loc));
                        TypeConstraint::None
                    },
                }
            },
            _ => {
                parser_context.push_err(Error::UnrecognizedTypeArgConstraint(loc));
                err_out
            }
        }
    }

    fn parse_type_decl_binary_op(&mut self, 
        op: &Token<&str>,
        lhs: &TypeConstraint, 
        rhs: &TypeConstraint, 
        loc: &SourceLocation,
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        match op{
            //intersection - has all of the members
            Token::Punct(Punct::Ampersand) => {
                TypeConstraint::Intersection(vec![lhs.clone(), rhs.clone()])
            },
            //union - has the intersection of members
            Token::Punct(Punct::Pipe) => {
                TypeConstraint::Union(vec![lhs.clone(), rhs.clone()])
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(loc.clone(), format!("expected | or &")));
                TypeConstraint::None
            }
        }
    }

    fn parse_type_decl_constraint_1(&mut self, 
        init_lhs: &TypeConstraint, 
        min_precedence: i32,
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        let mut lhs = init_lhs.clone();
        let lookahead_item = self.peek_next_item();
        let mut lookahead = lookahead_item.token;
        let lookahead_loc = lookahead_item.location;
        
        // while lookahead is a binary operator whose precedence is >= min_precedence
        loop {
            let o_lookahead_data = get_type_decl_binary_operator_data(&lookahead);
            match o_lookahead_data {
                None => break,
                Some(lookahead_data) => {
                    if lookahead_data.precedence >= min_precedence {
                        // op := lookahead
                        let op = lookahead;
                        let op_precedence = lookahead_data.precedence;
                        // advance to next token
                        self.skip_next_item();
                        // rhs := parse_primary ()
                        let r_rhs = self.parse_type_decl_constraint_primary(parser_context);
                        let mut rhs = r_rhs;
                        // lookahead := peek next token
                        let lookahead_item = self.peek_next_item();
                        lookahead = lookahead_item.token;
                        // while lookahead is a binary operator whose precedence is greater
                        // than op's, or a right-associative operator
                        // whose precedence is equal to op's
                        loop {
                            let o_lookahead_data = get_type_decl_binary_operator_data(&lookahead);
                            match o_lookahead_data {
                                None => break,
                                Some(lookahead_data) => {
                                    if lookahead_data.precedence > op_precedence || (lookahead_data.association == Association::Right && lookahead_data.precedence == op_precedence) {
                                        //rhs := parse_expression_1 (rhs, lookahead's precedence)
                                        let new_rhs = self.parse_type_decl_constraint_1(&rhs, lookahead_data.precedence, parser_context);
                                        rhs = new_rhs;
                                        //lookahead := peek next token
                                        let lookahead_item = self.peek_next_item();
                                        lookahead = lookahead_item.token;
                                    } else {
                                        break;
                                    }
                                }
                            }                                
                        }
                        lhs = self.parse_type_decl_binary_op(&op, &lhs, &rhs, &lookahead_loc, parser_context);
                    } else {
                        break;
                    }
                }
            }
        };
        return lhs.clone();
    }

    pub (crate) fn parse_type_decl_constraint(&mut self, 
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        let primary = self.parse_type_decl_constraint_primary(parser_context);
        self.parse_type_decl_constraint_1(&primary, 0, parser_context)
    }

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
            if !matches_type_constraint(&arg_type, &(type_args[idx].constraint), parser_context) {
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
                        TypeDecl::Type{name: _, inner, type_args, export: _, member_funcs: _, constructor: _, under_construction: _} => {
                            if type_args.len() > 0 {
                                let types = self.parse_type_args(&type_args, parser_context);
                                Some(Type::UserType{name: ident.clone(), type_args: types, inner: Box::new(inner)})
                            } else {
                                Some(Type::UserType{name: ident.clone(), type_args: vec![], inner: Box::new(inner)})
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

    ///Parse a type with optional mutability qualifier, e.g. mut Int
    pub(crate) fn parse_full_type(&mut self, 
        parser_context: &mut ParserContext,
    ) -> FullType {
        let next = self.peek_next_item();
        let token = &next.token;
        let mut mutability = Mutability::Const;
        let mut_loc = next.location.clone();
        if token.matches_keyword(Keyword::Mut) {
            mutability = Mutability::Mut;
            self.skip_next_item();
        }
        let t = self.parse_type(parser_context);
        if t.get_pass_style() == PassStyle::AtomicValue && mutability == Mutability::Mut{
            parser_context.push_err(Error::UnnecessaryMut(mut_loc, t.clone()));
            mutability = Mutability::Const;
        }
        FullType::new(&t, mutability)
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

    ///Parse a `type`, which is WA1 for an encapsulated alias plus member functions.
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
            type_args: type_args.iter().map(|type_arg| Type::VariableUsage{name: type_arg.name.clone(), constraint: type_arg.constraint.clone()}).collect(),
            inner: Box::new(inner.clone())
        };

        let mut member_funcs = vec![
            MemberFunc{
                type_args: vec![], func_type: FuncType{out_type: FullType::new_const(&inner), in_types: vec![FullType::new_const(&this_type)]}, 
                mangled_name: String::from("__getInner"), name: String::from("getInner"), privacy: Privacy::Private,
            },
        ];
        if inner.get_pass_style() == PassStyle::Reference {
            member_funcs.push(
                MemberFunc{
                    type_args: vec![], func_type: FuncType{out_type: FullType::new_mut(&inner), in_types: vec![FullType::new_mut(&this_type)]},
                    mangled_name: String::from("__getInnerMut"), name: String::from("getInnerMut"), privacy: Privacy::Private,
                }
            )
        }

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
                    let mf = self.parse_member_function_decl(&prefix, &FullType::new_const(&this_type), &type_args, Privacy::Public, export, phase, parser_context);
                    parser_context.append_type_decl_member_func(&id, &mf);
                    member_funcs.push(mf);
                },
                Token::Keyword(Keyword::Mut) => {
                    self.skip_next_item();
                    next = self.peek_next_item();
                    token = &next.token;
                    match token {
                        Token::Keyword(Keyword::Fn) => {
                            let mf = self.parse_member_function_decl(&prefix, &FullType::new_mut(&this_type), &type_args, Privacy::Public, export, phase, parser_context);
                            parser_context.append_type_decl_member_func(&id, &mf);
                            member_funcs.push(mf);
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                            self.skip_next_item();
                        }
                    }
                },
                Token::Keyword(Keyword::Constructor) => {
                    if constructor.is_some() {
                        parser_context.push_err(Error::OnlyOneConstructor(next.location.clone()))
                    }
                    let constructor_member_func = self.parse_constructor_decl(&prefix, &type_args, export, phase, parser_context);
                    if constructor_member_func.func_type.out_type.r#type != inner {
                        parser_context.push_err(Error::TypeFailure(next.location.clone(), FullType::new_mut(&inner), constructor_member_func.func_type.out_type.clone()))
                    }
                    
                    constructor = Some(constructor_member_func);
                },

                Token::Keyword(Keyword::Private) => {
                    self.skip_next_item();
                    next = self.peek_next_item();
                    token = &next.token;
                    match token {
                        Token::Keyword(Keyword::Fn) => {
                            let mf = self.parse_member_function_decl(&prefix, &FullType::new_const(&this_type), &type_args, Privacy::Private, export, phase, parser_context);
                            parser_context.append_type_decl_member_func(&id, &mf);
                            member_funcs.push(mf);
                        },
                        Token::Keyword(Keyword::Mut) => {
                            self.skip_next_item();
                            next = self.peek_next_item();
                            token = &next.token;
                            match token {
                                Token::Keyword(Keyword::Fn) => {
                                    let mf = self.parse_member_function_decl(&prefix, &FullType::new_mut(&this_type), &type_args, Privacy::Private, export, phase, parser_context);
                                    parser_context.append_type_decl_member_func(&id, &mf);
                                    member_funcs.push(mf);
                                },
                                _ => {
                                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                                    self.skip_next_item();
                                }
                            }
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

    ///Given the constraints on a type variable, parse a member function call.
    pub (crate) fn parse_type_variable_member_function_call(&mut self, 
        this_expr: &TypedExpr,
        constraint: &TypeConstraint,
        func_name: &String,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let member_funcs = get_member_funcs_for_type_constraint(constraint, parser_context);
        let o_member_func = member_funcs.iter().find(|&x| x.name == *func_name);
        match o_member_func {
            Some(member_func) => {
                self.parse_trait_member_function_call(this_expr, &member_func, loc, parser_func_context, parser_context)
            }
            None => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), this_expr.r#type.r#type.clone(), func_name.clone()));
                TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::Undeclared), loc: loc.clone()}
            }
        }     
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
                        check_privacy(member_func.privacy, &this_expr.r#type.r#type, &member_func.name, &loc, parser_func_context, parser_context);
        
                        let mangled_name = &member_func.mangled_name;
                        match mangled_name.as_str(){
                            "__getInner" => {
                                self.parse_empty_function_call_args(parser_func_context, parser_context);
                                TypedExpr{expr: Expr::FreeUserTypeUnwrap(Box::new(this_expr.clone())), r#type: FullType::new(&inner, Mutability::Const), loc: loc.clone()}
                            },
                            "__getInnerMut" => {
                                self.parse_empty_function_call_args(parser_func_context, parser_context);
                                TypedExpr{expr: Expr::FreeUserTypeUnwrap(Box::new(this_expr.clone())), r#type: FullType::new(&inner, Mutability::Mut), loc: loc.clone()}
                            },
                            _ => {
                                self.parse_member_function_call(this_expr, &type_args, &member_func, parser_func_context, parser_context)
                            }
                        }  
                    },
                    None => {
                        parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), this_expr.r#type.r#type.clone(), func_name.clone()));
                        TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::Undeclared), loc: loc.clone()}
                    }
                }
            },
            _ => unreachable!()
        }        
    }

    ///parse a `trait`, which is a reduced type class
    pub (crate) fn parse_trait_decl(&mut self, 
        export: bool,
        parser_context: &mut ParserContext,
    ) -> () {
        self.skip_next_item();
        let next = self.peek_next_item();
        let id = expect_ident!(next, parser_context, "trait must have name");
        self.skip_next_item();
     
        let mut member_funcs = vec![];

        expect_punct!(self, parser_context, Punct::OpenBrace);

        let mut next = self.peek_next_item();
        let mut token = &next.token;
        let this_type = Type::VariableUsage{name: id.clone(), constraint: TypeConstraint::Trait(id.clone())};
        parser_context.push_trait_type_scope(&TypeArg{name: id.clone(), constraint: TypeConstraint::Trait(id.clone())});

        while !token.matches_punct(Punct::CloseBrace) {
            if token.is_eof() {
                parser_context.push_err(Error::UnexpectedEoF(next.location.clone(), String::from("expecting '}'")));
                break;
            }

            match token {
                Token::Keyword(Keyword::Fn) => {
                    let mf = self.parse_trait_func_decl_header(&FullType::new_const(&this_type), &id, parser_context);
                    member_funcs.push(mf);
                },
                Token::Keyword(Keyword::Mut) => {
                    self.skip_next_item();
                    next = self.peek_next_item();
                    token = &next.token;
                    match token {
                        Token::Keyword(Keyword::Fn) => {
                            let mf = self.parse_trait_func_decl_header(&FullType::new_mut(&this_type), &id, parser_context);
                            member_funcs.push(mf);
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                            self.skip_next_item();
                        }
                    }
                },
                _ => {
                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                    self.skip_next_item();
                }
            }

            next = self.peek_next_item();
            token = &next.token;
        }

        self.skip_next_item();
        parser_context.pop_type_scope();

        parser_context.trait_map.insert(id.clone(), TraitDecl{name: id, export, member_funcs});
    }

    pub (crate) fn parse_trait_impl(&mut self, 
        export: bool,
        phase: ParserPhase,
        parser_context: &mut ParserContext,
    ) -> () {
        self.skip_next_item();
        let next = self.peek_next_item();
        let trait_loc = self.peek_next_location();

        //get the name and the type args
        let trait_name = expect_ident!(next, parser_context, "trait must be named");

        self.skip_next_item();
        let next = self.peek_next_item();
        let token = &next.token;
        let mut generic = false;
        let type_args = if token.matches_punct(Punct::LessThan) {
            let type_args = self.parse_type_decl_args(parser_context);
            let type_args = parser_context.push_type_scope(&type_args);
            generic = true;
            type_args
        } else {
            vec![]
        };
        
        expect_keyword!(self, parser_context, Keyword::For);

        //now parse the type we are implementing
        let loc = self.peek_next_location();
        let this_type = self.parse_type(parser_context);
        let type_name = this_type.get_type_name();
        let type_type_args = this_type.get_type_args();
        let type_member_funcs = get_member_funcs(&this_type, &parser_context.type_map);
        match &this_type {
            Type::UserType{name:_, type_args: _, inner: _} => {},
            _ => {
                parser_context.push_err(Error::CantImplementTraitFor(loc, this_type.clone()));
            }
        }
        
        //ok, now get the expected member functions by loading the trait
        let o_trait_decl = parser_context.trait_map.get(&trait_name);
        let mut required_member_funcs = match o_trait_decl {
            Some(trait_decl) => {
                trait_decl.member_funcs.iter().map(|x| {
                    TraitMemberFunc{
                        name: x.name.clone(), 
                        type_args: x.type_args.clone(), 
                        func_type: substitute_trait_func_type(&trait_name, &this_type, &x.func_type), 
                        trait_name: x.trait_name.clone()
                }
                }).collect()
            },
            None => {
                parser_context.push_err(Error::UnrecognizedTrait(trait_loc, trait_name.clone()));
                vec![]
            }
        };

        //right now these have to match
        if type_type_args.len() != type_args.len() {
            parser_context.push_err(Error::WrongNumberOfTypeArgs(loc));
        }

        //filter out what the type gives us for free (yes this is O(n^2))
        for tmf in type_member_funcs {
            required_member_funcs.retain(|x| {
                x.name != tmf.name && x.func_type.out_type != tmf.func_type.out_type && x.func_type.in_types != tmf.func_type.in_types
            });
        }

        let mut member_funcs = vec![];

        expect_punct!(self, parser_context, Punct::OpenBrace);

        let prefix = format!("!{}__!{}", trait_name, type_name);

        let mut next = self.peek_next_item();
        let mut token = &next.token;
        while !token.matches_punct(Punct::CloseBrace) {
            if token.is_eof() {
                parser_context.push_err(Error::UnexpectedEoF(next.location.clone(), String::from("expecting '}'")));
                break;
            }

            match token {
                Token::Keyword(Keyword::Fn) => {
                    let mf = self.parse_member_function_decl(&prefix, &FullType::new_const(&this_type), &type_args, Privacy::Public, export, phase, parser_context);
                    required_member_funcs.retain(|x| {
                        x.name != mf.name && x.func_type.out_type != mf.func_type.out_type && x.func_type.in_types != mf.func_type.in_types
                    });
                    parser_context.append_type_decl_member_func(&type_name, &mf);
                    member_funcs.push(mf);
                },
                Token::Keyword(Keyword::Mut) => {
                    self.skip_next_item();
                    next = self.peek_next_item();
                    token = &next.token;
                    match token {
                        Token::Keyword(Keyword::Fn) => {
                            let mf = self.parse_member_function_decl(&prefix, &FullType::new_mut(&this_type), &type_args, Privacy::Public, export, phase, parser_context);
                            required_member_funcs.retain(|x| {
                                x.name != mf.name && x.func_type.out_type != mf.func_type.out_type && x.func_type.in_types != mf.func_type.in_types
                            });
                            parser_context.append_type_decl_member_func(&type_name, &mf);
                            member_funcs.push(mf);
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                            self.skip_next_item();
                        }
                    }
                },
                _ => {
                    parser_context.push_err(Error::UnexpectedToken(next.location.clone(), token.to_string()));
                    self.skip_next_item();
                }
            }

            next = self.peek_next_item();
            token = &next.token;
        }

        if generic {
            parser_context.pop_type_scope();
        }

        self.skip_next_item();

        //now check we have everything
        for rmf in required_member_funcs {
            parser_context.push_err(Error::MemberNotImplemented(loc.clone(), rmf.name.clone()));
        }

        parser_context.trait_impl_map.insert((type_name, trait_name.clone()), TraitImpl{trait_name, for_type: this_type, export, member_funcs});
    }
}