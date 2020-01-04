use crate::Parser;
use crate::ParserContext;
use crate::Res;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use crate::assert_punct;
use crate::assert_ok;
use crate::assert_next;

impl<'a> Parser<'a> {
    pub(crate) fn parse_type_from_ident(&mut self, 
        ident: &Ident<&str>,
        parser_context: &mut ParserContext,
    ) -> Res<Type> {
        let o_type = parser_context.type_map.get(&ident.to_string());
        let next = self.peek_next_item();
        let token = &next.token;
        let type_args = vec![];
        if token.matches_punct(Punct::LessThan) {
            Err(Error::NotYetImplemented(next.location.clone(), String::from("type args")))
        } else {
            match o_type {
                None => Err(Error::InvalidTypeName(next.location.clone(), ident.as_str().to_owned())),
                Some(user_type) => {
                    match user_type {
                        UserType::Class(_) => {
                            Ok(Type::UserClass{name: ident.to_string(), type_args})
                        },
                        UserType::Struct{struct_type: _, under_construction: _} => {
                            Ok(Type::UnsafeUserStruct{name: ident.to_string()})
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn parse_type_from_keyword(&mut self, 
        keyword: &Keyword,
        loc: &SourceLocation,
        parser_context: &mut ParserContext,
    ) -> Res<Type> {
        match keyword {
            Keyword::Void => Ok(Type::RealVoid),
            Keyword::Boolean => Ok(Type::Boolean),
            Keyword::Unknown => Ok(Type::Unknown),
            Keyword::Never => Ok(Type::Never),
            Keyword::Number => Ok(Type::Number),
            Keyword::String => Ok(Type::String),
            Keyword::Array => {
                assert_punct!(self, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                assert_ok!(inner);
                assert_punct!(self, Punct::GreaterThan);
                Ok(Type::Array(Box::new(inner)))
            },
            Keyword::UnsafeStaticArray => {
                if !parser_context.is_unsafe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
                }
                assert_punct!(self, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                assert_ok!(inner);
                assert_punct!(self, Punct::Comma);
                let sz_type = self.parse_type(parser_context);
                assert_ok!(sz_type);
                assert_punct!(self, Punct::GreaterThan);
                match sz_type {
                    Type::IntLiteral(sz) => Ok(Type::UnsafeStaticArray(Box::new(inner), sz)),
                    _ => Err(Error::InvalidTypeName(loc.clone(), keyword.as_str().to_owned()))
                }
            },
            Keyword::BigInt => Ok(Type::BigInt),
            Keyword::Int => Ok(Type::Int),
            //Keyword::Tuple => Ok(Type::Tuple),
            //Keyword::Object => Ok(Type::Object),
            Keyword::Any => Ok(Type::Any),
            Keyword::UnsafePtr => {
                if !parser_context.is_unsafe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
                }
                Ok(Type::UnsafePtr)
            },
            Keyword::UnsafeSizeT => {
                if !parser_context.is_unsafe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc.clone()));
                }
                Ok(Type::UnsafeSizeT)
            },
            Keyword::Option => {
                assert_punct!(self, Punct::LessThan);
                let inner = self.parse_type(parser_context);
                assert_ok!(inner);
                assert_punct!(self, Punct::GreaterThan);
                Ok(Type::Option(Box::new(inner)))
            },
            _ => Err(Error::InvalidTypeName(loc.clone(), keyword.as_str().to_owned()))
        }
    }

    pub(crate) fn parse_type(&mut self, 
        parser_context: &mut ParserContext,
    ) -> Res<Type> {
        let next = assert_next!(self, "expecting type");
        let token = next.token;
        match token {
            Token::Keyword(keyword) => self.parse_type_from_keyword(&keyword, &next.location, parser_context),
            Token::Ident(ident) => self.parse_type_from_ident(&ident, parser_context),
            Token::Number(n) => {
                match n.kind() {
                    NumberKind::Hex => {
                        let number_i64 = n.parse_i64().unwrap();
                        if number_i64 > std::i32::MAX.into() || number_i64 < std::i32::MIN.into() {
                            return Ok(Type::BigIntLiteral(number_i64));
                        } else {
                            let number_i32 = n.parse_i32().unwrap();
                            return Ok(Type::IntLiteral(number_i32));
                        }
                    },
                    NumberKind::DecI => {
                        let number_i64 = n.parse_i64().unwrap();
                        if number_i64 > std::i32::MAX.into() || number_i64 < std::i32::MIN.into() {
                            return Ok(Type::BigIntLiteral(number_i64));
                        } else {
                            let number_i32 = n.parse_i32().unwrap();
                            return Ok(Type::IntLiteral(number_i32));
                        }
                    },
                    NumberKind::Bin => {
                        let number_i64 = n.parse_i64().unwrap();
                        if number_i64 > std::i32::MAX.into() || number_i64 < std::i32::MIN.into() {
                            return Ok(Type::BigIntLiteral(number_i64));
                        } else {
                            let number_i32 = n.parse_i32().unwrap();
                            return Ok(Type::IntLiteral(number_i32));
                        }                    
                    },
                    NumberKind::Oct => {
                        let number_i64 = n.parse_i64().unwrap();
                        if number_i64 > std::i32::MAX.into() || number_i64 < std::i32::MIN.into() {
                            return Ok(Type::BigIntLiteral(number_i64));
                        } else {
                            let number_i32 = n.parse_i32().unwrap();
                            return Ok(Type::IntLiteral(number_i32));
                        }
                    },
                    NumberKind::DecF => {
                        let number = n.parse_f64();
                        return Ok(Type::FloatLiteral(number.unwrap()));
                    },
                }
            },

            Token::String(s) => Ok(Type::StringLiteral(s.to_string())),

            _ => Err(Error::InvalidType(self.current_position))
        }
    }
}