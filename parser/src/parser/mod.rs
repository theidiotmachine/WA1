extern crate ress;
extern crate log;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;

pub use errs::Error;

use std::{mem::replace};

use std::collections::HashMap;

mod parser_unsafe;

use crate::ParserContext;
use crate::ParserFuncContext;
use crate::UserType;
use crate::Res;
use crate::assert_punct;
use crate::assert_ok;

macro_rules! assert_semicolon {
    ($s:ident) => (
        let next = $s.next_item()?;
        if !next.token.matches_punct(Punct::SemiColon) {
            return $s.expected_token_error(&next, &[&";"]);
        }
    )
}

/// Get the next token, returning if it is not ok. Usage: `let next = assert_next!(self);`
macro_rules! assert_next {
    ($s:ident, $m:expr) => ({let next = $s.next_item(); if next.is_err() { return Err(Error::UnexpectedEoF($m.to_string())); }; next?} )
}

macro_rules! assert_ident {
    ($n:ident, $m:expr) => (
        {
            let token = $n.token; 
            match token {
                Token::Ident(i) => i, 
                _ => return Err(Error::UnexpectedToken($n.location.clone(), $m.to_string())) 
            }
        }
    )
}

fn cast_int_to_number(expr: &TypedExpr) -> TypedExpr {
    TypedExpr{expr: Expr::IntToNumber(Box::new(expr.clone())), r#type: Type::Number, is_const: true, loc: expr.loc}
}

fn cast_int_to_bigint(expr: &TypedExpr) -> TypedExpr {
    TypedExpr{expr: Expr::IntToBigInt(Box::new(expr.clone())), r#type: Type::BigInt, is_const: true, loc: expr.loc}
}

fn free_type_widen(expr: &TypedExpr, to: &Type) -> TypedExpr {
    TypedExpr{expr: Expr::FreeTypeWiden(Box::new(expr.clone())), r#type: to.clone(), is_const: true, loc: expr.loc}
}

fn create_cast(want: &Type, got: &TypedExpr, cast: &TypeCast) -> Option<TypedExpr> {
    match cast {
        TypeCast::FreeWiden => Some(free_type_widen(got, want)),
        TypeCast::IntToBigIntWiden => Some(cast_int_to_bigint(got)),
        TypeCast::IntToNumberWiden => Some(cast_int_to_number(got)),
        TypeCast::None => None,
        TypeCast::NotNeeded => Some(got.clone()),
    }
}

fn try_cast(want: &Type, got: &TypedExpr) -> Option<TypedExpr> {
    let type_cast = types::cast::try_cast(&got.r#type, want);
    create_cast(want, got, &type_cast)
}

#[derive(Debug, Clone, PartialEq)]
enum ScopedVar{
    /// Actual local variable
    Local{
        internal_name: String,
        r#type: Type,
        constant: bool,
    },
    /// Closure reference. Looks like a local, actually a member of the function's closure
    ClosureRef{
        internal_name: String,
        r#type: Type,
        constant: bool,
    },
}

struct Scope{
    pub var_names: HashMap<String, ScopedVar>,
}

impl<> Default for Scope<> {
    fn default() -> Self {
        Self {
            var_names: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ScopedTypeVar{
    pub internal_name: String,
}

struct TypeScope{
    pub var_names: HashMap<String, ScopedTypeVar>,
}

impl<> Default for TypeScope<> {
    fn default() -> Self {
        Self {
            var_names: HashMap::new(),
        }
    }
}

/// The current parsing context.
/// This structure holds the relevant
/// information to know when some
/// text might behave differently
/// depending on what has come before it
struct Context<> {
    /// If we have entered a loop block
    in_iteration: bool,
    /// If the scanner has a pending line terminator
    /// before the next token
    has_line_term: bool,
    /// local variables
    var_stack: Vec<Scope>,
    /// type variables
    type_var_stack: Vec<TypeScope>,
    /// Uniqueness counter
    counter: u64,
}

impl Context {
    fn push_empty_scope(&mut self) {
        self.var_stack.push(Scope::default());
    }

    fn push_block_scope(&mut self) {
        let o_head = self.var_stack.last();
        match o_head {
            None => self.push_empty_scope(),
            Some(head) => {
                let mut new_var_names: HashMap<String, ScopedVar> = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    new_var_names.insert(key.clone(), val.clone());
                }

                self.var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    fn push_func_scope(&mut self) {
        let o_head = self.var_stack.last();
        match o_head {
            None => self.push_empty_scope(),
            Some(head) => {
                let mut new_var_names: HashMap<String, ScopedVar> = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    let new_val = match val {
                        ScopedVar::Local{internal_name, r#type, constant} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), constant: *constant, r#type: r#type.clone()},
                        ScopedVar::ClosureRef{internal_name, r#type, constant} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), constant: *constant, r#type: r#type.clone()},
                    };
                    new_var_names.insert(key.clone(), new_val);
                }

                self.var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    fn pop_scope(&mut self) {
        self.var_stack.pop();
    }

    fn add_var(&mut self, var_name: &String, internal_var_name: &String, r#type: Type, constant: bool,) {
        let o_head = self.var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type, constant: constant});
            }
        }
    }

    fn get_scoped_var(&mut self, var_name: &String) -> Option<&ScopedVar> {
        match self.var_stack.last() {
            None => None,
            Some(s) => {
                s.var_names.get(var_name)
            } 
        }
    }

    fn push_func_type_scope(&mut self, args: &Vec<String>) {
        let mut v: HashMap<String, ScopedTypeVar> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };
        
        for arg in args {
            v.insert(arg.clone(), ScopedTypeVar{internal_name: self.get_unique_name(arg)});
        }
        self.type_var_stack.push(TypeScope{var_names: v});
    }

    fn push_empty_func_type_scope(&mut self) {
        let v: HashMap<String, ScopedTypeVar> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };
        
        self.type_var_stack.push(TypeScope{var_names: v});
    }

    fn pop_type_scope(&mut self) {
        self.type_var_stack.pop();
    }

    fn get_unique_name(&mut self, name: &String) -> String {
        let counter = self.counter;
        self.counter += 1;
        format!("{}#{}", name, counter)
    }
}

impl<> Default for Context<> {
    fn default() -> Self {
        Self {
            in_iteration: false,
            has_line_term: false,
            var_stack: Vec::new(),
            type_var_stack: Vec::new(),
            counter: 0,
        }
    }
}

/// This is the primary interface that you would interact with.
pub struct Parser<'a>
{
    /// The current parsing context
    context: Context<>,
    /// The internal scanner (see the
    /// `ress` crate for more details)
    scanner: Scanner<'a>,
    /// The next item,
    look_ahead: Item<Token<&'a str>>,
    /// Since we are looking ahead, we need
    /// to make sure we don't miss the eof
    /// by using this flag
    found_eof: bool,
    /// The current position we are parsing
    current_position: Position,
    look_ahead_position: Position,
    /// To ease debugging this will be a String representation
    /// of the look_ahead token, it will be an empty string
    /// unless you are using the `debug_look_ahead` feature
    _look_ahead: String,
}

impl<'a> Parser<'a> {
    /// Create a new parser with the provided
    /// javascript
    pub fn new(text: &'a str) -> Res<Self> {
        let s = Scanner::new(text);
        let context = Context::default();
        Self::_new(s, context)
    }
}

impl<'b> Parser<'b> {
    fn _new(
        scanner: Scanner<'b>,
        context: Context<>,
    ) -> Res<Self> {
        let look_ahead = Item {
            token: Token::EoF,
            span: Span { start: 0, end: 0 },
            location: SourceLocation::new(Position::new(0, 0), Position::new(0, 0)),
        };
        let mut ret = Self {
            scanner,
            look_ahead,
            found_eof: false,
            context,
            current_position: Position { line: 1, column: 0 },
            look_ahead_position: Position { line: 1, column: 0 },
            _look_ahead: String::new(),
        };
        let _ = ret.next_item()?;
        Ok(ret)
    }

    /// consume a parser context, populate it
    fn parse_internal(
        &mut self, 
        parser_context: &mut ParserContext
    ) {
        let mut init_body = vec![];
        
        let mut fake_parser_func_context = ParserFuncContext::new();

        loop {
            if self.look_ahead.token.is_eof() {
                break;
            } else {
                self.parse_global_statement(&mut init_body, &mut fake_parser_func_context, parser_context);
            }
        }

        let start_function = Func{ 
            name: String::from("__start"), return_type: Type::RealVoid, args: vec![], export: false, import: false, body: init_body, 
            local_vars: vec![], closure: vec![], local_var_map: HashMap::new()
        };

        let idx = parser_context.funcs.len();
        parser_context.func_map.insert(String::from("__start"), idx as u32);
        parser_context.funcs.push(start_function);

        if !fake_parser_func_context.closure.is_empty() {
            parser_context.errors.push(Error::Other(String::from("found closure reference when not expecting it")));
        }
    }

    ///parse!
    pub fn parse(&mut self) -> Result<Program, Vec<Error>> {
        let mut parser_context = ParserContext::new();
        self.parse_internal(&mut parser_context);
        if parser_context.errors.is_empty() {
            Ok(Program {start: String::from("__start"), globals: parser_context.globals, funcs: parser_context.funcs, global_var_map: parser_context.global_var_map, func_map: parser_context.func_map})
        } else {
            Err(parser_context.errors)
        }
    }

    pub fn next_position(&self) -> SourceLocation {
        self.look_ahead.location
    }

    /// Skip the next token, ignoring it.
    fn skip_next_item(&mut self) -> () {
        loop {
            self.context.has_line_term = self.scanner.pending_new_line;
            if let Some(look_ahead) = self.scanner.next() {
                let look_ahead = look_ahead.unwrap();
                if cfg!(feature = "debug_look_ahead") {
                    self._look_ahead = format!(
                        "{:?}: {:?}",
                        look_ahead.token,
                        self.scanner.string_for(&look_ahead.span)
                    );
                }
                self.look_ahead_position = look_ahead.location.start;
                if look_ahead.token.is_comment() {
                    continue;
                }
                replace(&mut self.look_ahead, look_ahead);
                return;
            } else {
                // if the next item is None, the iterator is spent
                // if the last token was EOF then we want to return that
                // and mark that we have found EOF, if we get here a second
                // time we want to return the ParseAfterEoF error
                if self.look_ahead.token.is_eof() {
                    if !self.found_eof {
                        self.found_eof = true;
                        return;
                    }
                }
            }
        }
    }


    /// Peek at the next item, without changing state.
    fn peek_next_item(& self) -> Item<Token<&'b str>> {
        self.look_ahead.clone()
    }

    /// Peek at the next token, without changing state.
    fn peek_next_token(& self) -> Token<&'b str> {
        self.look_ahead.token.clone()
    }

    /// Peek at the next locatiom, without changing state.
    fn peek_next_location(& self) -> SourceLocation {
        self.look_ahead.location.clone()
    }

    /// Request the next token from the scanner
    /// swap the last look ahead with this new token
    /// and return the last token
    fn next_item(&mut self) -> Res<Item<Token<&'b str>>> {
        loop {
            self.context.has_line_term = self.scanner.pending_new_line;
            if let Some(look_ahead) = self.scanner.next() {
                let look_ahead = look_ahead.unwrap();
                if cfg!(feature = "debug_look_ahead") {
                    self._look_ahead = format!(
                        "{:?}: {:?}",
                        look_ahead.token,
                        self.scanner.string_for(&look_ahead.span)
                    );
                }
                self.look_ahead_position = look_ahead.location.start;
                if look_ahead.token.is_comment() {
                    continue;
                }
                let ret = replace(&mut self.look_ahead, look_ahead);
                return Ok(ret);
            } else {
                // if the next item is None, the iterator is spent
                // if the last token was EOF then we want to return that
                // and mark that we have found EOF, if we get here a second
                // time we want to return the ParseAfterEoF error
                if self.look_ahead.token.is_eof() {
                    if self.found_eof {
                        return Err(Error::ParseAfterEoF);
                    } else {
                        self.found_eof = true;
                        return Ok(self.look_ahead.clone());
                    }
                } else {
                    return Err(Error::UnexpectedEoF("".to_string()));
                }
            }
        }
    }

    fn expected_token_error_raw(&self, item: &Item<Token<&'b str>>, expectation: &[&str]) -> Error {
        let expectation = expectation
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if i == expectation.len() - 1 && expectation.len() > 1 {
                    format!("or `{}`", s)
                } else {
                    format!("`{}`", s)
                }
            })
            .collect::<Vec<String>>()
            .join(", ");
        Error::UnexpectedToken(
            item.location.clone(),
            format!("Expected {}; found {:?}", expectation, item.token),
        )
    }

    fn expected_token_error<T>(&self, item: &Item<Token<&'b str>>, expectation: &[&str]) -> Res<T> {
        Err(self.expected_token_error_raw(item, expectation))
    }

    fn unexpected_token_error_raw(&self, span: Span, location: &SourceLocation, msg: &str) -> Error {
        let name = self.scanner.string_for(&span).unwrap_or_default();
        Error::UnexpectedToken(
            location.clone(),
            format!("Found unexpected token: {}; {}", name, msg),
        )
    }

    fn unexpected_token_error<T>(&self, span: Span, location: &SourceLocation, msg: &str) -> Res<T> {
        Err(self.unexpected_token_error_raw(span, location, msg))
    }

    /// move on to the next item and validate it matches
    /// the keyword provided, discarding the result
    /// if it does
    #[inline]
    fn expect_keyword(&mut self, k: Keyword) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_keyword(k) {
            return self.expected_token_error(&next, &[&format!("{:?}", k)]);
        }
        Ok(())
    }

    fn parse_type_decl_args(&mut self) -> Res<Vec<String>> {
        assert_punct!(self, Punct::LessThan);

        let mut out: Vec<String> = Vec::new();
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::Punct(Punct::GreaterThan) => { self.skip_next_item(); return Ok(out); },
                Token::Ident(n) => {
                    out.push(n.to_string());
                },
                _ => return self.unexpected_token_error(next.span, &next.location, "expecting identifier in type list")
            }
        }
    }

    fn parse_type(&mut self, 
        parser_context: &mut ParserContext,
    ) -> Res<Type> {
        let next = assert_next!(self, "expecting type");
        let token = next.token;
        match token {
            Token::Keyword(keyword) => {
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
                    Keyword::BigInt => Ok(Type::BigInt),
                    Keyword::Int => Ok(Type::Int),
                    //Keyword::Tuple => Ok(Type::Tuple),
                    Keyword::Object => Ok(Type::Object),
                    Keyword::Any => Ok(Type::Any),
                    Keyword::Ptr => {
                        assert_punct!(self, Punct::LessThan);
                        let inner = self.parse_type(parser_context);
                        assert_ok!(inner);
                        assert_punct!(self, Punct::GreaterThan);
                        match inner {
                            Type::IntLiteral(n) => { 
                                let o_pa = PtrAlign::from_i32(n);
                                match o_pa {
                                    Some(pa) => Ok(Type::Ptr(pa)),
                                    None => Err(Error::InvalidTypeName(next.location.clone(), keyword.as_str().to_owned()))
                                }
                            },
                            _ => Err(Error::InvalidTypeName(next.location.clone(), keyword.as_str().to_owned()))        
                        }
                        
                    },
                    _ => Err(Error::InvalidTypeName(next.location.clone(), keyword.as_str().to_owned()))
                }
            },
            Token::Ident(ident) => {
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
                                UserType::Class(ct) => {
                                    Ok(Type::UserClass{name: ident.to_string(), type_args})
                                },
                                UserType::Struct(st) => {
                                    Ok(Type::UserStruct{name: ident.to_string()})
                                }
                            }
                        }
                    }
                }
            },
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

    fn parse_function_decl_arg(&mut self,
        parser_context: &mut ParserContext,
    ) -> Res<FuncArg> {
        let next = assert_next!(self, "expecting argument declaration");
        let id = assert_ident!(next, "Expecting variable name to be an identifier");
        let name = id.to_string();
        let next = self.peek_next_item();
        let token = &next.token;
        match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                let arg_type = self.parse_type(parser_context);
                assert_ok!(arg_type);
                
                let next = self.peek_next_item();
                let token = &next.token;
                if token.matches_punct(Punct::Comma) || token.matches_punct(Punct::CloseParen) {
                    if token.matches_punct(Punct::Comma) {
                        self.skip_next_item();
                    }
                    Ok(FuncArg{name: name, r#type: arg_type})
                } else {
                    self.unexpected_token_error(next.span, &next.location, "expecting ')' or ',' in arg list")
                }
            },
            Token::Punct(Punct::Comma) => {
                Ok(FuncArg{name: name, r#type: Type::Undeclared})
            },
            _ => self.unexpected_token_error(next.span, &next.location, "expecting ')' or ',' in arg list")
        }
    }

    fn parse_function_decl_args(&mut self,
        parser_context: &mut ParserContext,
    ) -> Res<Vec<FuncArg>> {
        assert_punct!(self, Punct::OpenParen);
        let mut args: Vec<FuncArg> = Vec::new();
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::Punct(Punct::CloseParen) => { self.skip_next_item(); return Ok(args); },
                Token::Ident(_) => {
                    let arg = self.parse_function_decl_arg(parser_context);
                    assert_ok!(arg);
                    args.push(arg)
                },
                _ => return self.unexpected_token_error(next.span, &next.location, "expecting identifier in arg list")
            }
        }
    }

    /// register params in the local scope
    fn register_params(&mut self, arg_list: &Vec<FuncArg>, parser_func_context: &mut ParserFuncContext) {
        for arg in arg_list {
            let internal_name = self.context.get_unique_name(&arg.name);
            self.context.add_var(&arg.name, &internal_name, arg.r#type.clone(), false);
            //todo: constant params
            //todo: default params
            let idx = parser_func_context.local_vars.len();
            parser_func_context.local_var_map.insert(internal_name.clone(), idx as u32);
            parser_func_context.local_vars.push(VariableDecl{internal_name: internal_name, orig_name: arg.name.clone(), r#type: arg.r#type.clone(), constant: false, init: None, closure_source: false, arg: true});
        }
    }

    fn parse_named_function_decl(&mut self, 
        export: bool,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr>{
        let mut parser_func_context_inner = ParserFuncContext::new();
        let mut loc = self.peek_next_location();

        self.expect_keyword(Keyword::Function)?;
        let next = assert_next!(self, "Expecting function name");
        let id = assert_ident!(next, "Expecting function name to be an identifier");

        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_punct(Punct::LessThan) {
            let type_args = self.parse_type_decl_args();
            assert_ok!(type_args);
            self.context.push_func_type_scope(&type_args);
            parser_context.errors.push(Error::NotYetImplemented(next.location.clone(), String::from("generic functions")));
        } else {
            self.context.push_empty_func_type_scope();
        }

        self.context.push_func_scope();

        let arg_list = self.parse_function_decl_args(parser_context);
        assert_ok!(arg_list);

        self.register_params(&arg_list, &mut parser_func_context_inner);

        assert_punct!(self, Punct::Colon);
        let return_type = self.parse_type(parser_context);
        assert_ok!(return_type);
        parser_func_context_inner.func_return_type = return_type;

        // for the moment, all functions need to have squigglies
        assert_punct!(self, Punct::OpenBrace);

        let old_in_iteration = self.context.in_iteration;
        self.context.in_iteration = false;
        
        let mut exprs:  Vec<TypedExpr> = Vec::new();
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            
            if token.matches_punct(Punct::CloseBrace) {
                loc.end = next.location.end.clone();
                self.skip_next_item();
                break;
            }

            let expr = self.parse_statement(&mut parser_func_context_inner, parser_context);
            assert_ok!(expr);
            exprs.push(expr);
        }

        self.context.pop_scope();
        self.context.in_iteration = old_in_iteration;

        self.context.pop_type_scope();

        // today if you don't have a return value at the end of function, you get errors at run time. So let's check now.
        if parser_func_context_inner.func_return_type != Type::RealVoid {
            let o_last = exprs.last();
            match o_last {
                Some(last) => {
                    match &last.expr {
                        Expr::Return(o_e) => {
                            if o_e.is_none() {
                                parser_context.errors.push(Error::NoValueReturned)
                            }
                        },
                        _ => {
                            if last.r#type == Type::RealVoid {
                                parser_context.errors.push(Error::NoValueReturned);
                            }
                        },
                    }
                },
                _ => parser_context.errors.push(Error::NoValueReturned)
            }
        }
        
        let func = Func{
            name: id.to_string(), return_type: parser_func_context_inner.func_return_type, args: arg_list, export, body: exprs, 
            local_vars: parser_func_context_inner.local_vars, closure: parser_func_context_inner.closure, 
            local_var_map: parser_func_context_inner.local_var_map, import: false
        };

        let name = func.name.clone();
        let func_closure = func.closure.clone();
        // now we have a closure of an inner function, we need to capture it. So look at every element
        for cr in &func_closure {
            // is it in our local vars?
            let o_local_var = parser_func_context_outer.local_vars.iter_mut().find(|lv| lv.internal_name == cr.internal_name);
            if o_local_var.is_some() {
                // yes it is, so mark that local variable as a closure source
                o_local_var.unwrap().closure_source = true;
            } else {
                //not a local variable, so it's in the closure of this function, so make sure it's captured
                parser_func_context_outer.closure.push(cr.clone())
            }
        }
        let idx = parser_context.funcs.len();
        parser_context.func_map.insert(func.name.clone(), idx as u32);
        let func_type = func.get_func_type();
        parser_context.funcs.push(func); 
        Ok(TypedExpr{expr: Expr::FuncDecl(FuncDecl{name: name, closure: func_closure}), is_const: true, r#type: Type::Func{func_type: Box::new(func_type), type_args: vec![]}, loc: loc})
    }

    /// This is for parsing an export declaration. We require these to be in module root, 
    /// and therefore we also pick up the baggage about 'fake' function contexts and init_body
    /// It's not brilliant, this code. It has a lot of baggage that could be removed
    fn parse_export_decl(&mut self, 
        init_body: &mut Vec<TypedExpr>,
        fake_parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) {
        self.skip_next_item();
        let tok = self.peek_next_token();
        match &tok {
            Token::Keyword(ref k) => match k {
                Keyword::Function => {
                    let func = self.parse_named_function_decl(true, fake_parser_func_context, parser_context);
                    match func {
                        Ok(f) => init_body.push(f),
                        Err(e) => parser_context.errors.push(e)
                    };
                },
                Keyword::Const => {
                    let const_decl = self.parse_variable_decl(true, true, true, fake_parser_func_context, parser_context);
                    match const_decl {
                        Ok(c) => init_body.push(c),
                        Err(e) => parser_context.errors.push(e)
                    };
                },
                Keyword::Let => {
                    let var_decl = self.parse_variable_decl(false, true, true, fake_parser_func_context, parser_context);
                    match var_decl {
                        Ok(c) => init_body.push(c),
                        Err(e) => parser_context.errors.push(e)
                    };
                },
                _ => parser_context.errors.push(Error::Other("argh".to_string()))
            },
            _ => parser_context.errors.push(Error::Other("argh".to_string()))
        }
    }

    fn parse_variable_decl(&mut self, 
        constant: bool, 
        global: bool,
        export: bool,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let next = assert_next!(self, "Expecting variable name");
        let id = assert_ident!(next, "Expecting variable name to be an identifier");
        let next = self.peek_next_item();
        let token = &next.token;
        let mut var_type = match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                let arg_type = self.parse_type(parser_context);
                assert_ok!(arg_type);
                arg_type
            },
            Token::Punct(Punct::Equal) => {
                Type::Undeclared
            },
            _ => return self.unexpected_token_error(next.span, &next.location, "variable must have a type or a value")
        };

        let next = self.peek_next_item();
        let token = &next.token;
        let init = if token.matches_punct(Punct::Equal) {
            self.skip_next_item();
            let init = self.parse_expr(false, parser_func_context, parser_context);
            assert_ok!(init);
            loc.end = init.loc.end.clone();

            if var_type.is_undeclared() {
                var_type = init.r#type.clone();
                Some(init)
            } else if var_type != init.r#type {
                let o_cast_expr = try_cast(&var_type, &init);
                match o_cast_expr {
                    Some(cast_expr) => Some(cast_expr),
                    None => { 
                        parser_context.errors.push(Error::TypeFailureVariableCreation(init.loc));
                        None
                    }
                }
            } else {
                Some(init)
            }
        } else {
            loc.end = next.location.end.clone();
            Option::None
        };

        assert_semicolon!(self);
        let name = id.to_string();
        if !global {
            let internal_name = self.context.get_unique_name(&name);
            self.context.add_var(&name, &internal_name, var_type.clone(), constant);
        
            let vd = VariableDecl{
                internal_name: internal_name, r#type: var_type, constant, init, closure_source: false, arg: false, orig_name: name
            };
            let idx = parser_func_context.local_vars.len();
            parser_func_context.local_vars.push(vd.clone());
            parser_func_context.local_var_map.insert(vd.internal_name.clone(), idx as u32);
            Ok(TypedExpr{expr: Expr::VariableDecl(Box::new(vd)), is_const: constant, r#type: Type::RealVoid, loc: loc})
        } else {
            let idx = parser_context.globals.len();
            let decl = GlobalVariableDecl{name: name.clone(), r#type: var_type, constant, init, export};
            parser_context.globals.push(decl.clone());
            parser_context.global_var_map.insert(name.clone(), idx as u32);
            
            Ok(TypedExpr{expr: Expr::GlobalVariableDecl(Box::new(decl)), is_const: constant, r#type: Type::RealVoid, loc: loc})
        }

    }

    /// The root of the module is parsed and will run in its 'start' function.
    /// This parses that. The init_body param is that function body. 
    /// Because we put restrictions on this function - it can't have locals or a closure - 
    /// we fake that and will blow up if it violates that. 
    fn parse_global_statement(&mut self, 
        init_body: &mut Vec<TypedExpr>,
        fake_parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) {
        let next = self.peek_next_item();
        let token = &next.token;
        match &token {
            Token::Keyword(ref k) => match k {
                Keyword::Export => self.parse_export_decl(init_body, fake_parser_func_context, parser_context),
                Keyword::Function => {
                    let func = self.parse_named_function_decl(false, fake_parser_func_context, parser_context);
                    match func {
                        Ok(f) => {
                            init_body.push(f);
                        },
                        Err(e) => parser_context.errors.push(e)
                    };
                },
                Keyword::Const => {
                    let const_decl = self.parse_variable_decl(true, true, false, fake_parser_func_context, parser_context);

                    match const_decl {
                        Ok(c) => init_body.push(c),
                        Err(e) => parser_context.errors.push(e)
                    };
                },
                Keyword::Let => {
                    let var_decl = self.parse_variable_decl(false, true, false, fake_parser_func_context, parser_context);
                    match var_decl {
                        Ok(c) => init_body.push(c),
                        Err(e) => parser_context.errors.push(e)
                    };
                },
                Keyword::Struct => {
                    let struct_decl = self.parse_struct_decl(parser_context);
                    match struct_decl {
                        Ok(c) => init_body.push(c),
                        Err(e) => parser_context.errors.push(e)
                    };
                }
                _ => { parser_context.errors.push(self.unexpected_token_error_raw(next.span, &next.location, "expecting valid statement")); self.skip_next_item(); }
            },
            _ => {
                // if we don't know what this statement is, parse it as an expr
                let expr = self.parse_expr(true, fake_parser_func_context, parser_context);

                // ugh this is like a hundred million lines to check the semi-colon
                let next = self.next_item();
                match next {
                    Ok(next) => {
                        if !next.token.matches_punct(Punct::SemiColon) {
                            parser_context.errors.push(self.expected_token_error_raw(&next, &[&";"]));
                        } else {
                            self.skip_next_item();
                        }
                        match expr {
                            Err(e) => parser_context.errors.push(e),
                            Ok(s) => {
                                init_body.push(s);
                            }
                        }
                    },
                    Err(e) => {
                        parser_context.errors.push(e)
                    }
                }
            }
        }
    }

    fn parse_block(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut out: Vec<TypedExpr> = vec![];
        let next = self.peek_next_item();
        let mut loc = next.location.clone();
        
        let token = &next.token;
        
        if token.matches_punct(Punct::OpenBrace) {
            self.skip_next_item();
                
            let mut next = self.peek_next_item();
            let mut token = &next.token;
            self.context.push_block_scope();
            
            while !token.matches_punct(Punct::CloseBrace) {
                let stmt = self.parse_statement(parser_func_context, parser_context);    
                assert_ok!(stmt);
                out.push(stmt);
                next = self.peek_next_item();
                loc.end = next.location.end.clone();
                token = &next.token;
            }
            self.skip_next_item();
            
            self.context.pop_scope();
        
        } else {
            let stmt = self.parse_statement(parser_func_context, parser_context);
            assert_ok!(stmt);
            loc.end = stmt.loc.end.clone();
            out.push(stmt);
        } 

        let out_type = out.last().map_or(Type::RealVoid, |x| x.r#type.clone());
        Ok(TypedExpr{expr: Expr::Block(out), is_const: true, r#type: out_type, loc: loc})
    }

    fn parse_if(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let lookahead_item = self.peek_next_item();
        let mut loc = lookahead_item.location.clone();
        self.skip_next_item();

        assert_punct!(self, Punct::OpenParen);
        let cond = self.parse_expr(true, parser_func_context, parser_context);
        assert_ok!(cond);
        if cond.r#type != Type::Boolean {
            parser_context.errors.push(Error::TypeFailure(cond.loc.clone(), Type::Boolean,  cond.r#type.clone()));
        }
        assert_punct!(self, Punct::CloseParen);

        let then_block = self.parse_block(parser_func_context, parser_context);
        assert_ok!(then_block);
            
        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_keyword(Keyword::Else) {
            self.skip_next_item();
            let else_block = self.parse_block(parser_func_context, parser_context);
            assert_ok!(else_block);
            let then_block_type = then_block.r#type.clone();
            if then_block.r#type != else_block.r#type {
                parser_context.errors.push(Error::TypeFailureIf(then_block.r#type.clone(), else_block.r#type.clone()));
            } 
            loc.end = else_block.loc.end.clone();
            Ok(TypedExpr{expr: Expr::IfThenElse(Box::new(cond), Box::new(then_block), Box::new(else_block)), is_const: true, r#type: then_block_type, loc: loc})
        } else {
            loc.end = then_block.loc.end.clone();
            Ok(TypedExpr{expr: Expr::IfThen(Box::new(cond), Box::new(then_block)), is_const: true, r#type: Type::RealVoid, loc: loc})
        }
    }

    fn parse_while(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        assert_punct!(self, Punct::OpenParen);
        let cond = self.parse_expr(true, parser_func_context, parser_context);
        assert_ok!(cond);
        if cond.r#type != Type::Boolean {
            parser_context.errors.push(Error::TypeFailure(cond.loc.clone(), Type::Boolean, cond.r#type.clone()));
        }
        assert_punct!(self, Punct::CloseParen);

        let old_in_iteration = self.context.in_iteration;
        self.context.in_iteration = true;
        let block = self.parse_block(parser_func_context, parser_context);
        self.context.in_iteration = old_in_iteration;
        assert_ok!(block);
        loc.end = block.loc.end.clone();

        Ok(TypedExpr{expr: Expr::While(Box::new(cond), Box::new(block)), is_const: true, r#type: Type::RealVoid, loc: loc})
    }

    fn parse_class_decl(&mut self,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let next = assert_next!(self, "Expecting class name");
        let id = assert_ident!(next, "Expecting class name to be an identifier");

        if parser_context.type_map.contains_key(&id.to_string()) {
            parser_context.errors.push(Error::DuplicateTypeName(id.to_string()))
        }

        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_punct(Punct::LessThan) {
            let type_args = self.parse_type_decl_args();
            assert_ok!(type_args);
            self.context.push_func_type_scope(&type_args);
            parser_context.errors.push(Error::NotYetImplemented(next.location.clone(), String::from("generic classes")));
        } else {
            self.context.push_empty_func_type_scope();
        }

        let mut members: Vec<ClassMember> = vec![];

        assert_punct!(self, Punct::OpenBrace);

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
                    assert_ok!(member_type);
                    members.push(ClassMember{name: i.to_string(), r#type: member_type.clone(), privacy: Privacy::Public});
                },
                _ => {
                    return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "expecting '}' or member")
                }
            }
        }

        self.context.pop_type_scope();

        parser_context.type_map.insert(id.to_string(), UserType::Class(ClassType{members: members}));

        Ok(TypedExpr{expr: Expr::ClassDecl(id.to_string()), is_const: true, r#type: Type::RealVoid, loc: loc})
    }

    fn parse_struct_decl(&mut self,    
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let next = assert_next!(self, "Expecting struct name");
        let id = assert_ident!(next, "Expecting struct name to be an identifier");

        if parser_context.type_map.contains_key(&id.to_string()) {
            parser_context.errors.push(Error::DuplicateTypeName(id.to_string()))
        }

        self.context.push_empty_func_type_scope();

        let mut members: Vec<StructMember> = vec![];

        assert_punct!(self, Punct::OpenBrace);

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
                    assert_ok!(member_type);
                    members.push(StructMember{name: i.to_string(), r#type: member_type.clone()});
                },
                _ => {
                    return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "expecting '}' or member")
                }
            }
        }

        self.context.pop_type_scope();

        parser_context.type_map.insert(id.to_string(), UserType::Struct(StructType{members: members}));

        Ok(TypedExpr{expr: Expr::StructDecl(id.to_string()), is_const: true, r#type: Type::RealVoid, loc: loc})
    }

    /// a statement is something you can't assign to (like break or a variable declaration) or a true expr.
    fn parse_statement(&mut self, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let next = self.peek_next_item();
        let token = &next.token;
        match &token {
            Token::Keyword(ref k) => match k {
                Keyword::Const => self.parse_variable_decl(true, false, false, parser_func_context, parser_context),
                Keyword::Let => self.parse_variable_decl(false, false, false, parser_func_context, parser_context),
                Keyword::Return => {
                    let mut loc = next.location.clone();
                    self.skip_next_item();
                    let next = self.peek_next_item();
                    let token = &next.token;
                    if token.matches_punct(Punct::SemiColon) {
                        // check the return type of this expression versus the function return type.
                        if parser_func_context.func_return_type != Type::RealVoid {
                            parser_context.errors.push(Error::TypeFailureReturn(parser_func_context.func_return_type.clone(), Type::RealVoid));
                        }
                        self.skip_next_item();
                        Ok(TypedExpr{expr: Expr::Return(Box::new(None)), is_const: true, r#type: Type::Never, loc: loc})
                    } else {
                        let expr = self.parse_expr(true, parser_func_context, parser_context);
                        assert_semicolon!(self);
                        assert_ok!(expr);
                        // return type checking
                        let expr_type = expr.r#type.clone();
                        if parser_func_context.func_return_type != expr.r#type {
                            let o_cast = try_cast(&parser_func_context.func_return_type, &expr);
                            match o_cast {
                                None => { 
                                    Err(Error::TypeFailureReturn(parser_func_context.func_return_type.clone(), expr_type))
                                },
                                Some(new_expr) => {
                                    loc.end = new_expr.loc.end.clone();
                                    Ok(TypedExpr{expr: Expr::Return(Box::new(Some(new_expr))), is_const: true, r#type: Type::Never, loc: loc})
                                }
                            }
                        } else {
                            loc.end = expr.loc.end.clone();
                            Ok(TypedExpr{expr: Expr::Return(Box::new(Some(expr))), is_const: true, r#type: Type::Never, loc: loc})
                        }
                    }
                },
                
                Keyword::Class => {
                    self.parse_class_decl(parser_context)
                },

                Keyword::Break => {
                    self.skip_next_item();
                    if self.context.in_iteration {
                        Ok(TypedExpr{expr: Expr::Break, is_const: true, r#type: Type::Never, loc: next.location.clone()})
                    } else {
                        Err(Error::NotInLoop(String::from("break")))
                    }
                },

                Keyword::Continue => {
                    self.skip_next_item();
                    if self.context.in_iteration {
                        Ok(TypedExpr{expr: Expr::Continue, is_const: true, r#type: Type::Never, loc: next.location.clone()})
                    } else {
                        Err(Error::NotInLoop(String::from("continue")))
                    }
                },

                Keyword::While => {
                    self.parse_while(parser_func_context, parser_context)
                },
                
                _ => {
                    // if we don't know what this statement is, parse it as an expr
                    let expr = self.parse_expr(true, parser_func_context, parser_context);
                    expr
                }
            },

            _ => {
                // if we don't know what this statement is, parse it as an expr
                let expr = self.parse_expr(true, parser_func_context, parser_context);
                assert_semicolon!(self);
                expr
            }
        }
    }

    fn parse_new(&mut self) -> Res<TypedExpr> {
        let next_item = self.next_item();
        return Err(Error::NotYetImplemented(next_item.unwrap().location, "new".to_string()));
    }

    fn parse_function_call_args(&mut self,
        arg_types: &Vec<Type>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<Vec<TypedExpr>> {

        let mut out: Vec<TypedExpr> = vec![];
        self.skip_next_item();

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        match lookahead {
            Token::Punct(p) => match p {
                Punct::CloseParen => { 
                    self.skip_next_item();
                    if arg_types.len() != 0 {
                        parser_context.errors.push(Error::NotEnoughArgs);        
                    }
                    return Ok(out);
                },
                _ => {}
            },
            _ => {}
        }

        loop{
            let expr = self.parse_expr(false, parser_func_context, parser_context);
            assert_ok!(expr);
            
            if out.len() == arg_types.len() {
                parser_context.errors.push(Error::TooManyArgs);
            } else {
                let arg_type = &arg_types[out.len()];
                if *arg_type != expr.r#type {
                    let expr_type = expr.r#type.clone();
                    let o_cast = try_cast(arg_type, &expr);
                    match o_cast {
                        None => parser_context.errors.push(Error::TypeFailure(expr.loc.clone(), arg_type.clone(), expr_type)),
                        Some(new_expr) => out.push(new_expr)
                    }
                } else {
                    out.push(expr);
                }
            }

            let lookahead_item = self.peek_next_item();
            let lookahead = lookahead_item.token;
         
            match lookahead {
                Token::Punct(p) => match p {
                    Punct::CloseParen => { 
                        self.skip_next_item();
                        if arg_types.len() != out.len() {
                            parser_context.errors.push(Error::NotEnoughArgs);        
                        }
                        return Ok(out);
                    },
                    Punct::Comma => {
                        self.skip_next_item();
                    },
                    _ => return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "need expr or comma")
                },
                _ => return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "need expr or comma")
            }
        }
    }

    fn parse_component(&mut self, id: String) -> Res<TypedExpr> {
        let next_item = self.next_item();
        return Err(Error::NotYetImplemented(next_item.unwrap().location, "dot component".to_string()));
    }

    fn parse_paren_expr(&mut self, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        // either a bracket or an arrow function declaration
        let lookahead_item = self.peek_next_item();
        let mut loc = lookahead_item.location.clone();

        assert_punct!(self, Punct::OpenParen);
        let expr = self.parse_expr(true, parser_func_context, parser_context);
        assert_ok!(expr);
        let lookahead_item = self.peek_next_item();
        loc.end = lookahead_item.location.end.clone();
        assert_punct!(self, Punct::CloseParen);

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        if lookahead.matches_punct(Punct::Arrow) {
            return Err(Error::NotYetImplemented(lookahead_item.location.clone(), "arrow function".to_string()));
        }

        let expr_type = expr.r#type.clone();
        Ok(TypedExpr{expr: Expr::Parens(Box::new(expr)), r#type: expr_type, is_const: true, loc: loc})
    }

    fn parse_object_literal(&mut self) -> Res<TypedExpr> {
        let next_item = self.next_item();
        
        return Err(Error::NotYetImplemented(next_item.unwrap().location, "object literal".to_string()));
    }

    fn get_postfix_unary_operator_for_token(&self, span: Span, location: SourceLocation, token: &Token<&'b str>) -> Res<UnaryOperator> {
        match token {
            Token::Punct(p) => match p {
                Punct::DoublePlus => Ok(UnaryOperator::PostfixIncrement),
                Punct::DoubleDash => Ok(UnaryOperator::PostfixDecrement),
                _ => self.unexpected_token_error(span, &location, "unexpected unary operator"),
            },
            _ => self.unexpected_token_error(span, &location, "unexpected binary operator"),
        }
    }

    fn try_parse_built_in(&mut self, 
        id: &String,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<Res<TypedExpr>> {
        match id.as_str() {
            "__memorySize" => {
                Some(self.parse_mem_size())
            },
            "__memoryGrow" => {
                Some(self.parse_mem_grow(parser_func_context, parser_context))
            },
            //it's a 
            "__trap" => {
                Some(self.parse_trap())
            },
            _ => None
        }
    }

    fn parse_ident_expr(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let next = assert_next!(self, "expecting argument declaration");
        let id = assert_ident!(next, "Expecting variable name to be an identifier");

        let o_sv = self.context.get_scoped_var(&id.to_string());
        let mut expr = match o_sv {
            None => {
                let g_idx = parser_context.global_var_map.get(&id.to_string());
                match g_idx {
                    Some(idx) => {
                        let g_var = &parser_context.globals[*idx as usize];
                        TypedExpr{expr: Expr::GlobalVariableUse(id.to_string()), r#type: g_var.r#type.clone(), is_const: g_var.constant, loc: next.location.clone()}
                    },
                    None => { 
                        // peek to see if this is a function call. If it is, we resolve it later
                        let lookahead_item = self.peek_next_item();
                        let lookahead = lookahead_item.token;

                        match lookahead {
                            Token::Punct(p) => match p {
                                Punct::OpenParen => {
                                    // the hash map returns a reference to the innards; the borrow checker freaks out
                                    // unless I deref as soon as possible
                                    let o_func_id = match parser_context.func_map.get(&id.to_string()) {
                                        Some(idx) => Some(*idx),
                                        None => None,
                                    };
                                    match o_func_id{
                                        Some(func_id) => {
                                            let arg_types = parser_context.funcs[func_id as usize].get_arg_types();
                                            let args = self.parse_function_call_args(&arg_types, parser_func_context, parser_context);
                                            assert_ok!(args);
                                            let func_return_type = parser_context.funcs[func_id as usize].return_type.clone();
                                            let loc = SourceLocation::new(next.location.start.clone(), args.last().map(|a| a.loc.end.clone()).unwrap_or(next.location.end.clone()));
                                            TypedExpr{expr: Expr::StaticFuncCall(id.to_string().clone(), args), r#type: func_return_type, is_const: true, loc: loc}
                                        },
                                        None => {
                                            //might be a built in, so check that
                                            let oe_built_in = self.try_parse_built_in(&id.to_string(), parser_func_context, parser_context);
                                            match oe_built_in {
                                                None => {return Err(Error::VariableNotRecognised(id.to_string().clone()));},
                                                Some(e_built_in) => {
                                                    match e_built_in {
                                                        Err(e) => {return Err(e);},
                                                        Ok(built_in) => built_in
                                                    }
                                                }
                                            }
                                        }
                                    }
                                },
                                _ => return Err(Error::VariableNotRecognised(id.to_string().clone()))
                            },
                            _ => return Err(Error::VariableNotRecognised(id.to_string().clone()))
                        }
                    }
                }
            },
            Some(sv) => match sv {
                ScopedVar::ClosureRef{internal_name, r#type, constant} => {
                    let is_new = !parser_func_context.closure.iter().any(|x| x.internal_name.eq(internal_name));
                    if is_new {
                        parser_func_context.closure.push(ClosureRef{internal_name: internal_name.clone(), r#type: r#type.clone(), constant: *constant})
                    }
                    
                    TypedExpr{expr: Expr::ClosureVariableUse(internal_name.clone()), r#type: r#type.clone(), is_const: *constant, loc: next.location.clone()}
                },
                ScopedVar::Local{internal_name, r#type, constant} => TypedExpr{expr: Expr::LocalVariableUse(internal_name.clone()), r#type: r#type.clone(), is_const: *constant, loc: next.location.clone()},
            }
        };

        //now we either have a function call or a variable, let's see if it breaks down further
        loop {
            let lookahead_item = self.peek_next_item();
            let lookahead = lookahead_item.token;
         
            //first peek for postfix operators
            let o_uod = lookahead.get_unary_operator_data();
            match o_uod {
                Some(uod) => {
                    if uod.fix == Fix::Postfix || uod.fix == Fix::Both {
                        let op = self.get_postfix_unary_operator_for_token(lookahead_item.span, lookahead_item.location, &lookahead);
                        assert_ok!(op);
                        self.skip_next_item();
                        let o_outer_type = types::get_unary_op_type(op.get_op_type(), &expr.r#type);
                        match o_outer_type {
                            Some(outer_type) => { 
                                let loc = SourceLocation::new(expr.loc.start.clone(), lookahead_item.location.end.clone());
                                expr = TypedExpr{expr: Expr::UnaryOperator(UnaryOperatorApplication{op: op, expr: Box::new(expr.clone())}), r#type: outer_type, is_const: true, loc: loc}; continue 
                            },
                            None => return Err(Error::TypeFailureUnaryOperator),
                        };
                    }
                },
                _ => {},
            };

            //now, other things it could be
            match lookahead {
                Token::Punct(p) => match p {
                    Punct::OpenParen => { 
                        match &expr.r#type {
                            Type::Func{func_type, type_args: _} => {
                                let args = self.parse_function_call_args(&func_type.in_types, parser_func_context, parser_context);
                                assert_ok!(args);
                                let loc = SourceLocation::new(lookahead_item.location.start.clone(), args.last().map(|a| a.loc.end.clone()).unwrap_or(next.location.end.clone()));                                
                                expr = TypedExpr{expr: Expr::DynamicFuncCall(Box::new(expr.clone()), args), r#type: func_type.out_type.clone(), is_const: true, loc: loc};
                                continue;
                            },
                            _ => return Err(Error::TypeFailureFuncCall),
                        }
                        
                    },
                    Punct::Period => { 
                        let new_expr = self.parse_component(id.to_string());
                        assert_ok!(new_expr);
                        expr = new_expr;
                        continue;
                    },

                    Punct::OpenBracket => {
                        let loc = lookahead_item.location.clone();
                        return Err(Error::NotYetImplemented(loc, String::from("dynamic member")));
                    }
                    _ => { break; }
                },
                _ => { break;}
            }
        }

        Ok(expr)
    }

    /// From wikipedia
    fn parse_expr(&mut self,
        allow_comma: bool,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let expr = self.parse_primary(parser_func_context, parser_context);
        assert_ok!(expr);
        self.parse_expr_1(expr, 0, allow_comma, parser_func_context, parser_context)
    }

    fn get_prefix_unary_operator_for_token(&self, span: Span, location: &SourceLocation, token: &Token<&'b str>) -> Res<UnaryOperator> {
        match token {
            Token::Punct(p) => match p {
                Punct::Bang => Ok(UnaryOperator::LogicalNot),
                Punct::Tilde => Ok(UnaryOperator::BitNot),
                Punct::Dash => Ok(UnaryOperator::Minus),
                Punct::Plus => Ok(UnaryOperator::Plus),
                Punct::DoublePlus => Ok(UnaryOperator::PrefixIncrement),
                Punct::DoubleDash => Ok(UnaryOperator::PrefixDecrement),
                _ => self.unexpected_token_error(span, location, "unexpected unary operator"),
            },
            _ => self.unexpected_token_error(span, location, "unexpected binary operator"),
        }
    }

    /// Parse a simple atomic expression.
    fn parse_primary(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        let o_uod = lookahead.get_unary_operator_data();
        match o_uod {
            Some(uod) => {
                if uod.fix == Fix::Prefix || uod.fix == Fix::Both {
                    let mut loc = lookahead_item.location.clone();
                    let op = self.get_prefix_unary_operator_for_token(lookahead_item.span, &lookahead_item.location, &lookahead);
                    assert_ok!(op);
                    self.skip_next_item();
                    let inner = self.parse_expr(false, parser_func_context, parser_context);
                    assert_ok!(inner);
                    loc.end = inner.loc.end.clone();
                    let o_outer_type = types::get_unary_op_type(op.get_op_type(), &inner.r#type);
                    match o_outer_type {
                        Some(outer_type) => 
                            return Ok(TypedExpr{expr: Expr::UnaryOperator(UnaryOperatorApplication{op: op, expr: Box::new(inner)}), r#type: outer_type, is_const: true, loc: loc}),
                        None => return Err(Error::TypeFailureUnaryOperator),
                    }
                }
            },
            _ => {},
        };

        match lookahead {
            Token::Number(n) => {
                self.skip_next_item();
                match n.kind() {
                    NumberKind::Hex => {
                        if n.str_len() > 10 {
                            let number = n.parse_i64();
                            return Ok(TypedExpr{expr: Expr::BigIntLiteral(number.unwrap()), r#type: Type::BigInt, is_const: true, loc: lookahead_item.location.clone()});
                        } else {
                            let number = n.parse_i32();
                            return Ok(TypedExpr{expr: Expr::IntLiteral(number.unwrap()), r#type: Type::Int, is_const: true, loc: lookahead_item.location.clone()});
                        }
                    },
                    NumberKind::DecI => {
                        let number_i64 = n.parse_i64().unwrap();
                        if number_i64 > std::i32::MAX.into() || number_i64 < std::i32::MIN.into() {
                            return Ok(TypedExpr{expr: Expr::BigIntLiteral(number_i64), r#type: Type::BigInt, is_const: true, loc: lookahead_item.location.clone()});
                        } else {
                            let number_i32 = n.parse_i32().unwrap();
                            return Ok(TypedExpr{expr: Expr::IntLiteral(number_i32), r#type: Type::Int, is_const: true, loc: lookahead_item.location.clone()});
                        }
                    },
                    NumberKind::Bin => {
                        if n.str_len() > 34 {
                            let number = n.parse_i64();
                            return Ok(TypedExpr{expr: Expr::BigIntLiteral(number.unwrap()), r#type: Type::BigInt, is_const: true, loc: lookahead_item.location.clone()});
                        } else {
                            let number = n.parse_i32();
                            return Ok(TypedExpr{expr: Expr::IntLiteral(number.unwrap()), r#type: Type::Int, is_const: true, loc: lookahead_item.location.clone()});
                        }
                    },
                    NumberKind::Oct => {
                        if n.str_len() > 18 {
                            let number = n.parse_i64();
                            return Ok(TypedExpr{expr: Expr::BigIntLiteral(number.unwrap()), r#type: Type::BigInt, is_const: true, loc: lookahead_item.location.clone()});
                        } else {
                            let number = n.parse_i32();
                            return Ok(TypedExpr{expr: Expr::IntLiteral(number.unwrap()), r#type: Type::Int, is_const: true, loc: lookahead_item.location.clone()});
                        }
                    },
                    NumberKind::DecF => {
                        let number = n.parse_f64();
                        return Ok(TypedExpr{expr: Expr::FloatLiteral(number.unwrap()), r#type: Type::Number, is_const: true, loc: lookahead_item.location.clone()});
                    },
                }
            },
            
            Token::Boolean(b) => {
                self.skip_next_item();
                return Ok(TypedExpr{expr:Expr::BoolLiteral(b.is_true()), r#type: Type::Boolean, is_const: true, loc: lookahead_item.location.clone()});
            },

            Token::Null => {
                self.skip_next_item();
                return Ok(TypedExpr{expr:Expr::Null, r#type: Type::Null, is_const: true, loc: lookahead_item.location.clone()});
            },

            Token::Template(_) => {
                self.skip_next_item();
                return Err(Error::NotYetImplemented(lookahead_item.location.clone(), "template strings".to_string()));
            },

            Token::String(sl) => {
                self.skip_next_item();
                return Ok(TypedExpr{expr:Expr::StringLiteral(sl.no_quote().to_string()), r#type: Type::String, is_const: true, loc: lookahead_item.location.clone()});
            },

            Token::Keyword(k) => {
                match k {
                    Keyword::New => return self.parse_new(),
                    Keyword::Void => { 
                        self.skip_next_item();
                        return Ok(TypedExpr{expr:Expr::Void, r#type: Type::FakeVoid, is_const: true, loc: lookahead_item.location.clone()}); 
                    },
                    Keyword::If => {
                        self.parse_if(parser_func_context, parser_context)
                    },
                    Keyword::Function => { 
                        self.parse_named_function_decl(false, parser_func_context, parser_context)
                    },
                    
                    _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "unexpected keyword"),
                }
            },

            Token::Ident(_) => self.parse_ident_expr(parser_func_context, parser_context),

            Token::Punct(p) => match p {
                Punct::OpenParen => self.parse_paren_expr(parser_func_context, parser_context),
                Punct::OpenBrace => self.parse_object_literal(),
                _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "unexpected punctuation"),
            },

            _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "unexpected token"),
        }
    }

    fn get_binary_operator_for_token(&self, token: &Token<&'b str>) -> Option<BinaryOperator> {
        match token {
            Token::Keyword(ref k) => match k {
                Keyword::In => Some(BinaryOperator::In),
                Keyword::InstanceOf => Some(BinaryOperator::InstanceOf),
                _ => None
            },
            Token::Punct(ref p) => match p {
                Punct::Ampersand => Some(BinaryOperator::BitAnd),
                Punct::Asterisk => Some(BinaryOperator::Multiply),
                Punct::Period => Some(BinaryOperator::Dot),
                Punct::Comma => Some(BinaryOperator::Comma),
                Punct::GreaterThan => Some(BinaryOperator::GreaterThan),
                Punct::LessThan => Some(BinaryOperator::LessThan),
                Punct::Plus => Some(BinaryOperator::Plus),
                Punct::Dash => Some(BinaryOperator::Minus),
                Punct::Percent => Some(BinaryOperator::Mod),
                Punct::Pipe => Some(BinaryOperator::BitOr),
                Punct::Caret => Some(BinaryOperator::BitXor),
                Punct::ForwardSlash => Some(BinaryOperator::Divide),
                Punct::TripleEqual => Some(BinaryOperator::StrictEqual),
                Punct::BangDoubleEqual => Some(BinaryOperator::StrictNotEqual),
                Punct::TripleGreaterThan => Some(BinaryOperator::UnsignedRightShift),
                Punct::DoubleAmpersand => Some(BinaryOperator::LogicalAnd),
                Punct::DoublePipe => Some(BinaryOperator::LogicalOr),
                Punct::DoubleEqual => Some(BinaryOperator::Equal),
                Punct::BangEqual => Some(BinaryOperator::NotEqual),
                Punct::DoubleLessThan  => Some(BinaryOperator::LeftShift),
                Punct::DoubleGreaterThan => Some(BinaryOperator::RightShift),
                Punct::GreaterThanEqual => Some(BinaryOperator::GreaterThanEqual),
                Punct::LessThanEqual => Some(BinaryOperator::LessThanEqual),
                Punct::DoubleAsterisk => Some(BinaryOperator::Exponent),
                _ => None
            },
            _ => None
        }
    }

    fn get_assignment_operator_for_token(&self, token: &Token<&'b str>) -> Option<AssignmentOperator> {
        match token {
            Token::Punct(ref p) => match p {
                Punct::AmpersandEqual => Some(AssignmentOperator::BitAndAssign),
                Punct::AsteriskEqual => Some(AssignmentOperator::MultiplyAssign),
                Punct::CaretEqual => Some(AssignmentOperator::BitXorAssign),
                Punct::DashEqual => Some(AssignmentOperator::MinusAssign),
                Punct::DoubleAsteriskEqual => Some(AssignmentOperator::ExponentAssign),
                Punct::DoubleGreaterThanEqual => Some(AssignmentOperator::RightShiftAssign),
                Punct::DoubleLessThanEqual => Some(AssignmentOperator::LeftShiftAssign),
                Punct::ForwardSlashEqual => Some(AssignmentOperator::DivideAssign),
                Punct::PercentEqual => Some(AssignmentOperator::ModAssign),
                Punct::PipeEqual => Some(AssignmentOperator::BitOrAssign),
                Punct::PlusEqual => Some(AssignmentOperator::PlusAssign),
                Punct::TripleGreaterThanEqual => Some(AssignmentOperator::UnsignedRightShiftAssign),
                Punct::Equal => Some(AssignmentOperator::Assign),
                _ => None
            },
            _ => None
        }
    }

    /// parse a binary operator.
    fn parse_binary_op(&mut self,
        op: &Token<&str>,
        lhs: &TypedExpr,
        rhs: &TypedExpr,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = SourceLocation::new(lhs.loc.start.clone(), rhs.loc.end.clone());

        //first see if it's a simple binary operator
        let o_bin_op = self.get_binary_operator_for_token(&op);
        match o_bin_op {
            Some(bin_op) => {
                //type check the binary operator
                let o_bin_op_type_cast = types::get_binary_op_type_cast(bin_op.get_op_type(), &lhs.r#type, &rhs.r#type);
                match o_bin_op_type_cast {
                    Some(bin_op_type_cast) => {
                        let o_cast = create_cast(&bin_op_type_cast.lhs_type, lhs, &bin_op_type_cast.lhs_type_cast);
                        let lhs_out = match o_cast {
                            Some(cast) => Box::new(cast),
                            None => {
                                parser_context.errors.push(Error::TypeFailureBinaryOperator(loc, format!("{}", lhs.r#type), format!("{}", rhs.r#type)));
                                Box::new(lhs.clone())
                            }
                        };

                        let o_cast = create_cast(&bin_op_type_cast.rhs_type, rhs, &bin_op_type_cast.rhs_type_cast);
                        let rhs_out = match o_cast {
                            Some(cast) => Box::new(cast),
                            None => {
                                parser_context.errors.push(Error::TypeFailureBinaryOperator(loc, format!("{}", lhs.r#type), format!("{}", rhs.r#type)));
                                Box::new(rhs.clone())
                            }
                        };
                        
                        TypedExpr{expr: Expr::BinaryOperator(BinaryOperatorApplication{op: bin_op, lhs: lhs_out, rhs: rhs_out}), r#type: bin_op_type_cast.out_type, is_const: true, loc: loc}
                    },
                    None => {
                        parser_context.errors.push(Error::TypeFailureBinaryOperator(loc, format!("{}", lhs.r#type), format!("{}", rhs.r#type)));
                        TypedExpr{expr: Expr::BinaryOperator(BinaryOperatorApplication{op: bin_op, lhs: Box::new(lhs.clone()), rhs: Box::new(rhs.clone())}), r#type: Type::Unknown, is_const: true, loc: loc}
                    }
                }
            },

            //it's not a regular binary op; so let's see if it's an assignment operator
            None => {
                let o_ass_op = self.get_assignment_operator_for_token(&op);
                match o_ass_op {
                    Some(ass_op) => {
                        //get an l-value. This will do const checking
                        let o_l_value = lhs.as_l_value();
                        match o_l_value {
                            None => {
                                parser_context.errors.push(Error::NotAnLValue);
                                lhs.clone()
                            },
                            Some(l_value) => {
                                //now type check the assignment operator. Note that we only need a type check from the rhs
                                let o_bin_op_type_cast = types::get_binary_op_type_cast(ass_op.get_op_type(), &lhs.r#type, &rhs.r#type);
                                match o_bin_op_type_cast {
                                    Some(bin_op_type_cast) => {
                                        let o_cast = create_cast(&bin_op_type_cast.rhs_type, rhs, &bin_op_type_cast.rhs_type_cast);
                                        let rhs_out = match o_cast {
                                            Some(cast) => Box::new(cast),
                                            None => {
                                                parser_context.errors.push(Error::TypeFailureBinaryOperator(loc, format!("{}", lhs.r#type), format!("{}", rhs.r#type)));
                                                Box::new(rhs.clone())
                                            }
                                        };
                                        TypedExpr{expr: Expr::Assignment(l_value, ass_op, rhs_out), r#type: bin_op_type_cast.out_type, is_const: false, loc: loc}
                                    },
                                    None => {
                                        parser_context.errors.push(Error::TypeFailureBinaryOperator(loc, format!("{}", lhs.r#type), format!("{}", rhs.r#type)));
                                        TypedExpr{expr: Expr::Assignment(l_value, ass_op, Box::new(rhs.clone())), r#type: Type::Unknown, is_const: false, loc: loc}
                                    }
                                }
                            }
                        }
                    }, 
                    None => {
                        parser_context.errors.push(Error::NotYetImplemented(loc, format!("{:#?}", op)));
                        lhs.clone()
                    }
                }
            }
        }
    }

    /// From wikipedia, https://en.wikipedia.org/wiki/Operator-precedence_parser
    /*
    parse_expression_1 (lhs, min_precedence)
        lookahead := peek next token
        while lookahead is a binary operator whose precedence is >= min_precedence
            op := lookahead
            advance to next token
            rhs := parse_primary ()
            lookahead := peek next token
            while lookahead is a binary operator whose precedence is greater
                    than op's, or a right-associative operator
                    whose precedence is equal to op's
                rhs := parse_expression_1 (rhs, lookahead's precedence)
                lookahead := peek next token
            lhs := the result of applying op with operands lhs and rhs
        return lhs
    */
    fn parse_expr_1(&mut self, 
        init_lhs: TypedExpr, 
        min_precedence: i32, 
        allow_comma: bool,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        // lookahead := peek next token
        let mut lhs = init_lhs;
        let lookahead_item = self.peek_next_item();
        let mut lookahead = lookahead_item.token;
        
        // while lookahead is a binary operator whose precedence is >= min_precedence
        loop {
            let o_lookahead_data = lookahead.get_binary_operator_data(allow_comma);
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
                        let r_rhs = self.parse_primary(parser_func_context, parser_context);
                        if r_rhs.is_err() { return Err(r_rhs.unwrap_err()) }; 
                        let mut rhs = r_rhs?;
                        // lookahead := peek next token
                        let lookahead_item = self.peek_next_item();
                        lookahead = lookahead_item.token;
                        // while lookahead is a binary operator whose precedence is greater
                        // than op's, or a right-associative operator
                        // whose precedence is equal to op's
                        loop {
                            let o_lookahead_data = lookahead.get_binary_operator_data(allow_comma);
                            match o_lookahead_data {
                                None => break,
                                Some(lookahead_data) => {
                                    if lookahead_data.precedence > op_precedence || (lookahead_data.association == Association::Right && lookahead_data.precedence == op_precedence) {
                                        //rhs := parse_expression_1 (rhs, lookahead's precedence)
                                        let new_rhs = self.parse_expr_1(rhs, lookahead_data.precedence, allow_comma, parser_func_context, parser_context);
                                        if new_rhs.is_err() { return Err(new_rhs.unwrap_err()) }; 
                                        rhs = new_rhs?;
                                        //lookahead := peek next token
                                        let lookahead_item = self.peek_next_item();
                                        lookahead = lookahead_item.token;
                                    } else {
                                        break;
                                    }
                                }
                            }                                
                        }
                        lhs = self.parse_binary_op(&op, &lhs, &rhs, parser_context);
                    } else {
                        break;
                    }
                }
            }
        };
        return Ok(lhs);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn add_test() {
        let add = "export function addd(x: number, y: number, z: number): number {
            let a = 1;
            return x + y + z + a;
        }";

        let mut parser = Parser::new(add).unwrap();
        let script = parser.parse().unwrap();
        println!("{:#?}", script);
    }
}