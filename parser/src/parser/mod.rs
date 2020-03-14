extern crate ress;
extern crate log;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;

pub use errs::Error;
use errs::prelude::*;

use std::{mem::replace};

use std::collections::{HashMap, HashSet};

mod parser_unsafe;
mod parser_int;
mod parser_func;
mod parser_type;
mod parser_option;
mod parser_phase_1;
mod parser_op;

use crate::{ParserContext, ScopedVar};
use crate::ParserFuncContext;
use crate::Res;
use crate::{assert_punct, assert_ok,assert_ident,assert_next,assert_semicolon, expect_punct, expect_next, expect_ident, expect_string_literal, expect_keyword, StartFuncType, expect_ok};
use crate::{try_create_cast, cast_typed_expr};
use crate::parser::parser_op::{get_unary_operator_data, Fix, get_binary_operator_data, Association, get_op_type_for_unop};
use crate::Commitment;
use crate::Importer;

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
}

impl Context {

}

impl<> Default for Context<> {
    fn default() -> Self {
        Self {
            in_iteration: false,
            has_line_term: false,
        }
    }
}

enum ImportFilter{
    All,
    Named(Vec<String>)
}

fn filter_imports(exports: Exports, imports: &ImportFilter) -> Exports {
    match imports {
        ImportFilter::All => (exports),
        ImportFilter::Named(_) => {panic!()}
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
        };
        let _ = ret.next_item()?;
        Ok(ret)
    }

    /// Consume a parser context, populate it.
    fn parse_internal(
        &mut self, 
        start_func_name: &String,
        start_func_type: StartFuncType,
        parser_context: &mut ParserContext,
        importer: &mut dyn Importer,
    ) {
        let mut init_body = vec![];
        
        let mut fake_parser_func_context = ParserFuncContext::new();

        loop {
            if self.look_ahead.token.is_eof() {
                break;
            } else {
                self.parse_global_statement(&mut init_body, &mut fake_parser_func_context, parser_context, importer);
            }
        }

        let start_function = Func{ 
            decl: FuncDecl{
                name: start_func_name.clone(), return_type: Type::RealVoid, args: vec![], 
                export: start_func_type == StartFuncType::WASMCallCtors, generic_impl: false, type_guard: None
            },
            body: Some(TypedExpr{
                expr: Expr::Block(init_body),
                r#type: Type::RealVoid,
                is_const: true,
                loc: SourceLocation::new(Position::new(0, 0), Position::new(0, 0))
            }),
            local_vars: vec![], closure: vec![], local_var_map: HashMap::new()
        };

        parser_context.func_decls.push(start_function);

        if !fake_parser_func_context.closure.is_empty() {
            parser_context.errors.push(Error::Other(String::from("found closure reference when not expecting it")));
        }
    }

    /// Produces a full parse.
    pub fn parse_full(&mut self, 
        is_unsafe: bool,
        importer: &mut dyn Importer,
        module_name: &String,
        start_func_type: StartFuncType,
        file_name: &String,
    ) -> Result<AST, Vec<Error>> {
        let mut parser_context = ParserContext::new(is_unsafe, file_name);
        
        let start_func_name = match start_func_type{
            StartFuncType::WASMCallCtors => String::from("__wasm_call_ctors"),
            StartFuncType::Start => format!("_start_{}", module_name)
        };
        self.parse_internal(&start_func_name, start_func_type, &mut parser_context, importer);
        if parser_context.errors.is_empty() || parser_context.errors.iter().all(|e| e.is_warning()) {
            Ok(AST{start: start_func_name, global_decls: parser_context.global_decls, global_imports: parser_context.global_imports,
                func_decls: parser_context.func_decls, func_imports: parser_context.func_imports, generic_func_decls: parser_context.generic_func_decls,
                type_map: parser_context.type_map
            })
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

    /// Peek at the next location, without changing state.
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

    fn parse_type_decl_constraint(&mut self, 
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        let err_out = TypeConstraint::None;
        let next = expect_next!(self, parser_context, err_out);
        let loc = next.location.clone();
        let token = &next.token;
        match token {
            Token::Keyword(k) => {
                match k {
                    Keyword::UnsafeStruct => {
                        self.skip_next_item();
                        TypeConstraint::IsAStruct
                    },
                    _ => {
                        parser_context.push_err(Error::UnrecognizedTypeArgConstraint(loc));
                        err_out
                    }
                }
            }, 
            _ => {
                parser_context.push_err(Error::UnrecognizedTypeArgConstraint(loc));
                err_out
            }
        }
    }

    fn parse_type_decl_args(&mut self, 
        parser_context: &mut ParserContext
    ) -> Vec<TypeArg> {
        let mut out: Vec<TypeArg> = Vec::new();
        expect_punct!(self, parser_context, Punct::LessThan);

        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::Punct(Punct::GreaterThan) => { self.skip_next_item(); return out; },
                Token::Ident(n) => {
                    self.skip_next_item();

                    let next = self.peek_next_item();
                    let token = &next.token;
                    
                    if token.matches_punct(Punct::Colon) {
                        self.skip_next_item();
                        let constraint = self.parse_type_decl_constraint(parser_context);
                        out.push(TypeArg{name: n.to_string(), constraint: constraint});
                    } else {
                        out.push(TypeArg{name: n.to_string(), constraint: TypeConstraint::None});
                    }
                    if token.matches_punct(Punct::Comma) {
                        self.skip_next_item();
                    }
                },
                _ => {
                    self.skip_next_item();
                    parser_context.push_err(Error::UnrecognizedTypeArg(next.location.clone()));
                }
            }
        }
    }

    
    /// register params in the local scope
    fn register_params(&mut self, 
        arg_list: &Vec<FuncArg>, 
        parser_func_context: &mut ParserFuncContext, 
        parser_context: &mut ParserContext,
    ) {
        for arg in arg_list {
            let internal_name = parser_context.get_unique_name(&arg.name);
            parser_context.add_var(&arg.name, &internal_name, &arg.r#type, false);
            //todo: constant params
            //todo: default params
            let idx = parser_func_context.local_vars.len();
            parser_func_context.local_var_map.insert(internal_name.clone(), idx as u32);
            parser_func_context.local_vars.push(
                LocalVar{internal_name: internal_name, r#type: arg.r#type.clone(), closure_source: false, arg: true}
            );
        }
    }

    fn parse_import_decl(&mut self,
        parser_context: &mut ParserContext,
        importer: &mut dyn Importer,
    ) ->Option<()> {
        self.skip_next_item();
        expect_punct!(self, parser_context, Punct::OpenBrace);
        let next = self.peek_next_item();
        self.skip_next_item();
        let import_filter: ImportFilter = match &next.token {
            Token::Punct(p) => {
                if *p == Punct::Asterisk {
                    ImportFilter::All
                } else {
                    parser_context.push_err(Error::UnexpectedToken(
                        next.location.clone(),
                        format!("Expected '*' or ident; found {:?}", next.token),
                    ));
                    ImportFilter::Named(vec![])
                }
            },
            Token::Ident(_) => {
                parser_context.push_err(Error::NotYetImplemented(next.location.clone(), String::from("Individual imports")));
                ImportFilter::Named(vec![])
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(
                    next.location.clone(),
                    format!("Expected '*' or ident; found {:?}", next.token),
                ));
                ImportFilter::Named(vec![])
            }
        };
        expect_punct!(self, parser_context, Punct::CloseBrace);

        //peek for 'as'?

        expect_keyword!(self, parser_context, Keyword::From);

        let next = expect_next!(self, parser_context, None);
        let id_string = expect_string_literal!(next, parser_context, "Expecting path name to import");
        if id_string.starts_with(".") {
            let r_imports = importer.import(&id_string, &(parser_context.file_name));
            let imports = match r_imports {
                Ok(imports) => imports,
                Err(e) => {
                    parser_context.push_err(Error::ImportFailed(next.location.clone(), e));
                    return None;
                }
            };
            let exports = imports.exports;
            let exports = filter_imports(exports, &import_filter);
            let namespace = imports.module_name;
            for g in &exports.global_decls {
                let import_name = format!("{}.{}", namespace, g.name);
                let decl = GlobalVariableImport{name: import_name.clone(), r#type: g.r#type.clone(), constant: g.constant, export: false};
                parser_context.global_imports.push(decl.clone());
            }

            for g in &exports.global_imports {
                let decl = GlobalVariableImport{name: g.name.clone(), r#type: g.r#type.clone(), constant: g.constant, export: false};
                parser_context.global_imports.push(decl.clone());
            }

            for f in &exports.func_decls {
                let import_name = format!("{}.{}", namespace, f.name);
                parser_context.func_imports.push(
                    FuncDecl{name: import_name.clone(), return_type: f.return_type.clone(), args: f.args.clone(), export: false, 
                        generic_impl: false, type_guard: f.type_guard.clone()},
                ); 
            }

            for f in &exports.generic_func_decls {
                let import_name = format!("{}.{}", namespace, f.func.decl.name);
                parser_context.generic_func_decls.push(
                    GenericFunc{type_args: f.type_args.clone(), func: Func{
                        decl: FuncDecl{name: import_name.clone(), return_type: f.func.decl.return_type.clone(), 
                            args: f.func.decl.args.clone(), export: false, generic_impl: false, type_guard: f.func.decl.type_guard.clone()},
                        body: f.func.body.clone(),
                        local_vars: f.func.local_vars.clone(),
                        closure: f.func.closure.clone(),
                        local_var_map: f.func.local_var_map.clone()
                    }}
                ); 
            }

            for f in &exports.func_imports {
                parser_context.func_imports.push(
                    FuncDecl{name: f.name.clone(), return_type: f.return_type.clone(), args: f.args.clone(), export: false, 
                        generic_impl: false, type_guard: f.type_guard.clone()},
                ); 
            }

            parser_context.import_namespace_map.insert(namespace.clone(), imports.unique_name.clone());
        } else {
            //it's an import from an external file
            parser_context.push_err(Error::NotYetImplemented(next.location.clone(), String::from("Import external projects")));
        }

        Some(())
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
                Keyword::Fn => {
                    let o_func = self.main_parse_function_decl(true, fake_parser_func_context, parser_context);
                    match o_func {
                        Some(f) => init_body.push(f),
                        _ => {}
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
                Keyword::UnsafeStruct => {
                    let struct_decl = self.parse_struct_decl(true, parser_context);
                    match struct_decl {
                        Ok(c) => init_body.push(c),
                        Err(e) => parser_context.errors.push(e)
                    };
                }
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

        //check to see if this shadows a pre-existing global
        if global && parser_context.get_global_decl(&id.to_string()).is_some() {
            parser_context.push_err(Error::DuplicateGlobalVariable(loc.clone(), id.to_string().clone()));
        }

        let next = self.peek_next_item();
        let token = &next.token;
        let mut var_type = match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                self.parse_type(parser_context)
            },
            Token::Punct(Punct::Equal) => {
                Type::Undeclared
            },
            _ => return self.unexpected_token_error(next.span, &next.location, "variable must have a type or a value")
        };

        assert_punct!(self, Punct::Equal);
        let init = self.parse_expr(parser_func_context, parser_context);
        if init.is_err() { return Err(init.unwrap_err()) }; 
        let mut init = init?;

        loc.end = init.loc.end.clone();

        if var_type.is_undeclared() {
            var_type = init.r#type.clone();
        } else if var_type != init.r#type {
            let o_cast_expr = try_create_cast(&var_type, &init, true);
            match o_cast_expr {
                Some(cast_expr) => { init = cast_expr; },
                None => { 
                    parser_context.errors.push(Error::TypeFailureVariableCreation(init.loc, format!("{}", var_type), format!("{}", init.r#type)));
                }
            }
        }

        assert_semicolon!(self);
        let name = id.to_string();
        if !global {
            let internal_name = parser_context.get_unique_name(&name);
            parser_context.add_var(&name, &internal_name, &var_type, constant);
            let idx = parser_func_context.local_vars.len();
            parser_func_context.local_vars.push(
                LocalVar{internal_name: internal_name.clone(), r#type: 
                    var_type.clone(), closure_source: false, arg: false}
            );
            parser_func_context.local_var_map.insert(internal_name.clone(), idx as u32);
            Ok(TypedExpr{expr: Expr::VariableInit{internal_name: internal_name.clone(), init: Box::new(Some(init))}, is_const: constant, r#type: Type::RealVoid, loc: loc})
        } else {
            let decl = GlobalVariableDecl{name: name.clone(), r#type: var_type, constant, init: Some(init), export};
            parser_context.global_decls.push(decl.clone());
            
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
        importer: &mut dyn Importer,
    ) {
        let next = self.peek_next_item();
        let token = &next.token;
        match &token {
            Token::Keyword(ref k) => match k {
                Keyword::Import => {self.parse_import_decl(parser_context, importer);},
                Keyword::Export => self.parse_export_decl(init_body, fake_parser_func_context, parser_context),
                Keyword::Fn => {
                    let o_func = self.main_parse_function_decl(false, fake_parser_func_context, parser_context);
                    match o_func {
                        Some(f) => init_body.push(f),
                        _ => {}
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
                Keyword::UnsafeStruct => {
                    let struct_decl = self.parse_struct_decl(false, parser_context);
                    match struct_decl {
                        Ok(c) => init_body.push(c),
                        Err(e) => parser_context.errors.push(e)
                    };
                },
                _ => { parser_context.errors.push(self.unexpected_token_error_raw(next.span, &next.location, "expecting valid statement")); self.skip_next_item(); }
            },
            _ => {
                // if we don't know what this statement is, parse it as an expr
                let expr = self.parse_expr(fake_parser_func_context, parser_context);

                // ugh this is like a hundred million lines to check the semi-colon
                let next = self.next_item();
                match next {
                    Ok(next) => {
                        if self.context.has_line_term {
                        } else if next.token.matches_punct(Punct::SemiColon) {
                            self.skip_next_item();
                        } else {
                            parser_context.errors.push(self.expected_token_error_raw(&next, &[&";"]));
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
        push_scope: bool,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let mut out: Vec<TypedExpr> = vec![];
        let next = self.peek_next_item();
        let mut loc = next.location.clone();
        
        let token = &next.token;
        
        if token.matches_punct(Punct::OpenBrace) {
            self.skip_next_item();
                
            let mut next = self.peek_next_item();
            let mut token = &next.token;
            if push_scope {
                parser_context.push_block_scope();
            }

            while !token.matches_punct(Punct::CloseBrace) {
                let r_stmt = self.parse_statement(parser_func_context, parser_context);
                if r_stmt.is_ok() {
                    out.push(r_stmt.unwrap());
                } else {
                    parser_context.push_err(r_stmt.unwrap_err());
                }
                
                next = self.peek_next_item();
                loc.extend_right(&next.location);
                token = &next.token;
            }
            self.skip_next_item();
            if push_scope {
                parser_context.pop_block_scope();
            }
        } else {
            let r_stmt = self.parse_statement(parser_func_context, parser_context);
            if r_stmt.is_ok() {
                let stmt = r_stmt.unwrap();
                loc.extend_right(&stmt.loc);
                out.push(stmt);
            } else {
                parser_context.push_err(r_stmt.unwrap_err());
            }
        } 

        let out_type = out.last().map_or(Type::RealVoid, |x| x.r#type.clone());
        TypedExpr{expr: Expr::Block(out), is_const: true, r#type: out_type, loc: loc}
    }

    fn parse_if(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        //get condition, check it
        expect_punct!(self, parser_context, Punct::OpenParen);
        let condition = self.parse_expr(parser_func_context, parser_context);
        let err_ret = TypedExpr{expr: Expr::NoOp, r#type: Type::Unknown, is_const: true, loc: loc.clone()};
        expect_ok!(condition, parser_context, err_ret);
        if condition.r#type != Type::Boolean {
            parser_context.push_err(Error::TypeFailure(condition.loc.clone(), Type::Boolean,  condition.r#type.clone()));
        }
        expect_punct!(self, parser_context, Punct::CloseParen);

        //is the condition a type guard?
        let (o_type_guard, v) = match &condition.expr {
            Expr::StaticFuncCall(s, fd, v) => {
                let o_type_guard = &fd.type_guard;
                if o_type_guard.is_some() {
                    (o_type_guard.clone(), v.clone())
                } else {
                    (None, vec![])
                }
            },
            _ => (None, vec![])
        };
        
        if o_type_guard.is_some() {
            let type_guard = o_type_guard.unwrap();
            let o_branch = type_guard.branches.iter().find(|b| b.literal.expr == Expr::BoolLiteral(true));
            parser_context.push_block_scope();
            //parser_context.add_var(var_name: &String, internal_var_name: &String, r#type: &Type, constant: bool)
        }

        let then_block = self.parse_block(true, parser_func_context, parser_context);

            
        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_keyword(Keyword::Else) {
            self.skip_next_item();
            let else_block = self.parse_block(true, parser_func_context, parser_context);
            let then_block_type = then_block.r#type.clone();
            loc.end = else_block.loc.end.clone();

            if then_block.r#type != else_block.r#type {
                //first try casting then to else
                let then_to_else_cast = try_create_cast(&else_block.r#type, &then_block, true);
                match then_to_else_cast {
                    None => {
                        //nope, not that way. Try the other
                        let else_to_then_cast = try_create_cast(&then_block.r#type, &else_block, true);
                        match else_to_then_cast {
                            None => {
                                //warn that we can't figure out the type, make it the top type
                                parser_context.push_err(Error::TypeFailureIf(loc.clone(), then_block.r#type.clone(), else_block.r#type.clone()));
                                TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(else_block)), is_const: true, r#type: Type::Unknown, loc: loc}
                            },
                            Some(new_else_block) => {
                                TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(new_else_block)), is_const: true, r#type: then_block_type, loc: loc}
                            }
                        }
                    }, 
                    Some(new_then_block) => {
                        let new_then_block_type = new_then_block.r#type.clone();
                        TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(new_then_block), Box::new(else_block)), is_const: true, r#type: new_then_block_type, loc: loc}
                    }  
                }
            } else {            
                TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(else_block)), is_const: true, r#type: then_block_type, loc: loc}
            }
        } else {
            loc.end = then_block.loc.end.clone();
            TypedExpr{expr: Expr::IfThen(Box::new(condition), Box::new(then_block)), is_const: true, r#type: Type::RealVoid, loc: loc}
        }
    }

    fn parse_while(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        assert_punct!(self, Punct::OpenParen);
        let condition = self.parse_expr(parser_func_context, parser_context);
        assert_ok!(condition);
        if condition.r#type != Type::Boolean {
            parser_context.push_err(Error::TypeFailure(condition.loc.clone(), Type::Boolean, condition.r#type.clone()));
        }
        assert_punct!(self, Punct::CloseParen);

        let old_in_iteration = self.context.in_iteration;
        self.context.in_iteration = true;
        let block = self.parse_block(true, parser_func_context, parser_context);
        self.context.in_iteration = old_in_iteration;
        loc.end = block.loc.end.clone();

        Ok(TypedExpr{expr: Expr::While(Box::new(condition), Box::new(block)), is_const: true, r#type: Type::RealVoid, loc: loc})
    }

    fn parse_class_decl(&mut self,
        export: bool,
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
            let type_args = self.parse_type_decl_args(parser_context);
            parser_context.push_type_scope(&type_args);
            parser_context.errors.push(Error::NotYetImplemented(next.location.clone(), String::from("generic classes")));
        } else {
            parser_context.push_empty_type_scope();
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
                    members.push(ClassMember{name: i.to_string(), r#type: member_type.clone(), privacy: Privacy::Public});
                },
                _ => {
                    return self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "expecting '}' or member")
                }
            }
        }

        parser_context.pop_type_scope();

        parser_context.type_map.insert(id.to_string(), TypeDecl::Class{class_type: ClassType{members: members}, export, name: id.to_string()});

        Ok(TypedExpr{expr: Expr::ClassDecl(id.to_string()), is_const: true, r#type: Type::RealVoid, loc: loc})
    }

    /// A statement is something you can't assign to (like break or a variable declaration) or a true expr.
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
                    let loc = next.location.clone();
                    self.skip_next_item();
                    let next = self.peek_next_item();
                    
                    let token = &next.token;

                    if token.matches_punct(Punct::SemiColon) || self.context.has_line_term {
                        // check the return type of this expression versus the function return type.
                        let check_return_type = if parser_func_context.given_func_return_type == Type::Undeclared {
                            if parser_func_context.implied_func_return_type == Type::Undeclared {
                                parser_func_context.implied_func_return_type = Type::RealVoid;
                            }
                            parser_func_context.implied_func_return_type.clone()
                        } else {
                            parser_func_context.given_func_return_type.clone()
                        };

                        if check_return_type != Type::RealVoid {
                            parser_context.errors.push(Error::TypeFailureReturn(loc.clone(), check_return_type.clone(), Type::RealVoid));
                        }
                        if !self.context.has_line_term {
                            self.skip_next_item();
                        }
                        
                        Ok(TypedExpr{expr: Expr::Return(Box::new(None)), is_const: true, r#type: Type::Never, loc: loc})
                    } else {
                        let expr = self.parse_expr(parser_func_context, parser_context);
                        assert_semicolon!(self);
                        assert_ok!(expr);
                        // return type checking
                        let expr_type = expr.r#type.clone();
                        let check_return_type = if parser_func_context.given_func_return_type == Type::Undeclared {
                            if parser_func_context.implied_func_return_type == Type::Undeclared {
                                parser_func_context.implied_func_return_type = expr_type.clone();
                            }
                            parser_func_context.implied_func_return_type.clone()
                        } else {
                            parser_func_context.given_func_return_type.clone()
                        };

                        Ok(TypedExpr{expr: Expr::Return(Box::new(Some(
                            cast_typed_expr(&check_return_type, Box::new(expr), false, parser_context)
                        ))), is_const: true, r#type: Type::Never, loc: loc})
                    }
                },
                
                Keyword::Class => {
                    self.parse_class_decl(false, parser_context)
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
                    let expr = self.parse_expr(parser_func_context, parser_context);
                    expr
                }
            },

            _ => {
                // if we don't know what this statement is, parse it as an expr
                let expr = self.parse_expr(parser_func_context, parser_context);
                assert_semicolon!(self);
                expr
            }
        }
    }

    /// Parse an array literal into a Vec.
    fn parse_array_literal_to_vec(&mut self,
        for_type: &Type,
        loc_in: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<Vec<TypedExpr>> {
        let mut loc = loc_in.clone();

        assert_punct!(self, Punct::OpenBracket);

        let inner_type: &Type = match for_type{
            Type::UnsafeArray(i) => {
                &**i
            },
            _ => { 
                parser_context.errors.push(Error::CantConstructUsingArrayLiteral(loc.clone(), for_type.clone()));
                &Type::RealVoid
            }
        };

        let mut out: Vec<TypedExpr> = vec![];
        let mut idx = 0;

        loop {
            let next_item = self.peek_next_item();
            let token = next_item.token;

            match token {
                Token::Punct(p) => {
                    match p {
                        Punct::CloseBracket => {
                            self.skip_next_item();
                            loc.end = next_item.location.end.clone();
                            break;
                        },
                        _ => {}
                    }
                },
                _ => {}
            }

            let v = self.parse_expr(parser_func_context, parser_context);
            assert_ok!(v);
            let o_cast_expr = try_create_cast(inner_type, &v, true);
            match o_cast_expr {
                Some(cast_expr) => { out.push(cast_expr) },
                None => { 
                    parser_context.errors.push(Error::TypeFailureMemberCreation(next_item.location, idx.to_string().clone(), inner_type.clone(), v.r#type.clone()));
                }
            }
            idx += 1;
            
            let lookahead = self.peek_next_token();
            match lookahead {
                Token::Punct(p) => {
                    match p {
                        Punct::CloseBracket => {
                        },
                        Punct::Comma => {
                            self.skip_next_item();
                        },
                        _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ',' or ']'"),
                    }
                },
                _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ',' or ']'"),
            }
        }

        Ok(out)
    }

    /// Parse an object literal into a Vec that can be used by other parts of the parser.
    /// It's a Vec rather than a HashMap because there may be order to the initialization.
    fn parse_object_literal_to_vec(&mut self,
        for_type: &Type,
        loc_in: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<Vec<ObjectLiteralElem>> {
        let mut loc = loc_in.clone();

        assert_punct!(self, Punct::OpenBrace);

        //this is what we want
        let member_map: HashMap<String, Type> = match for_type {
            Type::UnsafeStruct{name: struct_name} => {
                let tm_entry = parser_context.type_map.get(struct_name).unwrap();
                match tm_entry {
                    TypeDecl::Struct{struct_type, under_construction, export: _, name: _} => { 
                        if *under_construction {
                            parser_context.errors.push(Error::RecursiveTypeDefinition(loc.clone()));
                        }
                        struct_type.get_member_type_map()
                    },
                    _ => unreachable!()
                }
            }, 
            _ => {
                parser_context.errors.push(Error::CantConstructUsingObjectLiteral(loc.clone(), for_type.clone()));
                HashMap::new()
            }
        };

        //this is what we got
        let mut got: HashSet<String> = HashSet::new();

        let mut out: Vec<ObjectLiteralElem> = vec![];

        loop {
            let next_item = self.next_item();
            assert_ok!(next_item);
            let token = next_item.token;

            match token {
                Token::Punct(p) => {
                    match p {
                        Punct::CloseBrace => {
                            loc.end = next_item.location.end.clone();
                            break;
                        },
                        _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ',' or '}'"),
                    }
                },
                Token::Ident(i) => {
                    assert_punct!(self, Punct::Colon);
                    let v = self.parse_expr(parser_func_context, parser_context);
                    assert_ok!(v);

                    if got.contains(&i.to_string()) {
                        parser_context.errors.push(Error::ObjectDuplicateMember(next_item.location, i.to_string().clone()));
                    }
                    
                    got.insert(i.to_string());

                    let o_member_map_elem = member_map.get(&i.to_string());
                    match o_member_map_elem {
                        None => {
                            parser_context.errors.push(Error::ObjectHasNoMember(next_item.location, for_type.clone(), i.to_string().clone()));
                        },
                        Some(member_map_elem) => {
                            let o_cast = try_create_cast(member_map_elem, &v, true);
                            match o_cast {
                                None => {
                                    parser_context.errors.push(Error::TypeFailureMemberCreation(next_item.location, i.to_string().clone(), member_map_elem.clone(), v.r#type.clone()));
                                },
                                Some(cast) => {
                                    out.push(ObjectLiteralElem{name: i.to_string(), value: cast});
                                }
                            }
                        }
                    }

                    let lookahead = self.peek_next_token();
                    match lookahead {
                        Token::Punct(p) => {
                            match p {
                                Punct::CloseBrace => {
                                },
                                Punct::Comma => {
                                    self.skip_next_item();
                                },
                                _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ',' or '}'"),
                            }
                        },
                        _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ',' or '}'"),
                    }
                },
                _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ',' or '}'"),
            }
        }
        
        //make sure that we have everything we want
        if got.len() != member_map.len() {
            //ugh irritating 
            let keys = member_map.keys();
            for key in keys {
                if !got.contains(key) {
                    parser_context.errors.push(Error::ObjectMissingMember(loc.clone(), key.to_string().clone()));
                }
            }
        }

        Ok(out)
    }

    /// This parses an object literal and uses it to construct something. As a side effect it
    /// creates a variable called '__scratch_malloc' which is supposed to hold the call to malloc.
    /// This is perhaps not brilliant
    fn parse_object_literal_constructor(&mut self,
        for_type: &Type,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let loc = self.peek_next_location();
        
        let scratch_malloc = String::from("__scratch_malloc");
        let scratch_malloc_type = Type::UnsafePtr;
        parser_context.add_var(&scratch_malloc, &scratch_malloc, &scratch_malloc_type, false);
        let idx = parser_func_context.local_vars.len();
        parser_func_context.local_vars.push(
            LocalVar{internal_name: scratch_malloc.clone(), 
                r#type: scratch_malloc_type,
                closure_source: false, arg: false}
        );
        parser_func_context.local_var_map.insert(scratch_malloc.clone(), idx as u32);
        
        let out = self.parse_object_literal_to_vec(for_type, &loc, parser_func_context, parser_context);
        assert_ok!(out);

        Ok(TypedExpr{expr: Expr::ConstructFromObjectLiteral(for_type.clone(), out), is_const: true, loc: loc, r#type: for_type.clone()})
    }

    fn parse_new(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        self.skip_next_item();

        let type_to_construct = self.parse_type(parser_context);

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        match lookahead {
            Token::Punct(p) => {
                match p {
                    Punct::OpenBrace => self.parse_object_literal_constructor(&type_to_construct, parser_func_context, parser_context),
                    _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "{"),
                }
            },
            _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "{"),
        }
    }

    fn parse_struct_component(&mut self,
        lhs: &TypedExpr,
        component: &String,
        struct_type: &StructType,
        loc: &SourceLocation,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let o_mem = struct_type.members.iter().find(|x| x.name == *component);
        match o_mem {
            None => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), lhs.r#type.clone(), component.clone()));
                TypedExpr{
                    expr: Expr::NamedMember(Box::new(lhs.clone()), component.clone()),
                    r#type: Type::Undeclared,
                    is_const: false,
                    loc: loc.clone()
                }
            },
            Some(mem) => {
                TypedExpr{
                    expr: Expr::NamedMember(Box::new(lhs.clone()), component.clone()),
                    r#type: mem.r#type.clone(),
                    is_const: false,
                    loc: loc.clone()
                }
            }
        }
    }

    /*
    fn parse_component(&mut self, 
        lhs: &TypedExpr,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        //expect_punct!(self, parser_context, Punct::Period);
        self.skip_next_item();
        let lhs_type = &lhs.r#type;

        match lhs_type {
            Type::UnsafeStruct{name} => {
                let tm_entry = parser_context.type_map.get(name).unwrap().clone();
                match tm_entry {
                    TypeDecl::Struct{struct_type: st, under_construction: _, export: _, name: _} => self.parse_struct_component(lhs, &st, parser_context),
                    _ => unreachable!()
                }
            },
            Type::Int | Type::IntLiteral(_)  => self.parse_int_component(lhs, parser_func_context, parser_context),
            //FIXME64BIT
            Type::UnsafeSizeT => self.parse_unsafe_size_t_component(lhs, parser_func_context, parser_context),
            Type::UnsafeOption(_/*inner*/) => self.parse_unsafe_option_component(lhs, /*&inner,*/ parser_func_context, parser_context),
            _ => {
                parser_context.push_err(Error::NoComponents(lhs.loc.clone()));
                lhs.clone()
            }
        }
    }
    */

    ///A square bracket member access.
    fn parse_dynamic_component(&mut self, 
        lhs: &TypedExpr,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        //expect_punct!(self, parser_context, Punct::OpenBracket);
        self.skip_next_item();
        let lhs_type = &lhs.r#type;
        match lhs_type {
            Type::UnsafeArray(inner_type) => {
                self.parse_unsafe_array_dynamic_component(lhs, inner_type, parser_func_context, parser_context)
            },
            _ => {
                parser_context.push_err(Error::NoComponents(lhs.loc.clone()));
                lhs.clone()
            }
        }
    }

    fn parse_paren_expr(&mut self, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let mut loc = self.peek_next_location();
        //assert_punct!(self, Punct::OpenParen);
        self.skip_next_item();

        let mut out_arr: Vec<TypedExpr> = vec![];
        let mut out_arr_types: Vec<Type> = vec![];
        loop {
            //peek the next item
            let lookahead_item = self.peek_next_item();
            if lookahead_item.token.matches_punct(Punct::CloseParen) {
                loc.extend_right(&lookahead_item.location);
                self.skip_next_item();
                break;
            }

            let r_expr = self.parse_expr(parser_func_context, parser_context);
            if r_expr.is_ok() {
                let expr = r_expr.unwrap();
                loc.extend_right(&expr.loc);
                out_arr_types.push(expr.r#type.clone());
                out_arr.push(expr);
            } else {
                parser_context.push_err(r_expr.unwrap_err());
            }

            let lookahead_item = self.peek_next_item();
            if !(lookahead_item.token.matches_punct(Punct::Comma) || lookahead_item.token.matches_punct(Punct::CloseParen)) {
                parser_context.push_err(Error::UnexpectedToken(lookahead_item.location.clone(), String::from("Expecting ',' or ')")));
            }

            if lookahead_item.token.matches_punct(Punct::Comma) {
                loc.extend_right(&lookahead_item.location);
                self.skip_next_item();
            }
        }

        if out_arr.len() == 1 {
            out_arr[0].clone()
        } else {
            TypedExpr{expr: Expr::TupleLiteral(out_arr), is_const: true, loc: loc, r#type: Type::Tuple(out_arr_types)}
        }
    }

    fn parse_object_literal(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut loc = self.peek_next_location();
        assert_punct!(self, Punct::OpenBrace);

        let mut out: Vec<ObjectLiteralElem> = vec![];
        let mut type_out: HashMap<String, Type> = HashMap::new();

        loop {
            let next_item = self.next_item();
            assert_ok!(next_item);
            let token = next_item.token;

            match token {
                Token::Punct(p) => {
                    match p {
                        Punct::CloseBrace => {
                            loc.end = next_item.location.end.clone();
                            break;
                        },
                        _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ';' or '}'"),
                    }
                },
                Token::Ident(i) => {
                    assert_punct!(self, Punct::Colon);
                    let v = self.parse_expr(parser_func_context, parser_context);
                    assert_ok!(v);
                    type_out.insert(i.to_string(), v.r#type.clone());
                    out.push(ObjectLiteralElem{name: i.to_string(), value: v});
                    
                    let lookahead = self.peek_next_token();
                    match lookahead {
                        Token::Punct(p) => {
                            match p {
                                Punct::CloseBrace => {
                                },
                                Punct::Comma => {
                                    self.skip_next_item();
                                },
                                _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ';' or '}'"),
                            }
                        },
                        _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ';' or '}'"),
                    }
                },
                _ => return self.unexpected_token_error(next_item.span, &loc.clone(), "expecting ';' or '}'"),
            }
        }
        
        Ok(TypedExpr{expr: Expr::ObjectLiteral(out), is_const: true, loc: loc, r#type: Type::ObjectLiteral(type_out)})
    }

    fn get_postfix_unary_operator_for_token(&self, 
        span: Span, 
        location: SourceLocation, 
        token: &Token<&'b str>,
        parser_context: &mut ParserContext,
    ) -> UnaryOperator {
        match token {
            Token::Punct(Punct::DoublePlus) => UnaryOperator::PostfixIncrement,
            Token::Punct(Punct::DoubleDash) => UnaryOperator::PostfixDecrement,
            _ => {
                parser_context.push_err(self.unexpected_token_error_raw(span, &location, "unknown binary operator"));
                UnaryOperator::Plus
            }
        }
    }

    fn try_parse_built_in(&mut self, 
        id: &String,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<TypedExpr> {
        match id.as_str() {
            "__memorySize" => Some(self.parse_mem_size(parser_func_context, parser_context)),
            "__memoryGrow" => self.parse_mem_grow(parser_func_context, parser_context).ok(),
            "__trap" => Some(self.parse_trap(parser_func_context, parser_context)),
            "__sizeof" => self.parse_sizeof(parser_func_context, parser_context).ok(),
            _ => None
        }
    }

    fn parse_number(&mut self, 
        n: &Number<&'b str>,
        loc: &SourceLocation,
    ) -> TypedExpr {
        self.skip_next_item();
        match n.kind() {
            NumberKind::Hex => {
                if n.str_len() > 10 {
                    let number = n.parse_i64();
                    TypedExpr{expr: Expr::BigIntLiteral(number.unwrap()), r#type: Type::BigInt, is_const: true, loc: loc.clone()}
                } else {
                    let number = n.parse_i32();
                    TypedExpr{expr: Expr::IntLiteral(number.unwrap()), r#type: Type::Int, is_const: true, loc: loc.clone()}
                }
            },
            NumberKind::DecI => {
                let number_i64 = n.parse_i64().unwrap();
                if number_i64 > std::i32::MAX.into() || number_i64 < std::i32::MIN.into() {
                    TypedExpr{expr: Expr::BigIntLiteral(number_i64), r#type: Type::BigInt, is_const: true, loc: loc.clone()}
                } else {
                    let number_i32 = n.parse_i32().unwrap();
                    TypedExpr{expr: Expr::IntLiteral(number_i32), r#type: Type::Int, is_const: true, loc: loc.clone()}
                }
            },
            NumberKind::Bin => {
                if n.str_len() > 34 {
                    let number = n.parse_i64();
                    TypedExpr{expr: Expr::BigIntLiteral(number.unwrap()), r#type: Type::BigInt, is_const: true, loc: loc.clone()}
                } else {
                    let number = n.parse_i32();
                    TypedExpr{expr: Expr::IntLiteral(number.unwrap()), r#type: Type::Int, is_const: true, loc: loc.clone()}
                }
            },
            NumberKind::Oct => {
                if n.str_len() > 18 {
                    let number = n.parse_i64();
                    TypedExpr{expr: Expr::BigIntLiteral(number.unwrap()), r#type: Type::BigInt, is_const: true, loc: loc.clone()}
                } else {
                    let number = n.parse_i32();
                    TypedExpr{expr: Expr::IntLiteral(number.unwrap()), r#type: Type::Int, is_const: true, loc: loc.clone()}
                }
            },
            NumberKind::DecF => {
                let number = n.parse_f64();
                TypedExpr{expr: Expr::FloatLiteral(number.unwrap()), r#type: Type::Number, is_const: true, loc: loc.clone()}
            },
        }
    }

    fn parse_ident_expr(&mut self,
        o_holding: &Option<TypedExpr>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let err_ret = TypedExpr{expr: Expr::NoOp, r#type: Type::Undeclared, loc: SourceLocation::new(Position::new(0, 0), Position::new(0, 0)), is_const: true};
        let next = expect_next!(self, parser_context, err_ret);

        let id = expect_ident!(next, parser_context, "Expecting identifier declaration");
        let mut expr = match o_holding{
            None => {
                let o_sv = parser_context.get_scoped_var(&id.to_string());
                match o_sv {
                    Some(sv) => match sv {
                        ScopedVar::ClosureRef{internal_name, r#type, constant} => {
                            let is_new = !parser_func_context.closure.iter().any(|x| x.internal_name.eq(internal_name));
                            if is_new {
                                parser_func_context.closure.push(ClosureRef{internal_name: internal_name.clone(), r#type: r#type.clone(), constant: *constant})
                            }
                            
                            TypedExpr{expr: Expr::ClosureVariableUse(internal_name.clone()), r#type: r#type.clone(), is_const: *constant, loc: next.location.clone()}
                        },
                        ScopedVar::Local{internal_name, r#type, constant} => TypedExpr{expr: Expr::LocalVariableUse(internal_name.clone()), r#type: r#type.clone(), is_const: *constant, loc: next.location.clone()},
                    },
                    None => {
                        let o_g = parser_context.global_decls.iter().find(|&x| x.name == id.to_string());
                        match o_g {
                            Some(g_var) => 
                                TypedExpr{expr: Expr::GlobalVariableUse(id.to_string()), r#type: g_var.r#type.clone(), is_const: g_var.constant, loc: next.location.clone()},
                            None => {
                                //might be a type literal
                                let o_type = self.parse_type_from_ident(&id, Commitment::Speculative, parser_context);
                                match o_type {
                                    Some(t) => 
                                        TypedExpr{expr: Expr::TypeLiteral(t.clone()), r#type: Type::TypeLiteral(Box::new(t.clone())), is_const: true, loc: next.location.clone()},
                                    _ => {
                                        // peek to see if this is a function call. 
                                        let lookahead_item = self.peek_next_item();
                                        let lookahead = lookahead_item.token;
            
                                        match lookahead {
                                            Token::Punct(Punct::OpenParen) => {
                                                let o_f_d = parser_context.get_fn_decl_from_decls(&id);
                                                match &o_f_d {
                                                    Some(f_d) => {
                                                        //yep it's a function call
                                                        self.parse_static_func_call(&id, &f_d, &next.location, parser_func_context, parser_context)
                                                    },
                                                    None => {
                                                        //might be a built in, so check that
                                                        let o_built_in = self.try_parse_built_in(&id, parser_func_context, parser_context);
                                                        match o_built_in {
                                                            Some(built_in) => built_in,
                                                            None => {
                                                                //generic check 
                                                                let o_g_f_c = self.try_parse_generic_func_call(&id, parser_func_context, parser_context);
                                                                match o_g_f_c {
                                                                    Some(g_f_c) => g_f_c,
                                                                    None => {
                                                                        parser_context.push_err(Error::VariableNotRecognized(next.location.clone(), id.clone()));
                                                                        err_ret
                                                                    }
                                                                }
                                                            },
                                                        }
                                                    }
                                                }
                                            },
                                            _ => {
                                                //module check
                                                if parser_context.import_namespace_map.contains_key(&id) {
                                                    TypedExpr{expr: Expr::NoOp, r#type: Type::ModuleLiteral(id.clone()), is_const: true, loc: next.location.clone()}
                                                } else {
                                                    parser_context.push_err(Error::VariableNotRecognized(next.location.clone(), id.clone()));
                                                    err_ret
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            Some(holding) => {
                match &holding.r#type {
                    Type::ModuleLiteral(m) => {
                        let full_name = format!("{}.{}", m, id);
                        let o_g = parser_context.global_imports.iter().find(|&x| x.name == full_name);
                        match o_g {
                            Some(g_var) => 
                                TypedExpr{expr: Expr::GlobalVariableUse(full_name), r#type: g_var.r#type.clone(), is_const: g_var.constant, 
                                    loc: next.location.clone()},
                            None => {
                                let o_f_d = parser_context.get_fn_decl_from_imports(&full_name);
                                match o_f_d {
                                    Some(f_d) => 
                                        //yep it's a function call
                                        self.parse_static_func_call(&full_name, &f_d, &next.location, parser_func_context, parser_context),
                                    None => {
                                        //generic check 
                                        let o_g_f_c = self.try_parse_generic_func_call(&full_name, parser_func_context, parser_context);
                                        match o_g_f_c {
                                            Some(g_f_c) => g_f_c,
                                            None => {
                                                parser_context.push_err(Error::VariableNotRecognized(next.location.clone(), full_name.clone()));
                                                //give up
                                                return err_ret;
                                            },
                                        }
                                    },
                                }
                            },
                        }
                    },
                    Type::Int | Type::IntLiteral(_) => self.parse_int_component(&holding, &id, &next.location, parser_func_context, parser_context),
                    Type::UnsafeStruct{name} => {
                        let tm_entry = parser_context.type_map.get(name).unwrap().clone();
                        match tm_entry {
                            TypeDecl::Struct{struct_type: st, under_construction: _, export: _, name: _} => self.parse_struct_component(&holding, &id, &st, &next.location, parser_context),
                            _ => unreachable!()
                        }
                    },
                    Type::UnsafeSizeT => self.parse_unsafe_size_t_component(&holding, &id, &next.location, parser_func_context, parser_context),
                    Type::UnsafeOption(_) => self.parse_unsafe_option_component(&holding, &id, &next.location, parser_func_context, parser_context),
                    _ => {
                        parser_context.push_err(Error::NoComponents(next.location.clone()));
                        //give up
                        return err_ret;
                    }
                }
            }
        };

        //now we either have a function call or a variable, let's see if it breaks down further
        loop {
            let lookahead_item = self.peek_next_item();
            let lookahead = lookahead_item.token;
         
            //first peek for postfix operators
            let o_uod = get_unary_operator_data(&lookahead);
            match o_uod {
                Some(uod) => {
                    if uod.fix == Fix::Postfix || uod.fix == Fix::Both {
                        let op = self.get_postfix_unary_operator_for_token(lookahead_item.span, lookahead_item.location, &lookahead, parser_context);
                        self.skip_next_item();
                        let o_outer_type = types::get_unary_op_type(get_op_type_for_unop(&op), &expr.r#type);
                        let loc = SourceLocation::new(expr.loc.start.clone(), lookahead_item.location.end.clone());
                        let outer_type = match o_outer_type {
                            Some(outer_type) => outer_type,
                            None => {
                                parser_context.push_err(Error::TypeFailureUnaryOperator(loc));
                                expr.r#type.clone()
                            }
                        };
                        expr = TypedExpr{expr: Expr::UnaryOperator{op: op, expr: Box::new(expr.clone())}, r#type: outer_type, is_const: true, loc: loc}; 
                        continue;
                    }
                },
                _ => {},
            };

            //now, other things it could be
            match lookahead {
                Token::Punct(Punct::OpenParen) => {
                    match &expr.r#type {
                        Type::Func{func_type} => {
                            let args = self.parse_function_call_args(&func_type.in_types, parser_func_context, parser_context);
                            let loc = SourceLocation::new(lookahead_item.location.start.clone(), args.last().map(|a| a.loc.end.clone()).unwrap_or(next.location.end.clone()));
                            expr = TypedExpr{expr: Expr::DynamicFuncCall(Box::new(expr.clone()), args), r#type: func_type.out_type.clone(), is_const: true, loc: loc};
                            continue;
                        },
                        _ => {
                            let args = self.parse_function_call_args(&vec![], parser_func_context, parser_context);
                            let loc = SourceLocation::new(lookahead_item.location.start.clone(), next.location.end.clone());
                            parser_context.push_err(Error::TypeFailureFuncCall(loc.clone()));
                            expr = TypedExpr{expr: Expr::DynamicFuncCall(Box::new(expr.clone()), args), r#type: expr.r#type.clone(), is_const: true, loc: loc};
                            continue;
                        }
                    }
                },

                Token::Punct(Punct::OpenBracket) => {
                    expr = self.parse_dynamic_component(&expr, parser_func_context, parser_context);
                    continue;
                },
                _ => { break;}
            }
        }

        expr
    }

    /// From wikipedia
    fn parse_expr(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let expr = self.parse_primary(&None, parser_func_context, parser_context);
        assert_ok!(expr);
        self.parse_expr_1(&expr, 0, parser_func_context, parser_context)
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
        holding: &Option<TypedExpr>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        let o_uod = get_unary_operator_data(&lookahead);
        match o_uod {
            Some(uod) => {
                if uod.fix == Fix::Prefix || uod.fix == Fix::Both {
                    let mut loc = lookahead_item.location.clone();
                    let op = self.get_prefix_unary_operator_for_token(lookahead_item.span, &lookahead_item.location, &lookahead);
                    assert_ok!(op);
                    self.skip_next_item();
                    let inner = self.parse_expr(parser_func_context, parser_context);
                    assert_ok!(inner);
                    loc.end = inner.loc.end.clone();
                    let o_outer_type = types::get_unary_op_type(get_op_type_for_unop(&op), &inner.r#type);
                    match o_outer_type {
                        Some(outer_type) => 
                            return Ok(TypedExpr{expr: Expr::UnaryOperator{op: op, expr: Box::new(inner)}, r#type: outer_type, is_const: true, loc: loc}),
                        None => return Err(Error::TypeFailureUnaryOperator(loc)),
                    }
                }
            },
            _ => {},
        };

        match lookahead {
            Token::Number(n) => { return Ok(self.parse_number(&n, &lookahead_item.location)); },
            
            Token::Boolean(b) => {
                self.skip_next_item();
                return Ok(TypedExpr{expr:Expr::BoolLiteral(b.is_true()), r#type: Type::Boolean, is_const: true, loc: lookahead_item.location.clone()});
            },

            Token::Null => {
                self.skip_next_item();
                return Ok(Parser::create_null(&lookahead_item.location));
            },

            Token::UnsafeNull => {
                self.skip_next_item();
                return Ok(Parser::create_unsafe_null(&lookahead_item.location));
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
                    Keyword::New => return self.parse_new(parser_func_context, parser_context),
                    Keyword::UnsafeStatic => return self.parse_unsafe_static(parser_func_context, parser_context),
                    Keyword::Void => { 
                        self.skip_next_item();
                        return Ok(TypedExpr{expr:Expr::Void, r#type: Type::FakeVoid, is_const: true, loc: lookahead_item.location.clone()}); 
                    },
                    Keyword::If => Ok(self.parse_if(parser_func_context, parser_context)),
                    Keyword::Fn => {
                        let o_func = self.main_parse_function_decl(false, parser_func_context, parser_context);
                        match o_func{
                            Some(f) => Ok(f),
                            None => Err(Error::Dummy(lookahead_item.location))
                        }
                    },
                    Keyword::Some => self.parse_some(parser_func_context, parser_context),
                    Keyword::UnsafeSome => self.parse_unsafe_some(parser_func_context, parser_context),
                    _ => {
                        self.skip_next_item();
                        let t = self.parse_type_from_keyword(&k, &lookahead_item.location, parser_context);
                        Ok(TypedExpr{expr: Expr::TypeLiteral(t.clone()), r#type: Type::TypeLiteral(Box::new(t.clone())), is_const: true, loc: lookahead_item.location.clone()})
                    },
                }
            },

            Token::Ident(_) => Ok(self.parse_ident_expr(holding, parser_func_context, parser_context)),

            Token::Punct(p) => match p {
                Punct::OpenParen => Ok(self.parse_paren_expr(parser_func_context, parser_context)),
                Punct::OpenBrace => self.parse_object_literal(parser_func_context, parser_context),
                _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "unexpected punctuation"),
            },

            _ => self.unexpected_token_error(lookahead_item.span, &lookahead_item.location, "unexpected token"),
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
        init_lhs: &TypedExpr, 
        min_precedence: i32, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Res<TypedExpr> {
        let mut lhs = init_lhs.clone();
        let lookahead_item = self.peek_next_item();
        let mut lookahead = lookahead_item.token;
        
        // while lookahead is a binary operator whose precedence is >= min_precedence
        loop {
            let o_lookahead_data = get_binary_operator_data(&lookahead);
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
                        // OK, I will not lie this is a teeeeny bit bad. For dot operators we pass the
                        // lhs in so that the rhs can validate itself against the thing it is a member of.
                        // because we are precedence climbing, actually this could be wrong because the lhs
                        // and rhs might be some way away. BUT because dot is the highest of all the precedences...
                        // it won't be. 
                        // Yes I am ashamed, I will weep tiny tears like dancing giraffes
                        let holding = if lookahead_data.is_member {
                            Some(lhs.clone())
                        } else {
                            None
                        };
                        let r_rhs = self.parse_primary(&holding, parser_func_context, parser_context);
                        if r_rhs.is_err() { return Err(r_rhs.unwrap_err()) }; 
                        let mut rhs = r_rhs?;
                        // lookahead := peek next token
                        let lookahead_item = self.peek_next_item();
                        lookahead = lookahead_item.token;
                        // while lookahead is a binary operator whose precedence is greater
                        // than op's, or a right-associative operator
                        // whose precedence is equal to op's
                        loop {
                            let o_lookahead_data = get_binary_operator_data(&lookahead);
                            match o_lookahead_data {
                                None => break,
                                Some(lookahead_data) => {
                                    if lookahead_data.precedence > op_precedence || (lookahead_data.association == Association::Right && lookahead_data.precedence == op_precedence) {
                                        //rhs := parse_expression_1 (rhs, lookahead's precedence)
                                        let new_rhs = self.parse_expr_1(&rhs, lookahead_data.precedence, parser_func_context, parser_context);
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
        return Ok(lhs.clone());
    }
}

#[cfg(test)]
mod test {
    use super::*;
    
    struct DummyImporter{

    }

    impl Importer for DummyImporter{
        fn import(&mut self, _: &String, _: &String) -> Result<Imports, String> {
            Ok(Imports{exports: Exports::new(), module_name: String::from(""), unique_name: String::from("")})
        }
    }

    #[test]
    fn add_test() {
        let add = "export function addd(x: number, y: number, z: number): number {
            let a = 1;
            return x + y + z + a;
        }";

        let mut parser = Parser::new(add).unwrap();
        let script = parser.parse_full(false, & mut DummyImporter{}, &String::from(""), StartFuncType::Start, &String::from("")).unwrap();
        println!("{:#?}", script);
    }
}