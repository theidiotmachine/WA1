extern crate wa1_lexer;
extern crate log;

use crate::generics::new_type_map;
use crate::generics::transform_type;
use wa1_lexer::prelude::*;
use wa1_ast::prelude::*;
use wa1_types::prelude::*;

pub use wa1_errs::Error;
use wa1_errs::prelude::*;

use std::{mem::replace};

use std::collections::{HashMap, HashSet};

mod parser_unsafe;
mod parser_int;
mod parser_func;
mod parser_type;
mod parser_option;
mod parser_phase_1;
mod parser_op;
mod parser_typeguard;

use crate::{ParserContext, ScopedVar, UnsafeParseMode};
use crate::ParserFuncContext;
use crate::Res;
use crate::{expect_punct, expect_next, expect_ident, expect_string_literal, expect_keyword, StartFuncType, expect_semicolon, ParserPhase};
use crate::{try_create_cast, cast_typed_expr, guard_downcast_expr, create_cast};
use crate::parser::parser_op::{get_unary_operator_data, Fix, get_binary_operator_data, Association, get_op_type_for_unop};
use crate::parser::parser_typeguard::{get_type_guard_inst, apply_type_guard_inst, unapply_type_guard_inst};
use crate::Commitment;
use crate::Importer;

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
    /// The internal scanner (see the
    /// `ress` crate for more details)
    scanner: Scanner<'a>,
    /// The next item,
    look_ahead: Item<Token<&'a str>>,
    /// Since we are looking ahead, we need
    /// to make sure we don't miss the eof
    /// by using this flag
    found_eof: bool,

    /// If the scanner has a pending line terminator
    /// before the next token
    pub has_line_term: bool,

    look_ahead_position: Position,
}

impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Res<Self> {
        let s = Scanner::new(text);
        Self::_new(s)
    }
}

impl<'b> Parser<'b> {
    fn _new(
        scanner: Scanner<'b>,
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
            has_line_term: false,
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
        
        let mut fake_parser_func_context = ParserFuncContext::new(&None);

        loop {
            if self.look_ahead.token.is_eof() {
                break;
            } else {
                self.parse_global_statement(&mut init_body, &mut fake_parser_func_context, parser_context, importer);
            }
        }

        let start_function = Func{ 
            decl: FuncDecl{
                name: start_func_name.clone(), return_type: FullType::new_const(&Type::RealVoid), args: vec![], 
                export: start_func_type == StartFuncType::WASMCallCtors, generic_impl: false, type_guard: None, member_func: false,
            },
            body: Some(TypedExpr{
                expr: Expr::Block(init_body),
                r#type: FullType::new_const(&Type::RealVoid),
                loc: SourceLocation::new(Position::new(0, 0), Position::new(0, 0)), 
                return_expr: ReturnExpr::Func
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
        unsafe_parse_mode: UnsafeParseMode,
        importer: &mut dyn Importer,
        module_name: &String,
        start_func_type: StartFuncType,
        file_name: &String,
    ) -> (AST, Vec<Error>) {
        let mut parser_context = ParserContext::new(unsafe_parse_mode, file_name);
        
        let start_func_name = match start_func_type{
            StartFuncType::WASMCallCtors => String::from("__wasm_call_ctors"),
            StartFuncType::Start => format!("_start_{}", module_name)
        };
        self.parse_internal(&start_func_name, start_func_type, &mut parser_context, importer);
        
        (AST{start: start_func_name, global_decls: parser_context.global_decls, global_imports: parser_context.global_imports,
            func_decls: parser_context.func_decls, func_imports: parser_context.func_imports, generic_func_decls: parser_context.generic_func_decls,
            type_map: parser_context.type_map, trait_map: parser_context.trait_map, trait_impl_map: parser_context.trait_impl_map
        }, parser_context.errors)
    }

    pub fn next_position(&self) -> SourceLocation {
        self.look_ahead.location
    }

    /// Skip the next token, ignoring it.
    fn skip_next_item(&mut self) -> () {
        loop {
            self.has_line_term = self.scanner.pending_new_line;
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
                    } else {
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
            self.has_line_term = self.scanner.pending_new_line;
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
                    return Err(Error::UnexpectedEoF(self.look_ahead.location.clone(), "".to_string()));
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

    fn unexpected_token_error_raw(&self, span: Span, location: &SourceLocation, msg: &str) -> Error {
        let name = self.scanner.string_for(&span).unwrap_or_default();
        Error::UnexpectedToken(
            location.clone(),
            format!("Found unexpected token: {}; {}", name, msg),
        )
    }

    ///Parse a set of type variables in angle brackets
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
        this_type: &Option<FullType>,
        arg_list: &Vec<FuncArg>, 
        parser_func_context: &mut ParserFuncContext, 
        parser_context: &mut ParserContext,
    ) {
        if this_type.is_some() {
            let this_type = this_type.clone().unwrap();
            parser_context.add_var(&String::from("this"), &String::from("this"), &this_type, VariableMutability::Constant);
            parser_func_context.add_var(&String::from("this"), &this_type, false, true, VariableMutability::Constant);
        }

        for arg in arg_list {
            //vile hack
            if arg.name == "this" {
                continue;
            }
            let internal_name = parser_context.get_unique_name(&arg.name);
            parser_context.add_var(&arg.name, &internal_name, &arg.r#type, arg.mutability);
            //todo: constant params
            //todo: default params
            parser_func_context.add_var(&internal_name, &arg.r#type, false, true, arg.mutability);
        }
    }

    fn parse_import_decl(&mut self,
        parser_context: &mut ParserContext,
        importer: &mut dyn Importer,
    ) -> () {
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

        let next = self.peek_next_item();
        let id_string = expect_string_literal!(next, parser_context, "Expecting path name to import");
        self.skip_next_item();
        if id_string.starts_with(".") {
            let r_imports = importer.import(&id_string, &(parser_context.file_name));
            let imports = match r_imports {
                Ok(imports) => imports,
                Err(e) => {
                    parser_context.push_err(Error::ImportFailed(next.location.clone(), e));
                    return;
                }
            };
            let exports = imports.exports;
            let exports = filter_imports(exports, &import_filter);
            let namespace = imports.module_name;
            for g in &exports.global_decls {
                let import_name = format!("{}.{}", namespace, g.name);
                let decl = GlobalVariableImport{name: import_name.clone(), r#type: g.r#type.clone(), export: false};
                parser_context.global_imports.push(decl.clone());
            }

            for g in &exports.global_imports {
                let decl = GlobalVariableImport{name: g.name.clone(), r#type: g.r#type.clone(), export: false};
                parser_context.global_imports.push(decl.clone());
            }

            for f in &exports.func_decls {
                let import_name = format!("{}.{}", namespace, f.name);
                parser_context.func_imports.push(
                    FuncDecl{name: import_name.clone(), return_type: f.return_type.clone(), args: f.args.clone(), export: false, 
                        generic_impl: false, type_guard: f.type_guard.clone(), member_func: f.member_func},
                ); 
            }

            for f in &exports.generic_func_decls {
                let import_name = format!("{}.{}", namespace, f.func.decl.name);
                parser_context.generic_func_decls.push(
                    GenericFunc{type_args: f.type_args.clone(), num_this_type_args: f.num_this_type_args, func: Func{
                        decl: FuncDecl{name: import_name.clone(), return_type: f.func.decl.return_type.clone(), 
                            args: f.func.decl.args.clone(), export: false, generic_impl: false, type_guard: f.func.decl.type_guard.clone(),
                            member_func: f.func.decl.member_func},
                        body: f.func.body.clone(),
                        local_vars: f.func.local_vars.clone(),
                        closure: f.func.closure.clone(),
                        local_var_map: f.func.local_var_map.clone(),
                    }}
                ); 
            }

            for f in &exports.func_imports {
                parser_context.func_imports.push(
                    FuncDecl{name: f.name.clone(), return_type: f.return_type.clone(), args: f.args.clone(), export: false, 
                        generic_impl: false, type_guard: f.type_guard.clone(), member_func: f.member_func},
                ); 
            }

            parser_context.import_namespace_map.insert(namespace.clone(), imports.unique_name.clone());
        } else {
            //it's an import from an external file
            parser_context.push_err(Error::NotYetImplemented(next.location.clone(), String::from("Import external projects")));
        }
    }


    /// This is for parsing an export declaration. We require these to be in module root, 
    /// and therefore we also pick up the baggage about 'fake' function contexts and init_body
    /// It's not brilliant, this code. It has a lot of baggage that could be removed
    fn parse_export_decl_main_phase(&mut self, 
        init_body: &mut Vec<TypedExpr>,
        fake_parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) {
        self.skip_next_item();
        let next = self.peek_next_item();
        let tok = next.token;
        match &tok {
            Token::Keyword(ref k) => match k {
                Keyword::Fn => {
                    let o_func = self.parse_function_decl_main_phase(true, fake_parser_func_context, parser_context);
                    match o_func {
                        Some(f) => init_body.push(f),
                        _ => {}
                    };
                },
                /*
                Keyword::Const => {
                    let const_decl = self.parse_variable_decl(true, true, true, fake_parser_func_context, parser_context);
                    init_body.push(const_decl);
                },
                */
                Keyword::Let => {
                    let var_decl = self.parse_variable_decl(true, true, fake_parser_func_context, parser_context);
                    init_body.push(var_decl);
                },
                Keyword::UnsafeStruct => self.parse_struct_decl(true, parser_context),
                Keyword::Alias => self.parse_alias(true, parser_context),
                Keyword::Trait => self.parse_trait_decl(true, parser_context),
                Keyword::Type => self.parse_trait_decl(true, parser_context),
                Keyword::Implement => self.parse_trait_impl(true, ParserPhase::MainPhase, parser_context),
                _ => parser_context.errors.push(Error::UnexpectedToken(next.location.clone(), String::from("Can only export functions, variables, or types")))
            },
            _ => parser_context.errors.push(Error::UnexpectedToken(next.location.clone(), String::from("Can only export functions, variables, or types")))
        }
    }

    fn parse_variable_decl(&mut self, 
        global: bool,
        export: bool,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let mut mutability = VariableMutability::Constant;
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let mut next = self.peek_next_item();
        if next.token.matches_keyword(Keyword::Var) {
            mutability = VariableMutability::Variable;
            self.skip_next_item();
            next = self.peek_next_item();
        }

        let id = expect_ident!(next, parser_context, "Expecting variable name to be an identifier");
        self.skip_next_item();
        
        //check to see if this shadows a pre-existing global
        if global && parser_context.get_global_decl(&id.to_string()).is_some() {
            parser_context.push_err(Error::DuplicateGlobalVariable(loc.clone(), id.to_string().clone()));
        }

        let next = self.peek_next_item();
        let token = &next.token;
        let mut var_type = match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                self.parse_full_type(parser_context)
            },
            Token::Punct(Punct::Equal) => {
                FullType::new(&Type::Undeclared, Mutability::Unknown)
            },
            _ => {
                parser_context.push_err(self.unexpected_token_error_raw(next.span, &next.location, "variable must have a type or a value"));
                FullType::new(&Type::Undeclared, Mutability::Unknown)
            }
        };

        expect_punct!(self, parser_context, Punct::Equal);
        let mut init = self.parse_expr(parser_func_context, parser_context);
        
        //if init is temporary, strip that out
        init = match init.expr {
            Expr::TemporaryCreation(expr, temporary_name) => {
                parser_context.forget_temporary(&temporary_name);
                *expr
            },
            _ => init
        };
        
        loc.end = init.loc.end.clone();

        if var_type.r#type.is_undeclared() {
            if mutability == VariableMutability::Constant {
                var_type = init.r#type.clone();
            } else {
                var_type = FullType::new(&upcast_from_literal(&init.r#type.r#type), init.r#type.mutability);
            }
        } else if var_type != init.r#type {
            let o_cast_expr = try_create_cast(&var_type, &init, CastType::Implicit);
            match o_cast_expr {
                Some(cast_expr) => { init = cast_expr; },
                None => { 
                    parser_context.push_err(Error::TypeFailureVariableCreation(init.loc, format!("{}", var_type), format!("{}", init.r#type)));
                }
            }
        }

        expect_semicolon!(self, parser_context);
        let name = id.to_string();
        if !global {
            let internal_name = parser_context.get_unique_name(&name);
            parser_context.add_var(&name, &internal_name, &var_type, mutability);
            parser_func_context.add_var(&internal_name, &var_type, false, false, mutability);

            if var_type.r#type.is_type_variable() {
                TypedExpr{expr: Expr::UnresolvedVariableInit{internal_name: internal_name.clone(), var_type: var_type.clone(), init: Box::new(init)}, r#type: FullType::new_const(&Type::RealVoid), loc: loc, return_expr: ReturnExpr::None}
            } else {
                let is_standard_value_model = var_type.r#type.is_standard_value_model();
                if (is_standard_value_model && var_type.mutability == Mutability::Const && init.r#type.mutability == Mutability::Const) || !is_standard_value_model{
                    TypedExpr{expr: Expr::SimpleVariableInit{internal_name: internal_name.clone(), init: Box::new(init)}, r#type: FullType::new_const(&Type::RealVoid), loc: loc, return_expr: ReturnExpr::None}
                } else {
                    //this needs to be a deep copy
                    unimplemented!()
                }
            } 
        } else {
            let is_standard_value_model = var_type.r#type.is_standard_value_model();
            let var_type_mutability = var_type.mutability;
            let init_type_mutability = init.r#type.mutability;
            let decl = GlobalVariableDecl{name: name.clone(), r#type: var_type, init: Some(init), export, mutability: mutability};
            parser_context.global_decls.push(decl.clone());
            if (is_standard_value_model && var_type_mutability == Mutability::Const && init_type_mutability == Mutability::Const) || !is_standard_value_model{ 
                TypedExpr{expr: Expr::SimpleGlobalVariableDecl(Box::new(decl)), r#type: FullType::new_const(&Type::RealVoid), loc: loc, return_expr: ReturnExpr::None}
            } else {
                unimplemented!()
            }
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
                Keyword::Export => self.parse_export_decl_main_phase(init_body, fake_parser_func_context, parser_context),
                Keyword::Fn => {
                    let o_func = self.parse_function_decl_main_phase(false, fake_parser_func_context, parser_context);
                    match o_func {
                        Some(f) => init_body.push(f),
                        _ => {}
                    };
                },
                Keyword::Let => {
                    let var_decl = self.parse_variable_decl(true, false, fake_parser_func_context, parser_context);
                    init_body.push(var_decl);
                },
                Keyword::UnsafeStruct => self.parse_struct_decl(false, parser_context),
                Keyword::Alias => self.parse_alias(false, parser_context),
                Keyword::Type => self.parse_type_decl(false, ParserPhase::MainPhase, parser_context),
                Keyword::Trait => self.parse_trait_decl(false, parser_context),
                Keyword::Implement => self.parse_trait_impl(false, ParserPhase::MainPhase, parser_context),
                _ => { parser_context.errors.push(self.unexpected_token_error_raw(next.span, &next.location, "expecting valid statement")); self.skip_next_item(); }
            },
            _ => {
                // if we don't know what this statement is, parse it as an expr
                let expr = self.parse_expr(fake_parser_func_context, parser_context);
                init_body.push(expr);

                // ugh this is like a hundred million lines to check the semi-colon
                let next = self.next_item();
                match next {
                    Ok(next) => {
                        if self.has_line_term {
                        } else if next.token.matches_punct(Punct::SemiColon) || next.token.is_eof() {
                            self.skip_next_item();
                        } else {
                            parser_context.errors.push(self.expected_token_error_raw(&next, &[&";"]));
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
                if token.is_eof() {
                    parser_context.push_err(Error::UnexpectedEoF(next.location.clone(), String::from("expecting '}'")));
                    break;
                }
                
                let stmt = self.parse_statement(parser_func_context, parser_context);
                out.push(stmt);
                
                next = self.peek_next_item();
                loc.extend_right(&next.location);
                token = &next.token;
            }
            self.skip_next_item();
            if push_scope {
                parser_context.pop_block_scope();
            }
        } else {
            let stmt = self.parse_statement(parser_func_context, parser_context);
            loc.extend_right(&stmt.loc);
            out.push(stmt);
        } 

        if out.last().is_some() {
            out.last_mut().unwrap().set_return_expr(if push_scope { ReturnExpr::Block } else { ReturnExpr::Func } );
        }

        let out_type = out.last().map_or(FullType::new_const(&Type::RealVoid), |x| x.r#type.clone());
        TypedExpr{expr: Expr::Block(out), r#type: out_type, loc: loc, return_expr: ReturnExpr::None}
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
        if condition.r#type.r#type != Type::Bool {
            parser_context.push_err(Error::TypeFailure(condition.loc.clone(), FullType::new_const(&Type::Bool), condition.r#type.clone()));
        }
        expect_punct!(self, parser_context, Punct::CloseParen);

        //is the condition a type guard?
        let o_type_guard_inst = get_type_guard_inst(&condition.expr);
        
        //create the then block
        let pop_then_stack = apply_type_guard_inst(&o_type_guard_inst, &Expr::BoolLiteral(true), &condition.loc, parser_context);
        let then_block = self.parse_block(true, parser_func_context, parser_context);
        if pop_then_stack {
            unapply_type_guard_inst(&o_type_guard_inst, &Expr::BoolLiteral(true), parser_context)
        }

        let next = self.peek_next_item();
        let token = &next.token;
        if token.matches_keyword(Keyword::Else) {
            self.skip_next_item();

            //create the else block
            let pop_else_stack = apply_type_guard_inst(&o_type_guard_inst, &Expr::BoolLiteral(false), &condition.loc, parser_context);
            let else_block = self.parse_block(true, parser_func_context, parser_context);
            if pop_else_stack {
                unapply_type_guard_inst(&o_type_guard_inst, &Expr::BoolLiteral(false), parser_context)
            }

            let then_block_type = then_block.r#type.clone();
            loc.end = else_block.loc.end.clone();
            if then_block.r#type != else_block.r#type {
                //if either is void, discard the other
                if then_block_type.r#type == Type::RealVoid || else_block.r#type.r#type == Type::RealVoid {
                    TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(else_block)), r#type: FullType::new_const(&Type::RealVoid), loc: loc, return_expr: ReturnExpr::None}
                } else if then_block_type.r#type == Type::Never {
                    let else_block_type = else_block.r#type.clone();
                    TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(else_block)), r#type: else_block_type, loc: loc, return_expr: ReturnExpr::None}
                } else if else_block.r#type.r#type == Type::Never {
                    TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(else_block)), r#type: then_block_type, loc: loc, return_expr: ReturnExpr::None}
                } else {
                    //first try casting then to else
                    let then_to_else_cast = try_create_cast(&else_block.r#type, &then_block, CastType::Implicit);
                    match then_to_else_cast {
                        None => {
                            //nope, not that way. Try the other
                            let else_to_then_cast = try_create_cast(&then_block.r#type, &else_block, CastType::Implicit);
                            match else_to_then_cast {
                                None => {
                                    //if we can't figure out the type, make it the top type
                                    TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(else_block)), r#type: FullType::new_const(&Type::Unknown), loc: loc, return_expr: ReturnExpr::None}
                                },
                                Some(new_else_block) => {
                                    TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(new_else_block)), r#type: then_block_type, loc: loc, return_expr: ReturnExpr::None}
                                }
                            }
                        }, 
                        Some(new_then_block) => {
                            let new_then_block_type = new_then_block.r#type.clone();
                            TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(new_then_block), Box::new(else_block)), r#type: new_then_block_type, loc: loc, return_expr: ReturnExpr::None}
                        }  
                    }
                }
            } else {            
                TypedExpr{expr: Expr::IfThenElse(Box::new(condition), Box::new(then_block), Box::new(else_block)), r#type: then_block_type, loc: loc, return_expr: ReturnExpr::None}
            }
        } else {
            loc.end = then_block.loc.end.clone();
            TypedExpr{expr: Expr::IfThen(Box::new(condition), Box::new(then_block)), r#type: FullType::new_const(&Type::RealVoid), loc: loc, return_expr: ReturnExpr::None}
        }
    }

    fn parse_while(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        expect_punct!(self, parser_context, Punct::OpenParen);
        let condition = self.parse_expr(parser_func_context, parser_context);
        if condition.r#type.r#type != Type::Bool {
            parser_context.push_err(Error::TypeFailure(condition.loc.clone(), FullType::new_const(&Type::Bool), condition.r#type.clone()));
        }
        expect_punct!(self, parser_context, Punct::CloseParen);

        //is the condition a type guard?
        let o_type_guard_inst = get_type_guard_inst(&condition.expr);

        let old_in_iteration = parser_func_context.in_iteration;
        parser_func_context.in_iteration = true;
        let pop_stack = apply_type_guard_inst(&o_type_guard_inst, &Expr::BoolLiteral(true), &condition.loc, parser_context);
        let block = self.parse_block(true, parser_func_context, parser_context);
        if pop_stack {
            unapply_type_guard_inst(&o_type_guard_inst, &Expr::BoolLiteral(true), parser_context);
        }
        parser_func_context.in_iteration = old_in_iteration;
        loc.end = block.loc.end.clone();

        TypedExpr{expr: Expr::While(Box::new(condition), Box::new(block)), r#type: FullType::new_const(&Type::RealVoid), loc: loc, return_expr: ReturnExpr::None}
    }

    /// A statement is something you can't assign to (like break or a variable declaration) or a true expr.
    fn parse_statement(&mut self, 
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let next = self.peek_next_item();
        let token = &next.token;
        parser_context.open_temporary_scope();
        let out = match &token {
            Token::Keyword(ref k) => match k {
                Keyword::Let => {
                    self.parse_variable_decl(false, false, parser_func_context, parser_context)
                },
                Keyword::Return => {
                    let loc = next.location.clone();
                    self.skip_next_item();
                    let next = self.peek_next_item();
                    
                    let token = &next.token;

                    if token.matches_punct(Punct::SemiColon) || self.has_line_term || token.is_eof() {
                        // check the return type of this expression versus the function return type.
                        let check_return_type = if parser_func_context.given_func_return_type.r#type == Type::Undeclared {
                            if parser_func_context.implied_func_return_type.r#type == Type::Undeclared {
                                parser_func_context.implied_func_return_type = FullType::new_const(&Type::RealVoid);
                            }
                            parser_func_context.implied_func_return_type.clone()
                        } else {
                            parser_func_context.given_func_return_type.clone()
                        };

                        if check_return_type.r#type != Type::RealVoid {
                            parser_context.errors.push(Error::TypeFailureReturn(loc.clone(), check_return_type.r#type.clone(), Type::RealVoid));
                        }
                        if !self.has_line_term {
                            self.skip_next_item();
                        }
                        
                        TypedExpr{expr: Expr::Return(Box::new(None)), r#type: FullType::new_const(&Type::Never), loc: loc, return_expr: ReturnExpr::Func}
                    } else {
                        let mut expr = self.parse_expr(parser_func_context, parser_context);
                        expect_semicolon!(self, parser_context);
                        // return type checking
                        let expr_type = expr.r#type.clone();
                        let check_return_type = if parser_func_context.given_func_return_type.r#type == Type::Undeclared {
                            if parser_func_context.implied_func_return_type.r#type == Type::Undeclared {
                                parser_func_context.implied_func_return_type = expr_type.clone();
                            }
                            parser_func_context.implied_func_return_type.clone()
                        } else {
                            parser_func_context.given_func_return_type.clone()
                        };

                        expr.set_return_expr(ReturnExpr::Func);
                        TypedExpr{expr: Expr::Return(Box::new(Some(
                            cast_typed_expr(&check_return_type, Box::new(expr), CastType::Implicit, parser_context)
                        ))), r#type: FullType::new_const(&Type::Never), loc: loc, return_expr: ReturnExpr::Func}
                    }
                },

                Keyword::Break => {
                    self.skip_next_item();
                    if !parser_func_context.in_iteration {
                        parser_context.push_err(Error::NotInLoop(next.location.clone(), String::from("break")));
                    } 
                    TypedExpr{expr: Expr::Break, r#type: FullType::new_const(&Type::Never), loc: next.location.clone(), return_expr: ReturnExpr::None}
                },

                Keyword::Continue => {
                    self.skip_next_item();
                    if !parser_func_context.in_iteration {
                        parser_context.push_err(Error::NotInLoop(next.location.clone(), String::from("continue")));   
                    }
                    TypedExpr{expr: Expr::Continue, r#type: FullType::new_const(&Type::Never), loc: next.location.clone(), return_expr: ReturnExpr::None}
                },

                Keyword::While => self.parse_while(parser_func_context, parser_context),
                
                _ => {
                    // if we don't know what this statement is, parse it as an expr
                    let expr = self.parse_expr(parser_func_context, parser_context);
                    expr
                }
            },

            _ => {
                // if we don't know what this statement is, parse it as an expr
                let expr = self.parse_expr(parser_func_context, parser_context);
                expect_semicolon!(self, parser_context);
                expr
            }
        };
        parser_context.close_temporary_scope(parser_func_context);
        out
    }

    /// Parse an array literal into a Vec.
    fn parse_array_literal_to_vec(&mut self,
        for_type: &Type,
        loc_in: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Vec<TypedExpr> {
        let mut loc = loc_in.clone();

        expect_punct!(self, parser_context, Punct::OpenBracket);

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
            let o_cast_expr = try_create_cast(&FullType::new_const(&inner_type), &v, CastType::Implicit);
            match o_cast_expr {
                Some(cast_expr) => { out.push(cast_expr) },
                None => { 
                    parser_context.errors.push(Error::TypeFailureMemberCreation(next_item.location, idx.to_string().clone(), inner_type.clone(), v.r#type.r#type.clone()));
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
                        _ => {
                            parser_context.push_err(self.unexpected_token_error_raw(next_item.span, &loc.clone(), "expecting ',' or ']'"));
                            self.skip_next_item();
                        },
                    }
                },
                _ => {
                    parser_context.push_err(self.unexpected_token_error_raw(next_item.span, &loc.clone(), "expecting ',' or ']'"));
                    self.skip_next_item();
                },
            }
        }

        out
    }

    /// Parse an object literal into a Vec that can be used by other parts of the parser.
    /// It's a Vec rather than a HashMap because there may be order to the initialization.
    fn parse_object_literal_to_vec(&mut self,
        for_type: &Type,
        loc_in: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Vec<ObjectLiteralElem> {
        let mut loc = loc_in.clone();

        expect_punct!(self, parser_context, Punct::OpenBrace);

        //this is what we want
        let member_map: HashMap<String, Type> = match for_type {
            Type::UnsafeStruct{name: struct_name} => {
                let tm_entry = parser_context.type_map.get(struct_name).unwrap();
                match tm_entry {
                    TypeDecl::Struct{members, under_construction, export: _} => { 
                        if *under_construction {
                            parser_context.errors.push(Error::RecursiveTypeDefinition(loc.clone()));
                        }
                        Member::get_member_type_map(members)
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
            let next_item = self.peek_next_item();
            let token = next_item.token;

            match token {
                Token::EoF => {
                    parser_context.push_err(Error::UnexpectedEoF(next_item.location.clone(), String::from("expecting '}'")));
                    break;
                },
                Token::Punct(p) => {
                    match p {
                        Punct::CloseBrace => {
                            loc.end = next_item.location.end.clone();
                            self.skip_next_item();
                            break;
                        },
                        _ => {
                            parser_context.push_err(self.unexpected_token_error_raw(next_item.span, &loc.clone(), "expecting ',' or '}'"));
                            self.skip_next_item();
                        },
                    }
                },
                Token::Ident(i) => {
                    self.skip_next_item();
                    expect_punct!(self, parser_context, Punct::Colon);
                    let v = self.parse_expr(parser_func_context, parser_context);

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
                            let o_cast = try_create_cast(&FullType::new_const(member_map_elem), &v, CastType::Implicit);
                            match o_cast {
                                None => {
                                    parser_context.errors.push(Error::TypeFailureMemberCreation(next_item.location, i.to_string().clone(), member_map_elem.clone(), v.r#type.r#type.clone()));
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
                                Punct::CloseBrace => {},
                                Punct::Comma => {
                                    self.skip_next_item();
                                },
                                _ => {
                                    parser_context.push_err(self.unexpected_token_error_raw(next_item.span, &loc.clone(), "expecting ',' or '}'"));
                                }
                            }
                        },
                        _ => {
                            parser_context.push_err(self.unexpected_token_error_raw(next_item.span, &loc.clone(), "expecting ',' or '}'"));
                        }
                    }
                }, 
                _ => {
                    parser_context.push_err(self.unexpected_token_error_raw(next_item.span, &loc.clone(), "expecting ',' or '}'"));
                    self.skip_next_item();
                },
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

        out
    }

    /// This parses an object literal and uses it to construct something. As a side effect it
    /// creates a variable called '__scratch_malloc' which is supposed to hold the call to malloc.
    /// This is perhaps not brilliant
    /*
    fn parse_object_literal_constructor(&mut self,
        for_type: &Type,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
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
        TypedExpr{expr: Expr::ConstructFromObjectLiteral(for_type.clone(), out), is_const: true, loc: loc, r#type: for_type.clone()}
    }
    */

    fn parse_new(&mut self,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        self.skip_next_item();
        let (te, t) = self.parse_constructor_call(parser_func_context, parser_context);
        TypedExpr{expr: Expr::FreeUserTypeWrap(Box::new(te)), r#type: t, loc: loc, return_expr: ReturnExpr::None}
    }

    fn parse_named_component(&mut self,
        lhs: &TypedExpr,
        component: &String,
        members: &Vec<Member>,
        type_map: &HashMap<String, Type>,
        loc: &SourceLocation,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let o_mem = members.iter().find(|x| x.name == *component);
        match o_mem {
            None => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), lhs.r#type.r#type.clone(), component.clone()));
                TypedExpr{
                    expr: Expr::NamedMember(Box::new(lhs.clone()), component.clone()),
                    r#type: FullType::new(&Type::Undeclared, lhs.r#type.mutability),
                    loc: loc.clone(), return_expr: ReturnExpr::None
                }
            },
            Some(mem) => {
                let new_type = transform_type(&mem.r#type, type_map, loc, parser_context);
                TypedExpr{
                    expr: Expr::NamedMember(Box::new(lhs.clone()), component.clone()),
                    r#type: FullType::new(&new_type, lhs.r#type.mutability),
                    loc: loc.clone(), return_expr: ReturnExpr::None
                }
            }
        }
    }

    ///A square bracket member access.
    fn parse_dynamic_component(&mut self, 
        lhs: &TypedExpr,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        //expect_punct!(self, parser_context, Punct::OpenBracket);
        self.skip_next_item();
        let lhs_type = &lhs.r#type;
        match &lhs_type.r#type {
            Type::UnsafeArray(inner_type) => {
                self.parse_unsafe_array_dynamic_component(lhs, &inner_type, parser_func_context, parser_context)
            },
            _ => {
                parser_context.push_err(Error::NoComponents(lhs.loc.clone(), lhs_type.r#type.clone()));
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

            if lookahead_item.token.is_eof() {
                break;
            }

            let expr = self.parse_expr(parser_func_context, parser_context);
            loc.extend_right(&expr.loc);
            out_arr_types.push(expr.r#type.r#type.clone());
            out_arr.push(expr);
            
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
            TypedExpr{expr: Expr::TupleLiteral(out_arr), loc: loc, r#type: FullType::new_const(&Type::Tuple(out_arr_types)), return_expr: ReturnExpr::None}
        }
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
            "__memoryGrow" => Some(self.parse_mem_grow(parser_func_context, parser_context)),
            "__trap" => Some(self.parse_trap(parser_func_context, parser_context)),
            "__sizeof" => Some(self.parse_sizeof(parser_func_context, parser_context)),
            _ => None
        }
    }

    fn parse_number(&mut self, 
        n: &Number<&'b str>,
        loc: &SourceLocation,
    ) -> TypedExpr {
        self.skip_next_item();
        match n.kind() {
            NumberKind::Hex | NumberKind::DecI | NumberKind::Bin | NumberKind::Oct => {
                let number = n.parse_i128().unwrap();
                TypedExpr{expr: Expr::IntLiteral(number), r#type: FullType::new_const(&Type::Int(number, number)), loc: loc.clone(), return_expr: ReturnExpr::None}
            },
            NumberKind::DecF => {
                let number = n.parse_f64();
                TypedExpr{expr: Expr::FloatLiteral(number.unwrap()), r#type: FullType::new_const(&Type::Number), loc: loc.clone(), return_expr: ReturnExpr::None}
            },
        }
    }

    fn parse_ident_expr(&mut self,
        o_holding: &Option<TypedExpr>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = self.peek_next_location();
        let err_ret = TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::Undeclared), loc: loc.clone(), return_expr: ReturnExpr::None};
        let next = expect_next!(self, parser_context, err_ret);

        let id = expect_ident!(next, parser_context, "Expecting identifier declaration");
        let mut expr = match o_holding{
            None => {
                let o_sv = parser_context.get_scoped_var(&id.to_string());
                match o_sv {
                    Some(sv) => match sv {
                        ScopedVar::ClosureRef{internal_name, r#type, guard_type: o_guard_type, mutability: _} => {
                            let is_new = !parser_func_context.closure.iter().any(|x| x.internal_name.eq(internal_name));
                            if is_new {
                                parser_func_context.closure.push(ClosureRef{internal_name: internal_name.clone(), r#type: r#type.clone()})
                            }
                            
                            if o_guard_type.is_none() {
                                TypedExpr{expr: Expr::ClosureVariableUse(internal_name.clone()), r#type: r#type.clone(), loc: next.location.clone(), return_expr: ReturnExpr::None}
                            } else {
                                let guard_type = o_guard_type.as_ref().unwrap().clone();
                                let inner = Box::new(TypedExpr{expr: Expr::ClosureVariableUse(internal_name.clone()), r#type: r#type.clone(), loc: next.location.clone(), return_expr: ReturnExpr::None});
                                guard_downcast_expr(&FullType::new(&guard_type, r#type.mutability), inner, parser_context)
                            }
                        },
                        ScopedVar::Local{internal_name, r#type, guard_type: o_guard_type, mutability: _} => {
                            if o_guard_type.is_none() {
                                TypedExpr{expr: Expr::LocalVariableUse(internal_name.clone()), r#type: r#type.clone(), loc: next.location.clone(), return_expr: ReturnExpr::None}
                            } else {
                                let guard_type = o_guard_type.as_ref().unwrap().clone();
                                let inner = Box::new(TypedExpr{expr: Expr::LocalVariableUse(internal_name.clone()), r#type: r#type.clone(), loc: next.location.clone(), return_expr: ReturnExpr::None});
                                guard_downcast_expr(&FullType::new(&guard_type, r#type.mutability), inner, parser_context)
                            }
                        },                            
                    },
                    None => {
                        let o_g = parser_context.get_global_var(&id);
                        match o_g {
                            Some(g_var) => 
                                TypedExpr{expr: Expr::GlobalVariableUse(id.to_string()), r#type: g_var.r#type.clone(), loc: next.location.clone(), return_expr: ReturnExpr::None},
                            None => {
                                //might be a type literal
                                let o_type = self.parse_type_from_ident(&id, Commitment::Speculative, parser_context);
                                match o_type {
                                    Some(t) => 
                                        TypedExpr{
                                            expr: Expr::TypeLiteral(FullType::new_const(&t)), r#type: FullType::new_const(&Type::TypeLiteral(Box::new(FullType::new_const(&t)))), 
                                            loc: next.location.clone(), return_expr: ReturnExpr::None},
                                    _ => {
                                        // might be a function call
                                        let o_f_c = self.try_parse_func_call(&None, &id, &next.location, parser_func_context, parser_context);
                                        match o_f_c {
                                            Some(f_c) => f_c,
                                            None => {
                                                // might be a build in
                                                let o_built_in = self.try_parse_built_in(&id, parser_func_context, parser_context);
                                                match o_built_in {
                                                    Some(built_in) => built_in,
                                                    None => {
                                                        //module check
                                                        if parser_context.import_namespace_map.contains_key(&id) {
                                                            TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::ModuleLiteral(id.clone())), loc: next.location.clone(), return_expr: ReturnExpr::None}
                                                        } else {
                                                            parser_context.push_err(Error::VariableNotRecognized(next.location.clone(), id.clone()));
                                                            return err_ret
                                                        }
                                                    }
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
                match &holding.r#type.r#type {
                    Type::ModuleLiteral(m) => {
                        let full_name = format!("{}.{}", m, id);
                        let o_g = parser_context.global_imports.iter().find(|&x| x.name == full_name);
                        match o_g {
                            Some(g_var) => 
                                TypedExpr{expr: Expr::GlobalVariableUse(full_name), r#type: g_var.r#type.clone(), loc: next.location.clone(), return_expr: ReturnExpr::None},
                            None => {
                                let o_f_c = self.try_parse_func_call(&Some(m.clone()), &id, &next.location, parser_func_context, parser_context);
                                match o_f_c {
                                    Some(f_c) => f_c,
                                    None => {
                                        parser_context.push_err(Error::VariableNotRecognized(next.location.clone(), full_name.clone()));
                                        //give up
                                        return err_ret;
                                    },
                                }
                            },
                        }
                    },
                    Type::Int(_, _) => self.parse_int_component(&holding, &id, &next.location, parser_func_context, parser_context),
                    Type::UnsafePtr => self.parse_unsafe_ptr_component(&holding, &id, &next.location, parser_func_context, parser_context),
                    Type::UnsafeStruct{name} => {
                        let tm_entry = parser_context.type_map.get(name).unwrap().clone();
                        match tm_entry {
                            TypeDecl::Struct{members, under_construction: _, export: _} => self.parse_named_component(&holding, &id, &members, &HashMap::new(), &next.location, parser_context),
                            _ => unreachable!()
                        }
                    },
                    Type::UserClass{name, type_args} => {
                        let tm_entry = parser_context.type_map.get(name).unwrap().clone();
                        match tm_entry {
                            TypeDecl::UserClass{members, under_construction: _, export: _, type_args: given_type_args, member_funcs: _, constructor: _, storage: _} => {
                                let m = members.iter().find(|x| x.name == *id);
                                if m.is_some() {
                                    let mut type_map = new_type_map(&given_type_args);
                                    let mut i = 0;
                                    while i < type_args.len() && i < given_type_args.len() {
                                        type_map.insert(given_type_args[i].name.clone(), type_args[i].clone());
                                        i += 1;
                                    }
                                    self.parse_named_component(&holding, &id, &members, &type_map, &next.location, parser_context)
                                } else {
                                    self.parse_type_member_function_call(&holding, &name, &type_args, &id, &loc, parser_func_context, parser_context)
                                }
                            },
                            _ => unreachable!()
                        }
                    },
                    Type::UnsafeOption(_) => self.parse_unsafe_option_component(&holding, &id, &next.location, parser_func_context, parser_context),
                    Type::UserType{name, type_args, inner: _} => 
                        self.parse_type_member_function_call(&holding, &name, &type_args, &id, &loc, parser_func_context, parser_context),
                    Type::VariableUsage{name: _, constraint} => 
                        self.parse_type_variable_member_function_call(&holding, &constraint, &id, &loc, parser_func_context, parser_context),
                    _ => {
                        parser_context.push_err(Error::NoComponents(next.location.clone(), holding.r#type.r#type.clone()));
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
                        let o_unop_type_cast = wa1_types::get_unary_op_type_cast(get_op_type_for_unop(&op), &expr.r#type);
                        let loc = SourceLocation::new(expr.loc.start.clone(), lookahead_item.location.end.clone());
                        return self.create_unary_operator_expr(op, o_unop_type_cast, &expr, &loc, parser_context);
                    }
                },
                _ => {},
            };

            //now, other things it could be
            match lookahead {
                Token::Punct(Punct::OpenParen) => {
                    match &expr.r#type.r#type {
                        Type::Func{func_type} => {
                            let args = self.parse_function_call_args(&None, &func_type.in_types, parser_func_context, parser_context);
                            let loc = SourceLocation::new(lookahead_item.location.start.clone(), args.last().map(|a| a.loc.end.clone()).unwrap_or(next.location.end.clone()));
                            expr = TypedExpr{expr: Expr::DynamicFuncCall(Box::new(expr.clone()), args), r#type: func_type.out_type.clone(), loc: loc, return_expr: ReturnExpr::None};
                            continue;
                        },
                        _ => {
                            let args = self.parse_function_call_args(&None, &vec![], parser_func_context, parser_context);
                            let loc = SourceLocation::new(lookahead_item.location.start.clone(), next.location.end.clone());
                            parser_context.push_err(Error::TypeFailureFuncCall(loc.clone()));
                            expr = TypedExpr{expr: Expr::DynamicFuncCall(Box::new(expr.clone()), args), r#type: expr.r#type.clone(), loc: loc, return_expr: ReturnExpr::None};
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
    ) -> TypedExpr {
        let expr = self.parse_primary(&None, parser_func_context, parser_context);
        self.parse_expr_1(&expr, 0, parser_func_context, parser_context)
    }

    fn get_prefix_unary_operator_for_token(&self, 
        span: Span, 
        location: &SourceLocation, 
        token: &Token<&'b str>,
        parser_context: &mut ParserContext,
    ) -> UnaryOperator {
        match token {
            Token::Punct(Punct::Bang) => UnaryOperator::LogicalNot,
            Token::Punct(Punct::Tilde) => UnaryOperator::BitNot,
            Token::Punct(Punct::Dash) => UnaryOperator::Minus,
            Token::Punct(Punct::Plus) => UnaryOperator::Plus,
            Token::Punct(Punct::DoublePlus) => UnaryOperator::PrefixIncrement,
            Token::Punct(Punct::DoubleDash) => UnaryOperator::PrefixDecrement,
            _ => {
                parser_context.push_err(self.unexpected_token_error_raw(span, location, "unexpected binary operator"));
                UnaryOperator::LogicalNot
            },
        }
    }

    fn create_unary_operator_expr(&mut self,
        op: UnaryOperator,
        o_unop_type_cast: Option<UnOpTypeCast>,
        inner: &TypedExpr,
        loc: &SourceLocation,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        match o_unop_type_cast {
            Some(unop_type_cast) => {
                let o_cast = create_cast(&unop_type_cast.r#type, &inner, &unop_type_cast.type_cast);
                let out = match o_cast {
                    Some(cast) => Box::new(cast),
                    None => {
                        parser_context.push_err(Error::TypeFailureUnaryOperator(loc.clone()));
                        Box::new(inner.clone())
                    }
                };
                return TypedExpr{expr: Expr::UnaryOperator{op: op, expr: out}, r#type: unop_type_cast.r#type, loc: loc.clone(), return_expr: ReturnExpr::None}
            },
                
            None => {
                parser_context.push_err(Error::TypeFailureUnaryOperator(loc.clone()));
                let inner_type = inner.r#type.clone();
                return TypedExpr{expr: Expr::UnaryOperator{op: op, expr: Box::new(inner.clone())}, r#type: inner_type, loc: loc.clone(), return_expr: ReturnExpr::None}
            },
        }
    }

    /// Parse a simple atomic expression.
    fn parse_primary(&mut self,
        holding: &Option<TypedExpr>,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let lookahead_item = self.peek_next_item();
        let mut err_ret = TypedExpr{expr: Expr::NoOp, r#type: FullType::new_const(&Type::Undeclared), loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None};
        
        let lookahead = lookahead_item.token;
        let o_uod = get_unary_operator_data(&lookahead);
        match o_uod {
            Some(uod) => {
                if uod.fix == Fix::Prefix || uod.fix == Fix::Both {
                    let mut loc = lookahead_item.location.clone();
                    err_ret.loc = loc.clone();
                    let op = self.get_prefix_unary_operator_for_token(lookahead_item.span, &lookahead_item.location, &lookahead, parser_context);
                    self.skip_next_item();
                    let inner = self.parse_expr(parser_func_context, parser_context);
                    loc.end = inner.loc.end.clone();
                    let o_unop_type_cast = wa1_types::get_unary_op_type_cast(get_op_type_for_unop(&op), &inner.r#type);
                    return self.create_unary_operator_expr(op, o_unop_type_cast, &inner, &loc, parser_context)
                }
            },
            _ => {},
        };

        match lookahead {
            Token::Number(n) => { return self.parse_number(&n, &lookahead_item.location); },
            
            Token::Bool(b) => {
                self.skip_next_item();
                return TypedExpr{expr:Expr::BoolLiteral(b.is_true()), r#type: FullType::new_const(&Type::Bool), loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None};
            },

            Token::Null => {
                self.skip_next_item();
                return Parser::create_null(&lookahead_item.location);
            },

            Token::UnsafeNull => {
                self.skip_next_item();
                return Parser::create_unsafe_null(&lookahead_item.location);
            },

            Token::String(sl) => {
                self.skip_next_item();
                return TypedExpr{expr:Expr::StringLiteral(sl.no_quote().to_string()), r#type: FullType::new_const(&Type::String), loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None};
            },

            Token::Keyword(k) => {
                match k {
                    Keyword::New => {
                        self.parse_new(parser_func_context, parser_context)
                    },
                    Keyword::UnsafeStatic => {
                        self.parse_unsafe_static(parser_func_context, parser_context)
                    },
                    Keyword::Void => { 
                        self.skip_next_item();
                        return TypedExpr{expr:Expr::Void, r#type: FullType::new(&Type::FakeVoid, Mutability::Const), loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None}; 
                    },
                    Keyword::If => self.parse_if(parser_func_context, parser_context),
                    Keyword::Fn => {
                        let o_func = self.parse_function_decl_main_phase(false, parser_func_context, parser_context);
                        match o_func{
                            Some(f) => f,
                            None => err_ret
                        }
                    },
                    Keyword::UnsafeSome => self.parse_unsafe_some(parser_func_context, parser_context),
                    Keyword::This => {
                        self.skip_next_item();
                        if parser_func_context.this_type.is_some() {
                            let this_type = parser_func_context.this_type.clone().unwrap();
                            TypedExpr{expr: Expr::LocalVariableUse(String::from("this")), r#type: this_type, loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None}
                        } else {
                            parser_context.push_err(Error::NoThis(lookahead_item.location.clone()));
                            TypedExpr{expr: Expr::LocalVariableUse(String::from("this")), r#type: FullType::new_const(&Type::Undeclared), loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None}
                        }
                    },
                    Keyword::Mut => {
                        let full_type = self.parse_full_type(parser_context);
                        TypedExpr{expr: Expr::TypeLiteral(full_type.clone()), r#type: FullType::new_const(&Type::TypeLiteral(Box::new(full_type.clone()))), loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None}
                    },
                    _ => {
                        self.skip_next_item();
                        let t = self.parse_type_from_keyword(&k, &lookahead_item.location, parser_context);
                        TypedExpr{expr: Expr::TypeLiteral(FullType::new_const(&t)), r#type: FullType::new_const(&Type::TypeLiteral(Box::new(FullType::new_const(&t)))), loc: lookahead_item.location.clone(), return_expr: ReturnExpr::None}
                    },
                }
            },

            Token::Ident(_) => self.parse_ident_expr(holding, parser_func_context, parser_context),

            Token::Punct(p) => match p {
                Punct::OpenParen => self.parse_paren_expr(parser_func_context, parser_context),
                _ => {
                    self.skip_next_item();
                    parser_context.push_err(self.unexpected_token_error_raw(lookahead_item.span, &lookahead_item.location, "unexpected punctuation"));
                    err_ret
                },
            },

            _ => {
                self.skip_next_item();
                parser_context.push_err(self.unexpected_token_error_raw(lookahead_item.span, &lookahead_item.location, "unexpected token"));
                err_ret
            },
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
    ) -> TypedExpr {
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
                        let mut rhs = r_rhs;
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
                        lhs = self.parse_binary_op(&op, &lhs, &rhs, parser_context);
                    } else {
                        break;
                    }
                }
            }
        };
        return lhs.clone();
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
        let add = "export fn addd(x: Number, y: Number, z: Number) -> Number {
            let a = 1
            return x + y + z + a
        }";

        let mut parser = Parser::new(add).unwrap();
        let (ast, _) = parser.parse_full(UnsafeParseMode::Safe, & mut DummyImporter{}, &String::from(""), StartFuncType::Start, &String::from(""));
        println!("{:#?}", ast);
    }
}