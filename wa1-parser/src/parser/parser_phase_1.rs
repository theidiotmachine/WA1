use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::{UnsafeParseMode, ParserPhase};

use wa1_ast::Exports;

use wa1_lexer::prelude::*;
use wa1_ast::prelude::*;
pub use wa1_errs::Error;

fn exports_from_parser_context(parser_context: &ParserContext) -> Exports {
    let mut global_decls: Vec<GlobalVariableDecl> = vec![];
    let mut global_imports: Vec<GlobalVariableImport> = vec![];
    let mut func_decls: Vec<FuncDecl> = vec![];
    let mut func_imports: Vec<FuncDecl> = vec![];
    let mut generic_func_decls: Vec<GenericFunc> = vec![];
    let mut types: Vec<TypeDecl> = vec![];

    for g in &parser_context.global_decls {
        if g.export {
            global_decls.push(g.clone());
        }
    }

    for g in &parser_context.global_imports {
        if g.export {
            global_imports.push(g.clone());
        }
    }

    for f in &parser_context.func_decls {
        if f.decl.export {
            func_decls.push(f.decl.clone())
        }
    }

    for f in &parser_context.func_imports {
        if f.export {
            func_imports.push(f.clone())
        }
    }

    for f in &parser_context.generic_func_decls {
        if f.func.decl.export {
            generic_func_decls.push(f.clone());
        }
    }

    for t in &parser_context.type_map {
        let td = t.1;
        match td{
            TypeDecl::Struct{name: _, struct_type: _, under_construction: _, export} => {
                if *export {
                    types.push(td.clone())
                }
            },
            TypeDecl::Alias{name: _, of: _, export} => {
                if *export {
                    types.push(td.clone())
                }
            },
            TypeDecl::Type{name: _, inner: _, type_args: _, export, member_funcs: _, constructor: _, under_construction: _} => {
                if *export {
                    types.push(td.clone())
                }
            }
        }
    }

    Exports{ global_imports, global_decls, func_imports, func_decls, generic_func_decls, types }
}

impl<'a> Parser<'a> {
    fn parse_export_decl_exports_phase(&mut self,
        parser_context: &mut ParserContext,
    ) -> () {
        self.skip_next_item();
        let next = self.peek_next_item();
        let token = &next.token;
        match &token {
            Token::Keyword(ref k) => match k {   
                Keyword::Fn => self.parse_func_exports_phase(parser_context),
                Keyword::UnsafeStruct => {let _ = self.parse_struct_decl(true, parser_context);},
                //Keyword::Const => self.parse_global_exports_phase(true, parser_context),
                Keyword::Let => self.parse_global_exports_phase(parser_context),
                Keyword::Alias => { self.parse_alias(true, parser_context);},
                Keyword::Implement => { self.parse_trait_impl(true, ParserPhase::ExportsPhase, parser_context);},
                Keyword::Type => { self.parse_type_decl(true, ParserPhase::ExportsPhase, parser_context);},
                Keyword::Trait => { self.parse_trait_decl(true, parser_context);},
                _ => {},
            },
            _ => {},
        }
    }
    
    fn parse_func_exports_phase(&mut self,
        parser_context: &mut ParserContext,
    ) -> () {
        let mut fake_parser_func_context = ParserFuncContext::new(&None);
        self.parse_named_function_decl_export_phase(true, &mut fake_parser_func_context, parser_context);
    }

    fn parse_global_exports_phase(&mut self,
        parser_context: &mut ParserContext,
    ) -> () {
        let mut fake_parser_func_context = ParserFuncContext::new(&None);
        let _ = self.parse_variable_decl(true, true, &mut fake_parser_func_context, parser_context);
    }

    /// Phase 1 parse. Produces the file outline, to be used by the import code
    pub fn parse_exports_phase(&mut self, 
        file_name: &String,
    ) -> Exports {
        let mut parser_context = ParserContext::new(UnsafeParseMode::ExportsPhase, file_name);
        loop {
            if self.look_ahead.token.is_eof() {
                break;
            }
            let next = self.peek_next_item();
            let token = &next.token;
            match &token {
                Token::Keyword(ref k) => match k {
                    Keyword::Export => self.parse_export_decl_exports_phase(&mut parser_context),
                    _ => self.skip_next_item(),
                },
                _ => self.skip_next_item(),
            }
        }
        exports_from_parser_context(&parser_context)
    }
}