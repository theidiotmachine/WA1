use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use ast::Exports;

use ress::prelude::*;
use ast::prelude::*;
pub use errs::Error;

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
            TypeDecl::Class{name: _, class_type: _, export} => {
                if *export {
                    types.push(td.clone())
                }
            },
        }
    }

    Exports{ global_imports, global_decls, func_imports, func_decls, generic_func_decls, types }
}

impl<'a> Parser<'a> {
    fn parse_phase_1_export_decl(&mut self,
        parser_context: &mut ParserContext,
    ) -> () {
        self.skip_next_item();
        let next = self.peek_next_item();
        let token = &next.token;
        match &token {
            Token::Keyword(ref k) => match k {   
                Keyword::Function => self.parse_phase_1_func(parser_context),
                Keyword::UnsafeStruct => {let _ = self.parse_struct_decl(true, parser_context);},
                Keyword::Const => self.parse_phase_1_global(true, parser_context),
                Keyword::Let => self.parse_phase_1_global(false, parser_context),
                _ => {},
            },
            _ => {},
        }
    }
    
    fn parse_phase_1_func(&mut self,
        parser_context: &mut ParserContext,
    ) -> () {
        let mut fake_parser_func_context = ParserFuncContext::new();
        self.export_parse_named_function_decl(true, &mut fake_parser_func_context, parser_context);
    }

    fn parse_phase_1_global(&mut self,
        is_const: bool,
        parser_context: &mut ParserContext,
    ) -> () {
        let mut fake_parser_func_context = ParserFuncContext::new();
        let _ = self.parse_variable_decl(is_const, true, true, &mut fake_parser_func_context, parser_context);
    }

    /// Phase 1 parse. Produces the file outline, to be used by the import code
    pub fn parse_phase_1(&mut self, 
        is_unsafe: bool,
        file_name: &String,
    ) -> Exports {
        let mut parser_context = ParserContext::new(is_unsafe, file_name);
        loop {
            if self.look_ahead.token.is_eof() {
                break;
            }
            let next = self.peek_next_item();
            let token = &next.token;
            match &token {
                Token::Keyword(ref k) => match k {
                    Keyword::Export => self.parse_phase_1_export_decl(&mut parser_context),
                    _ => self.skip_next_item(),
                },
                _ => self.skip_next_item(),
            }
        }
        exports_from_parser_context(&parser_context)
    }
}