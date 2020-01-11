use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use ast::Exports;
use std::collections::HashMap;

use ress::prelude::*;
use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
use errs::prelude::*;

macro_rules! should_be_ok {
    ($e:ident) => (if $e.is_err() { return; }; let $e = $e.unwrap();)
}

fn fake_typed_expr(return_type: &Type) -> TypedExpr {
    TypedExpr{expr: Expr::Block(vec![]), r#type: return_type.clone(), is_const: true, loc: SourceLocation::new(Position::new(0, 0), Position::new(0, 0))}
}

fn exports_from_parser_context(parser_context: &ParserContext) -> Exports {
    let mut globals: Vec<GlobalVariableDecl> = vec![];
    let mut funcs: Vec<FuncDecl>  = vec![];
    let mut types: Vec<TypeDecl>  = vec![];

    for g in &parser_context.globals {
        if g.export {
            globals.push(g.clone());
        }
    }

    for f in &parser_context.funcs {
        if f.decl.export {
            funcs.push(f.decl.clone())
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

    Exports{ globals, funcs, types }
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
        let decl = self.parse_function_decl(true, parser_context);
        should_be_ok!(decl);
        let return_type = decl.return_type.clone();
        let func = Func{
            decl: decl, local_vars: vec![], closure: vec![], 
            local_var_map: HashMap::new(), body: fake_typed_expr(&return_type), 
        };

        let idx = parser_context.funcs.len();
        parser_context.func_map.insert(func.decl.name.clone(), idx as u32);
        parser_context.funcs.push(func); 
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
        is_unsafe: bool
    ) -> Exports {
        let mut parser_context = ParserContext::new(is_unsafe, true);
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