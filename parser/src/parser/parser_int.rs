use crate::Parser;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::create_cast;

use ast::prelude::*;
use types::prelude::*;
pub use errs::Error;
pub use errs::prelude::*;


use lazy_static;

lazy_static!{
    static ref INT_CLZ_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_const(&Type::Int(0, 32)), in_types: vec![FullType::new_const(&INT_S_32)]},
        FuncType{out_type: FullType::new_const(&Type::Int(0, 64)), in_types: vec![FullType::new_const(&INT_S_64)]},
        FuncType{out_type: FullType::new_const(&Type::Int(0, 32)), in_types: vec![FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&Type::Int(0, 64)), in_types: vec![FullType::new_const(&INT_U_64)]},
    ];

    static ref INT_CTZ_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_const(&Type::Int(0, 32)), in_types: vec![FullType::new_const(&INT_S_32)]},
        FuncType{out_type: FullType::new_const(&Type::Int(0, 64)), in_types: vec![FullType::new_const(&INT_S_64)]},
        FuncType{out_type: FullType::new_const(&Type::Int(0, 32)), in_types: vec![FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&Type::Int(0, 64)), in_types: vec![FullType::new_const(&INT_U_64)]},
    ];

    static ref INT_SL_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_const(&INT_S_32), in_types: vec![FullType::new_const(&INT_S_32), FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&INT_S_64), in_types: vec![FullType::new_const(&INT_S_64), FullType::new_const(&INT_U_64)]},
        FuncType{out_type: FullType::new_const(&INT_U_32), in_types: vec![FullType::new_const(&INT_U_32), FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&INT_U_64), in_types: vec![FullType::new_const(&INT_U_64), FullType::new_const(&INT_U_64)]},
    ];

    static ref INT_SR_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_const(&INT_S_32), in_types: vec![FullType::new_const(&INT_S_32), FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&INT_S_64), in_types: vec![FullType::new_const(&INT_S_64), FullType::new_const(&INT_U_64)]},
        FuncType{out_type: FullType::new_const(&INT_U_32), in_types: vec![FullType::new_const(&INT_U_32), FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&INT_U_64), in_types: vec![FullType::new_const(&INT_U_64), FullType::new_const(&INT_U_64)]},
    ];

    static ref INT_TTS_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_const(&INT_S_32), in_types: vec![FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&INT_S_64), in_types: vec![FullType::new_const(&INT_U_64)]},
    ];

    static ref INT_UN_METHODS: Vec<FuncType> = vec![
        FuncType{out_type: FullType::new_const(&INT_U_32), in_types: vec![FullType::new_const(&INT_U_32)]},
        FuncType{out_type: FullType::new_const(&INT_U_64), in_types: vec![FullType::new_const(&INT_U_64)]},
    ];
}

pub fn get_int_member_funcs(_lower: i128, _upper: i128) -> Vec<MemberFunc> {
    vec![] //FIXME
}

impl<'a> Parser<'a> {
    pub (crate) fn parse_int_component(&mut self,
        holding: &TypedExpr,
        id: &String,
        loc: &SourceLocation,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let func_types: &Vec<FuncType> = match id.as_str() {
            "countLeadingZeros" => &INT_CLZ_METHODS,
            "countTrailingZeros" => &INT_CTZ_METHODS,
            "shiftLeft" => &INT_SL_METHODS,
            "shiftRight" => &INT_SR_METHODS,
            "truncateToSigned" => &INT_TTS_METHODS,
            "unsignedNegate" => &INT_UN_METHODS,
            _ => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), holding.r#type.r#type.clone(), id.clone()));
                return TypedExpr{expr: Expr::MemberFuncCall(Box::new(holding.clone()), id.clone(), vec![]), r#type: holding.r#type.clone(), loc: loc.clone()};
            }
        };
    
        let mut arg_types: Vec<FullType> = vec![holding.r#type.clone()];
        let (args, _) = self.parse_function_call_args_unchecked(parser_func_context, parser_context);
        for arg in &args {
            arg_types.push(arg.r#type.clone());    
        }
        let t = get_type_casts_for_function_set(&func_types, &arg_types);

        match t {
            None => {
                parser_context.push_err(Error::ObjectHasNoMember(loc.clone(), holding.r#type.r#type.clone(), id.clone()));
                TypedExpr{expr: Expr::MemberFuncCall(Box::new(holding.clone()), id.clone(), vec![]), r#type: holding.r#type.clone(), loc: loc.clone()}
            },
            Some(FuncCallTypeCast{func_type, arg_type_casts}) => {
                let out_self_type_cast = arg_type_casts.get(0).unwrap().clone();
                let out_self_type = get_type_from_type_cast(&out_self_type_cast, &func_type.in_types.get(0).unwrap());
                let o_cast = create_cast(&out_self_type, holding, &out_self_type_cast);

                let mut out_args = vec![];
                let mut i = 1;
                while i < arg_type_casts.len() {
                    let out_type_cast = arg_type_casts.get(i).unwrap().clone();
                    let out_type = get_type_from_type_cast(&out_type_cast, &func_type.in_types.get(i).unwrap());
                    let o_cast = create_cast(&out_type, &args[i-1], &out_type_cast);
                    out_args.push(o_cast.unwrap().clone());
                    i += 1;
                }

                TypedExpr{expr: Expr::MemberFuncCall(Box::new(o_cast.unwrap().clone()), id.clone(), out_args), r#type: func_type.out_type.clone(), loc: loc.clone()}
            }
        }
    }
}