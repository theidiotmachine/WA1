use ast::prelude::*;

use parity_wasm::builder::module;
use parity_wasm::elements::{Module, ValueType, Local, Instructions, Instruction};
use parity_wasm::builder::{FunctionDefinition, FunctionBuilder};

use std::collections::HashMap;

pub use errs::Error;

fn get_ir_type(r#type: &Type) -> ValueType {
    match r#type {
        Type::Number => ValueType::F64,
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::Void => ValueType::I32, 
        _ => panic!()
    }
}

fn get_ir_return_type(r#type: &Type) -> Option<ValueType> {
    match r#type {
        Type::Void => None,
        _ => Some(get_ir_type(r#type))
    }
}

fn transform_expr(
    expr: &Expr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
) -> Vec<Instruction> {
    match expr {
        Expr::FloatLiteral(v) => {
            vec![Instruction::F64Const(v.to_bits())]
        },

        Expr::LocalVariableUse(lvu) => {
            let o_idx = local_var_map.get(lvu);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognised(lvu.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetLocal(*idx)]
                }
            }
        },

        Expr::GlobalVariableUse(vu) => {
            let o_idx = global_var_map.get(vu);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognised(vu.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetGlobal(*idx)]
                }
            }
        },
        /*
        Expr::IntLiteral(v) => {
            vec![Instruction::I64Const(*v)]
        },
        */
        Expr::BinaryOperator(bo) => {
            let mut vi: Vec<Instruction> = vec![];
                    
            if bo.op.is_assign_operator() {
                
            } else if bo.op.is_simple_operator() {
                vi.append(&mut transform_expr(&bo.lhs, global_var_map, local_var_map, errors));
                vi.append(&mut transform_expr(&bo.rhs, global_var_map, local_var_map, errors));
            }
            match bo.op {
                BinaryOperator::Plus => {
                    vi.push(Instruction::F64Add);
                },
                BinaryOperator::Minus => {
                    vi.push(Instruction::F64Sub);
                },
                BinaryOperator::Multiply => {
                    vi.push(Instruction::F64Mul);
                },
                BinaryOperator::Divide => {
                    vi.push(Instruction::F64Div);
                },
                _ => {
                    errors.push(Error::NotYetImplemented(String::from("operator")))
                }
            };

            vi
        },

        Expr::Parens(p) => {
            transform_expr(&p, global_var_map, local_var_map, errors)
        },

        _ => { errors.push(Error::NotYetImplemented(String::from("operator"))); vec![] },
    }
}

fn transform_stmt(stmt: &Stmt,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
) -> Vec<Instruction> {
    match stmt {
        Stmt::Return(o_expr) => {
            match o_expr {
                None => vec![Instruction::Return],
                Some(expr) => {
                    let mut this_vi = transform_expr(&expr, global_var_map, local_var_map, errors);
                    this_vi.push(Instruction::Return);
                    this_vi
                }
            }
        },
        Stmt::Variable(v) => {
            //first,  run the init expression
            let mut vi = vec![];
            match &v.init {
                Some(expr) => {
                    let mut this_vi = transform_expr(&expr, global_var_map, local_var_map, errors);
                    vi.append(& mut this_vi);
                    //then set the variable
                    let o_idx = local_var_map.get(&v.internal_name);
                    match o_idx {
                        Some(idx) => vi.push(Instruction::SetLocal(*idx)),
                        None => errors.push(Error::VariableNotRecognised(v.internal_name.clone())),
                    };
                },
                _ => {}
            }
            vi
        },
        Stmt::Expr(e) => {
            // an expr returns something, so run it
            let mut this_vi = transform_expr(&e, global_var_map, local_var_map, errors);
            // now pop what it returned
            this_vi.push(Instruction::Drop);
            this_vi
        },
        _ => {
            //todo: closures 
            vec![]
        }
    }
}

fn transform_func(func: &Func, 
    global_var_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
) -> FunctionDefinition{
    let fb = FunctionBuilder::new();
    let sb = fb.signature();
    let sb = sb.with_return_type(get_ir_return_type(&func.return_type));
    let mut params: Vec<ValueType> = vec![];
    for arg in &func.args {
        params.push(get_ir_type(&arg.r#type));
    }
    let sb = sb.with_params(params);
    let fb = sb.build();

    let mut locals: Vec<Local> = vec![];
    for lv in &func.local_vars {
        if !lv.arg {
            locals.push(Local::new(1, get_ir_type(&lv.r#type)));
        }
    }
    let fbb = fb.body();
    let fbb = fbb.with_locals(locals);

    let mut vi: Vec<Instruction> = vec![];
    for stmt in &func.body {
        let mut this_vi = transform_stmt(stmt, global_var_map, &func.local_var_map, errors);
        vi.append(&mut this_vi);
    }
    
    vi.push(Instruction::End);
    let fbb = fbb.with_instructions(Instructions::new(vi));

    let fb = fbb.build();

    fb.build()
}

pub fn transform(program: Program, errors: &mut Vec<Error>) -> Module {
    let mut m = module();
    for func in &program.funcs {
        if !func.import {
            m.push_function(transform_func(func, &program.global_var_map, errors));
        }
        if func.export {
            m = m.export().field(&func.name).internal().func(*(program.func_map.get(&func.name).unwrap())).build();
        }
    }
    m.build()
}