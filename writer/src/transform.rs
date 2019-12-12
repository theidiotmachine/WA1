use ast::prelude::*;

use parity_wasm::builder::module;
use parity_wasm::elements::{Module, ValueType, Local, Instructions, Instruction, BlockType};
use parity_wasm::builder::{FunctionDefinition, FunctionBuilder};

use std::collections::HashMap;

pub use errs::Error;
pub use types::Type;

fn get_ir_type(r#type: &Type) -> ValueType {
    match r#type {
        Type::Number => ValueType::F64,
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::FakeVoid => ValueType::I32, 
        Type::Boolean => ValueType::I32,
        _ => panic!()
    }
}

fn get_ir_return_type(r#type: &Type) -> Option<ValueType> {
    match r#type {
        Type::RealVoid => None,
        _ => Some(get_ir_type(r#type))
    }
}

fn transform_lvalue_get(
    l_value: &TypedLValueExpr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
) -> Vec<Instruction> {
    match &l_value.expr {
        LValueExpr::GlobalVariableAssign(name) => {
            let o_idx = global_var_map.get(name);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetGlobal(*idx)]
                }
            }
        },

        LValueExpr::LocalVariableAssign(name) => {
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetLocal(*idx)]
                }
            }
        },
        _ => { errors.push(Error::NotYetImplemented(String::from("lvalue get"))); vec![] }
    }
}

fn transform_lvalue_tee(
    l_value: &TypedLValueExpr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
) -> Vec<Instruction> {
    match &l_value.expr {
        LValueExpr::GlobalVariableAssign(name) => {
            let o_idx = global_var_map.get(name);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::SetGlobal(*idx), Instruction::GetGlobal(*idx)]
                }
            }
        },

        LValueExpr::LocalVariableAssign(name) => {
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::TeeLocal(*idx)]
                }
            }
        },
        _ => { errors.push(Error::NotYetImplemented(String::from("lvalue get"))); vec![] }
    }
}

fn transform_typed_expr(
    typed_expr: &TypedExpr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
) -> Vec<Instruction> {
    match &typed_expr.expr {
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
                    
            vi.append(&mut transform_typed_expr(&bo.lhs, global_var_map, local_var_map, func_map, errors));
            vi.append(&mut transform_typed_expr(&bo.rhs, global_var_map, local_var_map, func_map, errors));
        
            match bo.op {
                BinaryOperator::Plus => vi.push(Instruction::F64Add),
                BinaryOperator::Minus => vi.push(Instruction::F64Sub),
                BinaryOperator::Multiply => vi.push(Instruction::F64Mul),
                BinaryOperator::Divide => vi.push(Instruction::F64Div),
                BinaryOperator::GreaterThan => vi.push(Instruction::F64Gt),
                BinaryOperator::GreaterThanEqual => vi.push(Instruction::F64Ge),
                BinaryOperator::LessThan=> vi.push(Instruction::F64Lt),
                BinaryOperator::LessThanEqual => vi.push(Instruction::F64Le),

                //these are the bit instructions, but our strong typing should mean that this is safe
                BinaryOperator::LogicalAnd => vi.push(Instruction::I32And),
                BinaryOperator::LogicalOr => vi.push(Instruction::I32Or),
                BinaryOperator::BitAnd => vi.push(Instruction::I32And),
                BinaryOperator::BitOr => vi.push(Instruction::I32Or),
                BinaryOperator::BitXor => vi.push(Instruction::I32Xor),
                _ => {
                    errors.push(Error::NotYetImplemented(String::from("binary operator")))
                }
            };

            vi
        },

        Expr::UnaryOperator(uo) => {
            let mut vi: Vec<Instruction> = vec![];
            vi.append(&mut transform_typed_expr(&uo.expr, global_var_map, local_var_map, func_map, errors));
            match uo.op {
                UnaryOperator::LogicalNot => {
                    vi.push(Instruction::I32Eqz);
                },

                UnaryOperator::BitNot => {
                    vi.push(Instruction::I32Const(-1));
                    vi.push(Instruction::I32Xor);
                },

                UnaryOperator::Plus => {},

                UnaryOperator::Minus => {
                    let v: f64 = -1.0;
                    vi.push(Instruction::F64Const(v.to_bits()));
                    vi.push(Instruction::F64Mul);
                },
                _ => {
                    errors.push(Error::NotYetImplemented(String::from("unary operator")))
                }
            }
            vi
        }

        Expr::Parens(p) => {
            transform_typed_expr(&p, global_var_map, local_var_map, func_map, errors)
        },

        Expr::BoolLiteral(b) => {
            if *b {
                vec![Instruction::I32Const(1)]
            } else {
                vec![Instruction::I32Const(0)]
            }
        },

        Expr::Assignment(l_value, op, r_value) => {
            let mut vi: Vec<Instruction> = vec![];
            
            if *op == AssignmentOperator::Assign {
                vi.append(&mut transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, errors));
                vi.append(&mut transform_lvalue_tee(l_value, global_var_map, local_var_map, errors));
            } else {
                vi.append(&mut transform_lvalue_get(l_value, global_var_map, local_var_map, errors));
                vi.append(&mut transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, errors));
            
                match op {
                    AssignmentOperator::MinusAssign => vi.push(Instruction::F64Sub),
                    AssignmentOperator::PlusAssign => vi.push(Instruction::F64Add),
                    AssignmentOperator::MultiplyAssign => vi.push(Instruction::F64Mul),
                    AssignmentOperator::DivideAssign => vi.push(Instruction::F64Div),
                    AssignmentOperator::BitAndAssign => vi.push(Instruction::I32And),
                    AssignmentOperator::BitOrAssign => vi.push(Instruction::I32Or),
                    AssignmentOperator::BitXorAssign => vi.push(Instruction::I32Xor),
                    _ => {
                        errors.push(Error::NotYetImplemented(String::from("assignment operator")))
                    }
                }
                vi.append(&mut transform_lvalue_tee(l_value, global_var_map, local_var_map, errors));
            }

            vi
        },

        Expr::StaticFuncCall(name, args) => {
            let mut vi: Vec<Instruction> = vec![];
            
            for arg in args {
                vi.append(&mut transform_typed_expr(&arg, global_var_map, local_var_map, func_map, errors));
            }

            let o_func_id = func_map.get(name);
            if o_func_id.is_some() {
                vi.push(Instruction::Call(*o_func_id.unwrap()));
            } else {
                errors.push(Error::FuncNotRecognised(name.clone()));
            }

            vi
        },

        _ => { errors.push(Error::NotYetImplemented(String::from("expr"))); vec![] },
    }
}

fn transform_stmt(stmt: &Stmt,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
) -> Vec<Instruction> {
    match stmt {
        Stmt::Return(o_expr) => {
            match o_expr {
                None => vec![Instruction::Return],
                Some(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, errors);
                    this_vi.push(Instruction::Return);
                    this_vi
                }
            }
        },
        Stmt::VariableDecl(v) => {
            //first,  run the init expression
            let mut vi = vec![];
            match &v.init {
                Some(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, errors);
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
            let mut this_vi = transform_typed_expr(&e, global_var_map, local_var_map, func_map, errors);
            // now pop what it returned
            this_vi.push(Instruction::Drop);
            this_vi
        },
        Stmt::IfThen(c, t) => {
            let mut vi = vec![];
            
            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, errors);
            vi.append(&mut this_vi);
            
            vi.push(Instruction::If(BlockType::NoResult));
            
            let mut this_vi: Vec<Instruction> = transform_stmts(t, global_var_map, local_var_map, func_map, errors);
            vi.append(&mut this_vi);
            vi.push(Instruction::End);
            vi
        },

        Stmt::IfThenElse(c, t, e) => {
            let mut vi = vec![];
            
            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, errors);
            vi.append(&mut this_vi);
            
            vi.push(Instruction::If(BlockType::NoResult));
            
            let mut this_vi: Vec<Instruction> = transform_stmts(t, global_var_map, local_var_map, func_map, errors);
            vi.append(&mut this_vi);
            vi.push(Instruction::Else);
            let mut this_vi: Vec<Instruction> = transform_stmts(e, global_var_map, local_var_map, func_map, errors);
            vi.append(&mut this_vi);            
            vi.push(Instruction::End);
            vi
        },

        Stmt::While(c, b) => {
            let mut vi: Vec<Instruction> = vec![];

            // a br of 1 will be break
            vi.push(Instruction::Block(BlockType::NoResult));

            // a br of 0 will be continue
            vi.push(Instruction::Loop(BlockType::NoResult));

            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, errors);
            vi.append(&mut this_vi);
            
            // if the condition failed, we bail
            // the lack of a br_if_not is irritating. This should be a single
            // instruction really
            vi.push(Instruction::I32Eqz);
            vi.push(Instruction::BrIf(1));

            //run the body
            let mut this_vi: Vec<Instruction> = transform_stmts(b, global_var_map, local_var_map, func_map, errors);
            vi.append(&mut this_vi);

            // jump back to the start of the loop
            vi.push(Instruction::Br(0));
            
            vi.push(Instruction::End);
            vi.push(Instruction::End);
            
            vi
        },

        Stmt::Break => {
            vec![
                Instruction::Br(1)
            ]
        },

        Stmt::Continue => {
            vec![
                Instruction::Br(0)
            ]
        },

        _ => {
            panic!();
        }
    }
}

fn transform_stmts(stmts: &Vec<Stmt>, 
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
    errors: &mut Vec<Error>
)-> Vec<Instruction> {
    let mut vi: Vec<Instruction> = vec![];
    
    for stmt in stmts {
        let mut this_vi = transform_stmt(stmt, global_var_map, local_var_map, func_map, errors);
        vi.append(&mut this_vi);
    }

    vi
}

fn transform_func(func: &Func, 
    start_function: &String,
    global_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
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

    let mut vi: Vec<Instruction> = transform_stmts(&func.body, global_var_map, &func.local_var_map, func_map, errors);
    
    vi.push(Instruction::End);
    let fbb = fbb.with_instructions(Instructions::new(vi));

    let fb = fbb.build();

    let fb = if start_function.eq(&func.name) {
        fb.main()
    } else {
        fb
    };

    fb.build()
}

pub fn transform(program: Program, errors: &mut Vec<Error>) -> Module {
    let mut m = module();
    for func in &program.funcs {
        if !func.import {
            m.push_function(transform_func(func, &program.start, &program.global_var_map, &program.func_map, errors));
        }
        if func.export {
            m = m.export().field(&func.name).internal().func(*(program.func_map.get(&func.name).unwrap())).build();
        }
    }
    m.build()
}

#[cfg(test)]
mod test {
}