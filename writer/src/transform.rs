use ast::prelude::*;

use parity_wasm::builder::module;
use parity_wasm::elements::{Module, ValueType, Local, Instructions, Instruction, BlockType};
use parity_wasm::builder::{FunctionDefinition, FunctionBuilder};

use std::collections::HashMap;

pub use errs::Error;
pub use types::Type;

struct Context{
    pub prev_type: Type,
    pub errors: Vec<Error>,
}

fn get_ir_value_type(r#type: &Type) -> ValueType {
    match r#type {
        Type::Number => ValueType::F64,
        Type::Int => ValueType::I32,
        Type::BigInt => ValueType::I64,
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::FakeVoid => ValueType::I32, 
        Type::Boolean => ValueType::I32,
        Type::Ptr(_) => ValueType::I32,
        _ => panic!()
    }
}

fn get_ir_return_type(r#type: &Type) -> Option<ValueType> {
    match r#type {
        Type::RealVoid => None,
        _ => Some(get_ir_value_type(r#type))
    }
}

fn get_ir_block_type(r#type: &Type) -> BlockType {
    match r#type {
        Type::RealVoid => BlockType::NoResult,
        _ => BlockType::Value(get_ir_value_type(r#type))
    }
}

fn transform_lvalue_get(
    l_value: &TypedLValueExpr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    context: &mut Context
) -> Vec<Instruction> {
    match &l_value.expr {
        LValueExpr::GlobalVariableAssign(name) => {
            let o_idx = global_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetGlobal(*idx)]
                }
            }
        },

        LValueExpr::LocalVariableAssign(name) => {
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetLocal(*idx)]
                }
            }
        },
        _ => { context.errors.push(Error::NotYetImplemented(String::from("lvalue get"))); vec![] }
    }
}

fn transform_lvalue_tee(
    l_value: &TypedLValueExpr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    context: &mut Context
) -> Vec<Instruction> {
    match &l_value.expr {
        LValueExpr::GlobalVariableAssign(name) => {
            let o_idx = global_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::SetGlobal(*idx), Instruction::GetGlobal(*idx)]
                }
            }
        },

        LValueExpr::LocalVariableAssign(name) => {
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::TeeLocal(*idx)]
                }
            }
        },
        _ => { context.errors.push(Error::NotYetImplemented(String::from("lvalue get"))); vec![] }
    }
}

fn transform_typed_expr(
    typed_expr: &TypedExpr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
    context: &mut Context,
) -> Vec<Instruction> {
    let mut vi: Vec<Instruction> = vec![];
    
    match &typed_expr.expr {
        Expr::FloatLiteral(v) => {
            vi.push(Instruction::F64Const(v.to_bits()));
        },

        Expr::LocalVariableUse(lvu) => {
            let o_idx = local_var_map.get(lvu);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(lvu.clone())); },
                Some(idx) => {
                    vi.push(Instruction::GetLocal(*idx));
                }
            }
        },

        Expr::GlobalVariableUse(vu) => {
            let o_idx = global_var_map.get(vu);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(vu.clone())); },
                Some(idx) => {
                    vi.push(Instruction::GetGlobal(*idx));
                }
            }
        },
        
        Expr::IntLiteral(v) => {
            vi.push(Instruction::I32Const(*v));
        },
        
        Expr::BigIntLiteral(v) => {
            vi.push(Instruction::I64Const(*v));
        },

        Expr::BinaryOperator(bo) => {
            vi.append(&mut transform_typed_expr(&bo.lhs, global_var_map, local_var_map, func_map, context));
            vi.append(&mut transform_typed_expr(&bo.rhs, global_var_map, local_var_map, func_map, context));

            match bo.lhs.r#type {
                Type::Number => {
                    // number * ? => ?
                    match typed_expr.r#type {
                        Type::Number => {
                            // number * ? => number
                            match bo.op {
                                BinaryOperator::Plus => vi.push(Instruction::F64Add),
                                BinaryOperator::Minus => vi.push(Instruction::F64Sub),
                                BinaryOperator::Multiply => vi.push(Instruction::F64Mul),
                                BinaryOperator::Divide => vi.push(Instruction::F64Div),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },
                        Type::Boolean => {
                            // number * ? => boolean
                            match bo.op {
                                BinaryOperator::GreaterThan => vi.push(Instruction::F64Gt),
                                BinaryOperator::GreaterThanEqual => vi.push(Instruction::F64Ge),
                                BinaryOperator::LessThan=> vi.push(Instruction::F64Lt),
                                BinaryOperator::LessThanEqual => vi.push(Instruction::F64Le),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },        
                        _ => {
                            context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                        }
                    }
                },

                Type::Int => {
                    // int * ? => ?
                    match typed_expr.r#type {
                        Type::Int => {
                            // int * ? => int
                            match bo.op {
                                BinaryOperator::Plus => vi.push(Instruction::I32Add),
                                BinaryOperator::Minus => vi.push(Instruction::I32Sub),
                                BinaryOperator::Multiply => vi.push(Instruction::I32Mul),
                                BinaryOperator::Divide => vi.push(Instruction::I32DivS),
                                BinaryOperator::BitAnd => vi.push(Instruction::I32And),
                                BinaryOperator::BitOr => vi.push(Instruction::I32Or),
                                BinaryOperator::BitXor => vi.push(Instruction::I32Xor),

                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },
                        Type::Boolean => {
                            // int * ? => boolean
                            match bo.op {
                                BinaryOperator::GreaterThan => vi.push(Instruction::I32GtS),
                                BinaryOperator::GreaterThanEqual => vi.push(Instruction::I32GeS),
                                BinaryOperator::LessThan=> vi.push(Instruction::I32LtS),
                                BinaryOperator::LessThanEqual => vi.push(Instruction::I32LeS),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                        }
                    }
                },

                Type::BigInt => {
                    // bigint * ? => ?
                    match typed_expr.r#type {
                        Type::BigInt => {
                            // bigint * ? => bigint
                            match bo.op {
                                BinaryOperator::Plus => vi.push(Instruction::I64Add),
                                BinaryOperator::Minus => vi.push(Instruction::I64Sub),
                                BinaryOperator::Multiply => vi.push(Instruction::I64Mul),
                                BinaryOperator::Divide => vi.push(Instruction::I64DivS),
                                BinaryOperator::BitAnd => vi.push(Instruction::I64And),
                                BinaryOperator::BitOr => vi.push(Instruction::I64Or),
                                BinaryOperator::BitXor => vi.push(Instruction::I64Xor),

                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },
                        Type::Boolean => {
                            // bigint * ? => boolean
                            match bo.op {
                                BinaryOperator::GreaterThan => vi.push(Instruction::I64GtS),
                                BinaryOperator::GreaterThanEqual => vi.push(Instruction::I64GeS),
                                BinaryOperator::LessThan=> vi.push(Instruction::I64LtS),
                                BinaryOperator::LessThanEqual => vi.push(Instruction::I64LeS),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                        }
                    }
                },

                Type::Ptr(_) => {
                    match typed_expr.r#type {
                        Type::Ptr(_) => {
                            match bo.op {
                                BinaryOperator::Plus => vi.push(Instruction::I32Add),
                                BinaryOperator::Minus => vi.push(Instruction::I32Sub),
                                BinaryOperator::Multiply => vi.push(Instruction::I32Mul),
                                BinaryOperator::Divide => vi.push(Instruction::I32DivU),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },
                        Type::Boolean => {
                            match bo.op {
                                BinaryOperator::GreaterThan => vi.push(Instruction::I32GtS),
                                BinaryOperator::GreaterThanEqual => vi.push(Instruction::I32GeS),
                                BinaryOperator::LessThan=> vi.push(Instruction::I32LtS),
                                BinaryOperator::LessThanEqual => vi.push(Instruction::I32LeS),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                        }
                    }
                },

                Type::Boolean => {
                    // boolean * ? => ?
                    match bo.op {
                        //these are the bit instructions, but our strong typing should mean that this is safe
                        BinaryOperator::LogicalAnd => vi.push(Instruction::I32And),
                        BinaryOperator::LogicalOr => vi.push(Instruction::I32Or),
                        _ => {
                            context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                        }
                    }
                },

                _ => {
                    context.errors.push(Error::NotYetImplemented(String::from("binary operator")))
                }
            };
        },

        Expr::UnaryOperator(uo) => {
            vi.append(&mut transform_typed_expr(&uo.expr, global_var_map, local_var_map, func_map, context));
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
                    context.errors.push(Error::NotYetImplemented(String::from("unary operator")))
                }
            };
        }

        Expr::Parens(p) => {
            vi.append(&mut transform_typed_expr(&p, global_var_map, local_var_map, func_map, context))
        },

        Expr::BoolLiteral(b) => {
            if *b {
                vi.push(Instruction::I32Const(1))
            } else {
                vi.push(Instruction::I32Const(0))
            };
        },

        Expr::Assignment(l_value, op, r_value) => {
            if *op == AssignmentOperator::Assign {
                vi.append(&mut transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, context));
                vi.append(&mut transform_lvalue_tee(l_value, global_var_map, local_var_map, context));
            } else {
                vi.append(&mut transform_lvalue_get(l_value, global_var_map, local_var_map, context));
                vi.append(&mut transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, context));
            

                match l_value.r#type {
                    //number *= ?
                    Type::Number => {
                        match op {
                            AssignmentOperator::MinusAssign => vi.push(Instruction::F64Sub),
                            AssignmentOperator::PlusAssign => vi.push(Instruction::F64Add),
                            AssignmentOperator::MultiplyAssign => vi.push(Instruction::F64Mul),
                            AssignmentOperator::DivideAssign => vi.push(Instruction::F64Div),
                            _ => {
                                context.errors.push(Error::NotYetImplemented(String::from("assignment operator")))
                            }
                        }    
                    },

                    Type::Int => {
                        match op {
                            AssignmentOperator::MinusAssign => vi.push(Instruction::I32Sub),
                            AssignmentOperator::PlusAssign => vi.push(Instruction::I32Add),
                            AssignmentOperator::MultiplyAssign => vi.push(Instruction::I32Mul),
                            AssignmentOperator::DivideAssign => vi.push(Instruction::I32DivS),
                            AssignmentOperator::BitAndAssign => vi.push(Instruction::I32And),
                            AssignmentOperator::BitOrAssign => vi.push(Instruction::I32Or),
                            AssignmentOperator::BitXorAssign => vi.push(Instruction::I32Xor),
                    
                            _ => {
                                context.errors.push(Error::NotYetImplemented(String::from("assignment operator")))
                            }
                        }
                    },

                    Type::BigInt => {
                        match op {
                            AssignmentOperator::MinusAssign => vi.push(Instruction::I64Sub),
                            AssignmentOperator::PlusAssign => vi.push(Instruction::I64Add),
                            AssignmentOperator::MultiplyAssign => vi.push(Instruction::I64Mul),
                            AssignmentOperator::DivideAssign => vi.push(Instruction::I64DivS),
                            AssignmentOperator::BitAndAssign => vi.push(Instruction::I64And),
                            AssignmentOperator::BitOrAssign => vi.push(Instruction::I64Or),
                            AssignmentOperator::BitXorAssign => vi.push(Instruction::I64Xor),
                    
                            _ => {
                                context.errors.push(Error::NotYetImplemented(String::from("assignment operator")))
                            }
                        }
                    },

                    _ => {
                        context.errors.push(Error::NotYetImplemented(String::from("assignment operator")))
                    }
                }
                
                vi.append(&mut transform_lvalue_tee(l_value, global_var_map, local_var_map, context));
            }
        },

        Expr::StaticFuncCall(name, args) => {
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
            };
            for arg in args {
                vi.append(&mut transform_typed_expr(&arg, global_var_map, local_var_map, func_map, context));
            }

            let o_func_id = func_map.get(name);
            if o_func_id.is_some() {
                vi.push(Instruction::Call(*o_func_id.unwrap()));
            } else {
                context.errors.push(Error::FuncNotRecognised(name.clone()));
            };
        },

        Expr::IntToNumber(p) => {
            vi.append(&mut transform_typed_expr(&p, global_var_map, local_var_map, func_map, context));
            vi.push(Instruction::F64ConvertSI32);
        },

        Expr::IntToBigInt(p) => {
            vi.append(&mut transform_typed_expr(&p, global_var_map, local_var_map, func_map, context));
            vi.push(Instruction::I64ExtendSI32);
        },

        Expr::Return(bo_expr) => {
            match &**bo_expr {
                None => vi.push(Instruction::Return),
                Some(expr) => {
                    vi.append(&mut transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context));
                    vi.push(Instruction::Return);
                }
            }
        },

        Expr::Break => {
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
            };
            vi.push(Instruction::Br(1))
        },

        Expr::Continue => {
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
            };
            vi.push(Instruction::Br(0))
        },

        Expr::IfThen(c, t) => {
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
            };
            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, context);
            vi.append(&mut this_vi);
            
            vi.push(Instruction::If(BlockType::NoResult));
            context.prev_type = Type::RealVoid;

            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**t, global_var_map, local_var_map, func_map, context);
            vi.append(&mut this_vi);
            vi.push(Instruction::End);
        },

        Expr::IfThenElse(c, t, e) => {
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
            };
            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, context);
            vi.append(&mut this_vi);
            
            vi.push(Instruction::If(get_ir_block_type(&typed_expr.r#type)));
            context.prev_type = Type::RealVoid;

            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**t, global_var_map, local_var_map, func_map, context);
            vi.append(&mut this_vi);
            vi.push(Instruction::Else);
            context.prev_type = Type::RealVoid;

            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**e, global_var_map, local_var_map, func_map, context);
            vi.append(&mut this_vi);            
            vi.push(Instruction::End);
        },

        Expr::While(c, b) => {
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
            };
            // a br of 1 will be break
            vi.push(Instruction::Block(BlockType::NoResult));

            // a br of 0 will be continue
            vi.push(Instruction::Loop(BlockType::NoResult));

            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, context);
            vi.append(&mut this_vi);
            
            // if the condition failed, we bail
            // the lack of a br_if_not is irritating. This should be a single
            // instruction really
            vi.push(Instruction::I32Eqz);
            vi.push(Instruction::BrIf(1));
            context.prev_type = Type::RealVoid;

            //run the body
            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**b, global_var_map, local_var_map, func_map, context);
            vi.append(&mut this_vi);

            //a while loop never returns a value, so we have to drop whatever came out of the last thing.
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
                context.prev_type = Type::RealVoid;
            }

            // jump back to the start of the loop
            vi.push(Instruction::Br(0));
            
            vi.push(Instruction::End);
            vi.push(Instruction::End);
        },

        Expr::VariableDecl(v) => {
            if context.prev_type != Type::RealVoid {
                vi.push(Instruction::Drop);
            };
            //first,  run the init expression
            match &v.init {
                Some(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context);
                    vi.append(& mut this_vi);
                    //then set the variable
                    let o_idx = local_var_map.get(&v.internal_name);
                    match o_idx {
                        Some(idx) => vi.push(Instruction::SetLocal(*idx)),
                        None => context.errors.push(Error::VariableNotRecognised(v.internal_name.clone())),
                    };
                },
                _ => {}
            };
        },

        Expr::Block(b) => {
            for elem in b {
                let mut this_vi = transform_typed_expr(&elem, global_var_map, local_var_map, func_map, context);
                vi.append(& mut this_vi);
            }
        },
        

        _ => { context.errors.push(Error::NotYetImplemented(String::from(format!("expr{:#?}", typed_expr.expr)))); },
    };

    context.prev_type = typed_expr.r#type.clone();

    vi
}

fn transform_exprs(stmts: &Vec<TypedExpr>, 
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
    context: &mut Context
)-> Vec<Instruction> {
    let mut vi: Vec<Instruction> = vec![];
    
    for stmt in stmts {
        let mut this_vi = transform_typed_expr(stmt, global_var_map, local_var_map, func_map, context);
        vi.append(&mut this_vi);
    }

    vi
}

fn transform_func(func: &Func, 
    start_function: &String,
    global_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
    context: &mut Context
) -> FunctionDefinition{
    let fb = FunctionBuilder::new();
    let sb = fb.signature();
    let sb = sb.with_return_type(get_ir_return_type(&func.return_type));
    let mut params: Vec<ValueType> = vec![];
    for arg in &func.args {
        params.push(get_ir_value_type(&arg.r#type));
    }
    let sb = sb.with_params(params);
    let fb = sb.build();

    let mut locals: Vec<Local> = vec![];
    for lv in &func.local_vars {
        if !lv.arg {
            locals.push(Local::new(1, get_ir_value_type(&lv.r#type)));
        }
    }
    let fbb = fb.body();
    let fbb = fbb.with_locals(locals);

    context.prev_type = Type::RealVoid;

    let mut vi: Vec<Instruction> = transform_exprs(&func.body, global_var_map, &func.local_var_map, func_map, context);
    
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
    let mut context = Context{
        errors: vec![],
        prev_type: Type::RealVoid,
    };
    for func in &program.funcs {
        if !func.import {
            m.push_function(transform_func(func, &program.start, &program.global_var_map, &program.func_map, &mut context));
        }
        if func.export {
            m = m.export().field(&func.name).internal().func(*(program.func_map.get(&func.name).unwrap())).build();
        }
    }
    errors.append(& mut context.errors);
    m.build()
}

#[cfg(test)]
mod test {
}