use ast::prelude::*;

use parity_wasm::builder::module;
use parity_wasm::elements::{Module, ValueType, Local, Instructions, Instruction, BlockType, GlobalEntry, GlobalType, InitExpr};
use parity_wasm::builder::{FunctionDefinition, FunctionBuilder};

use std::collections::HashMap;

pub use errs::Error;
pub use types::Type;

struct Context{
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
        _ => { context.errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from("lvalue get"))); vec![] }
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
        _ => { context.errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from("lvalue get"))); vec![] }
    }
}

fn transform_typed_expr(
    typed_expr: &TypedExpr,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    func_map: &HashMap<String, u32>,
    context: &mut Context,
    consume_result: bool,
) -> Vec<Instruction> {
    let mut vi: Vec<Instruction> = vec![];
    let mut nop = false;
    
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
            vi.append(&mut transform_typed_expr(&bo.lhs, global_var_map, local_var_map, func_map, context, true));
            vi.append(&mut transform_typed_expr(&bo.rhs, global_var_map, local_var_map, func_map, context, true));

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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
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
                                BinaryOperator::Equal => vi.push(Instruction::F64Eq),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
                                }
                            }
                        },        
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
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
                                BinaryOperator::Equal => vi.push(Instruction::I32Eq),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
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
                                BinaryOperator::Equal => vi.push(Instruction::I64Eq),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
                        }
                    }
                },

                Type::Boolean => {
                    // boolean * ? => ?
                    match bo.op {
                        //these are the bit instructions, but our strong typing should mean that this is safe
                        BinaryOperator::LogicalAnd => vi.push(Instruction::I32And),
                        BinaryOperator::LogicalOr => vi.push(Instruction::I32Or),
                        BinaryOperator::Equal => vi.push(Instruction::I32Eq),
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
                        }
                    }
                },

                _ => {
                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator")))
                }
            };
        },

        Expr::UnaryOperator(uo) => {
            vi.append(&mut transform_typed_expr(&uo.expr, global_var_map, local_var_map, func_map, context, true));

            match uo.expr.r#type {
                Type::Int => {
                    match uo.op {
                        UnaryOperator::BitNot => {
                            vi.push(Instruction::I32Const(-1));
                            vi.push(Instruction::I32Xor);
                        },
        
                        UnaryOperator::Plus => {},
                        UnaryOperator::Minus => {
                            vi.push(Instruction::I32Const(-1));
                            vi.push(Instruction::I32Mul);
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("unary operator")))
                        }
                    }
                },

                Type::Number => {
                    match uo.op {
                        UnaryOperator::Plus => {},
                        UnaryOperator::Minus => {
                            let v: f64 = -1.0;
                            vi.push(Instruction::F64Const(v.to_bits()));
                            vi.push(Instruction::F64Mul);
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("unary operator")))
                        }
                    }
                },

                Type::Boolean => {
                    match uo.op {
                        UnaryOperator::LogicalNot => {
                            vi.push(Instruction::I32Eqz);
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("unary operator")))
                        }
                    }
                },

                _ => {
                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("unary operator")))
                }
            }
        }

        Expr::Parens(p) => {
            vi.append(&mut transform_typed_expr(&p, global_var_map, local_var_map, func_map, context, consume_result))
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
                vi.append(&mut transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, context, true));
                vi.append(&mut transform_lvalue_tee(l_value, global_var_map, local_var_map, context));
            } else {
                vi.append(&mut transform_lvalue_get(l_value, global_var_map, local_var_map, context));
                vi.append(&mut transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, context, true));

                match &l_value.r#type {
                    //number *= ?
                    Type::Number => {
                        match op {
                            AssignmentOperator::MinusAssign => vi.push(Instruction::F64Sub),
                            AssignmentOperator::PlusAssign => vi.push(Instruction::F64Add),
                            AssignmentOperator::MultiplyAssign => vi.push(Instruction::F64Mul),
                            AssignmentOperator::DivideAssign => vi.push(Instruction::F64Div),
                            _ => {
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
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
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
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
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                            }
                        }
                    },

                    Type::Ptr(_) => {
                        match op {
                            AssignmentOperator::MinusAssign => vi.push(Instruction::I32Sub),
                            AssignmentOperator::PlusAssign => vi.push(Instruction::I32Add),
                            AssignmentOperator::MultiplyAssign => vi.push(Instruction::I32Mul),
                            AssignmentOperator::DivideAssign => vi.push(Instruction::I32DivU),
                            AssignmentOperator::BitAndAssign => vi.push(Instruction::I32And),
                            AssignmentOperator::BitOrAssign => vi.push(Instruction::I32Or),
                            AssignmentOperator::BitXorAssign => vi.push(Instruction::I32Xor),
                            _ => {
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                            }
                        }
                    },

                    _ => {
                        context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                    }
                }
                
                vi.append(&mut transform_lvalue_tee(l_value, global_var_map, local_var_map, context));
            }
        },

        Expr::StaticFuncCall(name, args) => {
            for arg in args {
                vi.append(&mut transform_typed_expr(&arg, global_var_map, local_var_map, func_map, context, true));
            }

            let o_func_id = func_map.get(name);
            if o_func_id.is_some() {
                vi.push(Instruction::Call(*o_func_id.unwrap()));
            } else {
                context.errors.push(Error::FuncNotRecognised(name.clone()));
            };
        },

        Expr::IntToNumber(p) => {
            vi.append(&mut transform_typed_expr(&p, global_var_map, local_var_map, func_map, context, true));
            vi.push(Instruction::F64ConvertSI32);
        },

        Expr::IntToBigInt(p) => {
            vi.append(&mut transform_typed_expr(&p, global_var_map, local_var_map, func_map, context, true));
            vi.push(Instruction::I64ExtendSI32);
        },

        Expr::Return(bo_expr) => {
            match &**bo_expr {
                None => vi.push(Instruction::Return),
                Some(expr) => {
                    vi.append(&mut transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context, true));
                    vi.push(Instruction::Return);
                }
            }
        },

        Expr::Break => {
            vi.push(Instruction::Br(1))
        },

        Expr::Continue => {
            vi.push(Instruction::Br(0))
        },

        Expr::IfThen(c, t) => {
            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, context, true);
            vi.append(&mut this_vi);
            
            vi.push(Instruction::If(BlockType::NoResult));

            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**t, global_var_map, local_var_map, func_map, context, false);
            vi.append(&mut this_vi);

            vi.push(Instruction::End);
        },

        Expr::IfThenElse(c, t, e) => {
            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, context, true);
            vi.append(&mut this_vi);
            
            vi.push(Instruction::If(get_ir_block_type(&typed_expr.r#type)));

            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**t, global_var_map, local_var_map, func_map, context, consume_result);
            vi.append(&mut this_vi);
            vi.push(Instruction::Else);

            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**e, global_var_map, local_var_map, func_map, context, consume_result);
            vi.append(&mut this_vi);            
            vi.push(Instruction::End);
        },

        Expr::While(c, b) => {
            // a br of 1 will be break
            vi.push(Instruction::Block(BlockType::NoResult));

            // a br of 0 will be continue
            vi.push(Instruction::Loop(BlockType::NoResult));

            let mut this_vi = transform_typed_expr(&c, global_var_map, local_var_map, func_map, context, true);
            vi.append(&mut this_vi);
            
            // if the condition failed, we bail
            // the lack of a br_if_not is irritating. This should be a single
            // instruction really
            vi.push(Instruction::I32Eqz);
            vi.push(Instruction::BrIf(1));

            //run the body
            let mut this_vi: Vec<Instruction> = transform_typed_expr(&**b, global_var_map, local_var_map, func_map, context, false);
            vi.append(&mut this_vi);

            // jump back to the start of the loop
            vi.push(Instruction::Br(0));
            
            vi.push(Instruction::End);
            vi.push(Instruction::End);
        },

        Expr::VariableDecl(v) => {
            //first,  run the init expression
            match &v.init {
                Some(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context, true);
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

        Expr::GlobalVariableDecl(v) => {
            //first,  run the init expression
            match &v.init {
                Some(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context, true);
                    vi.append(& mut this_vi);
                    //then set the variable
                    let o_idx = global_var_map.get(&v.name);
                    match o_idx {
                        Some(idx) => vi.push(Instruction::SetGlobal(*idx)),
                        None => context.errors.push(Error::VariableNotRecognised(v.name.clone())),
                    };
                },
                _ => {}
            };
        },

        Expr::Block(b) => {
            let mut i = b.len();
            for elem in b {
                i -= 1;
                let mut this_vi = transform_typed_expr(&elem, global_var_map, local_var_map, func_map, context, i == 0 && consume_result);
                vi.append(& mut this_vi);
            }
            //this is a horrible hack to stop a double drop
            nop = true;
        },

        Expr::Intrinsic(i) => {
            match i {
                Intrinsic::MemorySize => {
                    vi.push(Instruction::CurrentMemory(0));
                },
                Intrinsic::MemoryGrow(sz_expr) => {
                    let mut this_vi = transform_typed_expr(&sz_expr, global_var_map, local_var_map, func_map, context, true);
                    vi.append(& mut this_vi);
                    vi.push(Instruction::GrowMemory(0));
                },
                Intrinsic::Trap => {
                    vi.push(Instruction::Unreachable)
                }
            }  
        },

        Expr::FuncDecl(fd) => {
            if fd.closure.len() > 0 {
                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("closure")));
            }
            //kinda wrong for the moment, but
            nop = true;
        },

        Expr::FreeTypeWiden(t) => {
            let mut this_vi = transform_typed_expr(&t, global_var_map, local_var_map, func_map, context, true);
            vi.append(& mut this_vi);
        },

        Expr::StructDecl(_) => {
            //all done at compile time
            nop = true;
        },

        _ => { context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("expr{:#?}", typed_expr.expr)))); },
    };

    if !consume_result && typed_expr.r#type != Type::RealVoid && !nop{
        vi.push(Instruction::Drop);
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

    let consume_result = func.return_type != Type::RealVoid;
    let mut vi = transform_typed_expr(&func.body, global_var_map, &func.local_var_map, func_map, context, consume_result);
        
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

    m = m.memory().with_min(1).build();

    for g in program.globals {
        let vt = get_ir_value_type(&g.r#type);
        let instruction = match vt {
            ValueType::F32 => Instruction::F32Const(0),
            ValueType::F64 => Instruction::F64Const(0),
            ValueType::I32 => Instruction::I32Const(0),
            ValueType::I64 => Instruction::I64Const(0),
        };

        m = m.with_global(GlobalEntry::new(GlobalType::new(vt, true), InitExpr::new(vec![instruction, Instruction::End])));
    }

    let mut context = Context{
        errors: vec![],
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