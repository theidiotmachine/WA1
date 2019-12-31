use ast::prelude::*;

use parity_wasm::builder::module;
use parity_wasm::elements::{Module, ValueType, Local, Instructions, Instruction, BlockType, GlobalEntry, GlobalType, InitExpr};
use parity_wasm::builder::{FunctionDefinition, FunctionBuilder};

use std::collections::HashMap;
use std::convert::TryInto;

pub use errs::Error;
pub use types::Type;

use crate::wasm_types::get_ir_value_type;
use crate::mem::*;

struct Context{
    pub errors: Vec<Error>,
    pub mem_layout_map: HashMap<String, UserMemLayout>,
}

fn stupid_log2(x: u32) -> u32 {
    //I am ashamed to say I have no idea how to do this properly in Rust
    match x {
        16 => 4,
        8 => 3,
        4 => 2,
        2 => 1,
        _ => panic!(),
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

fn transform_struct_member_get(
    type_name: &String,
    member_name: &String,
    vi: &mut Vec<Instruction>,
    context: &mut Context
) -> () {
    let mem_layout = context.mem_layout_map.get(type_name).unwrap();
    match mem_layout {
        UserMemLayout::Struct(struct_mem_layout) => {
            let mem_layout_elem = struct_mem_layout.members.get(member_name).unwrap();
            let align = stupid_log2(mem_layout_elem.align);
            match mem_layout_elem.value_type {
                ValueType::I32 => vi.push(Instruction::I32Load(align, mem_layout_elem.offset)),
                ValueType::I64 => vi.push(Instruction::I64Load(align, mem_layout_elem.offset)),
                ValueType::F32 => vi.push(Instruction::F32Load(align, mem_layout_elem.offset)),
                ValueType::F64 => vi.push(Instruction::F64Load(align, mem_layout_elem.offset)),
            }
        },
    }
}

fn transform_struct_member_set(
    type_name: &String,
    member_name: &String,
    vi: &mut Vec<Instruction>,
    context: &mut Context
) -> () {
    let mem_layout = context.mem_layout_map.get(type_name).unwrap();
    match mem_layout {
        UserMemLayout::Struct(struct_mem_layout) => {
            let mem_layout_elem = struct_mem_layout.members.get(member_name).unwrap();
            let align = stupid_log2(mem_layout_elem.align);
            match mem_layout_elem.value_type {
                ValueType::I32 => vi.push(Instruction::I32Store(align, mem_layout_elem.offset)),
                ValueType::I64 => vi.push(Instruction::I64Store(align, mem_layout_elem.offset)),
                ValueType::F32 => vi.push(Instruction::F32Store(align, mem_layout_elem.offset)),
                ValueType::F64 => vi.push(Instruction::F64Store(align, mem_layout_elem.offset)),
            }
        },
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
                None => { context.errors.push(Error::VariableNotRecognised(l_value.loc.clone(), name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetGlobal(*idx)]
                }
            }
        },

        LValueExpr::LocalVariableAssign(name) => {
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(l_value.loc.clone(), name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetLocal(*idx)]
                }
            }
        },

        LValueExpr::StaticNamedMemberAssign(inner_l_value, member_name) => {
            let mut vi: Vec<Instruction> = vec![];
            vi.append(&mut transform_lvalue_get(inner_l_value, global_var_map, local_var_map, context));

            match &inner_l_value.r#type {
                Type::UserStruct{name: type_name} => {
                    transform_struct_member_get(type_name, member_name, &mut vi, context);
                },
                _ => context.errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from(format!("expr{:#?}", l_value)))),
            }

            vi
        },

        _ => { context.errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from("lvalue get"))); vec![] }
    }
}

fn transform_lvalue_tee(
    l_value: &TypedLValueExpr,
    r_value_code: &mut Vec<Instruction>,
    global_var_map: &HashMap<String, u32>,
    local_var_map: &HashMap<String, u32>,
    context: &mut Context
) -> Vec<Instruction> {
    let mut vi: Vec<Instruction> = vec![];
    match &l_value.expr {
        LValueExpr::GlobalVariableAssign(name) => {
            vi.append(r_value_code);
            let o_idx = global_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(l_value.loc.clone(), name.clone())); },
                Some(idx) => {
                    vi.push(Instruction::SetGlobal(*idx));
                    vi.push(Instruction::GetGlobal(*idx));
                }
            };
            vi
        },

        LValueExpr::LocalVariableAssign(name) => {
            vi.append(r_value_code);
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(l_value.loc.clone(), name.clone())); },
                Some(idx) => {
                    vi.push(Instruction::TeeLocal(*idx))
                }
            };
            vi
        },

        LValueExpr::StaticNamedMemberAssign(inner_l_value, member_name) => {
            let mut vi: Vec<Instruction> = vec![];
            vi.append(&mut transform_lvalue_get(inner_l_value, global_var_map, local_var_map, context));
            vi.append(r_value_code);
            match &inner_l_value.r#type {
                Type::UserStruct{name: type_name} => {
                    transform_struct_member_set(type_name, member_name, &mut vi, context);

                    vi.append(&mut transform_lvalue_get(inner_l_value, global_var_map, local_var_map, context));
                    transform_struct_member_get(type_name, member_name, &mut vi, context);
                },
                _ => context.errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from(format!("expr{:#?}", l_value)))),
            }

            vi
        },

        _ => { context.errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from("lvalue get"))); vec![] }
    }
}

fn call_func(
    name: &String,
    func_map: &HashMap<String, u32>,
    vi: &mut Vec<Instruction>,
    context: &mut Context,
) -> () {
    let o_func_id = func_map.get(name);
    if o_func_id.is_some() {
        vi.push(Instruction::Call(*o_func_id.unwrap()));
    } else {
        context.errors.push(Error::FuncNotRecognised(name.clone()));
    };
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
                None => { context.errors.push(Error::VariableNotRecognised(typed_expr.loc.clone(), lvu.clone())); },
                Some(idx) => {
                    vi.push(Instruction::GetLocal(*idx));
                }
            }
        },

        Expr::GlobalVariableUse(vu) => {
            let o_idx = global_var_map.get(vu);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognised(typed_expr.loc.clone(), vu.clone())); },
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (number ★ ? => number)")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (number ★ ? => boolean)")))
                                }
                            }
                        },        
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (number ★ ? => ?)")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (int ★ ? => int)")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (int ★ ? => boolean)")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (int ★ ? => ?)")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (bigint ★ ? => bigint)")))
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
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (bigint ★ ? => boolean)")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (bigint ★ ? => ?)")))
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
                                BinaryOperator::BitAnd => vi.push(Instruction::I32And),
                                BinaryOperator::BitOr => vi.push(Instruction::I32Or),
                                BinaryOperator::BitXor => vi.push(Instruction::I32Xor),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (ptr ★ ? => ptr)")))
                                }
                            }
                        },
                        Type::Boolean => {
                            match bo.op {
                                BinaryOperator::GreaterThan => vi.push(Instruction::I32GtU),
                                BinaryOperator::GreaterThanEqual => vi.push(Instruction::I32GeU),
                                BinaryOperator::LessThan=> vi.push(Instruction::I32LtU),
                                BinaryOperator::LessThanEqual => vi.push(Instruction::I32LeU),
                                BinaryOperator::Equal => vi.push(Instruction::I32Eq),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (ptr ★ ? => boolean)")))
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
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (boolean ★ ? => boolean)")))
                        }
                    }
                },

                _ => {
                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (? ★ ? => ?)")))
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
                let mut r_value_code = transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, context, true);
                vi.append(&mut transform_lvalue_tee(l_value, &mut r_value_code, global_var_map, local_var_map, context));
            } else {
                let mut r_value_code = transform_lvalue_get(l_value, global_var_map, local_var_map, context);
                r_value_code.append(&mut transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, context, true));
                
                match &l_value.r#type {
                    //number *= ?
                    Type::Number => {
                        match op {
                            AssignmentOperator::MinusAssign => r_value_code.push(Instruction::F64Sub),
                            AssignmentOperator::PlusAssign => r_value_code.push(Instruction::F64Add),
                            AssignmentOperator::MultiplyAssign => r_value_code.push(Instruction::F64Mul),
                            AssignmentOperator::DivideAssign => r_value_code.push(Instruction::F64Div),
                            _ => {
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                            }
                        }    
                    },

                    Type::Int => {
                        match op {
                            AssignmentOperator::MinusAssign => r_value_code.push(Instruction::I32Sub),
                            AssignmentOperator::PlusAssign => r_value_code.push(Instruction::I32Add),
                            AssignmentOperator::MultiplyAssign => r_value_code.push(Instruction::I32Mul),
                            AssignmentOperator::DivideAssign => r_value_code.push(Instruction::I32DivS),
                            AssignmentOperator::BitAndAssign => r_value_code.push(Instruction::I32And),
                            AssignmentOperator::BitOrAssign => r_value_code.push(Instruction::I32Or),
                            AssignmentOperator::BitXorAssign => r_value_code.push(Instruction::I32Xor),
                            _ => {
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                            }
                        }
                    },

                    Type::BigInt => {
                        match op {
                            AssignmentOperator::MinusAssign => r_value_code.push(Instruction::I64Sub),
                            AssignmentOperator::PlusAssign => r_value_code.push(Instruction::I64Add),
                            AssignmentOperator::MultiplyAssign => r_value_code.push(Instruction::I64Mul),
                            AssignmentOperator::DivideAssign => r_value_code.push(Instruction::I64DivS),
                            AssignmentOperator::BitAndAssign => r_value_code.push(Instruction::I64And),
                            AssignmentOperator::BitOrAssign => r_value_code.push(Instruction::I64Or),
                            AssignmentOperator::BitXorAssign => r_value_code.push(Instruction::I64Xor),
                            _ => {
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                            }
                        }
                    },

                    Type::Ptr(_) => {
                        match op {
                            AssignmentOperator::MinusAssign => r_value_code.push(Instruction::I32Sub),
                            AssignmentOperator::PlusAssign => r_value_code.push(Instruction::I32Add),
                            AssignmentOperator::MultiplyAssign => r_value_code.push(Instruction::I32Mul),
                            AssignmentOperator::DivideAssign => r_value_code.push(Instruction::I32DivU),
                            AssignmentOperator::BitAndAssign => r_value_code.push(Instruction::I32And),
                            AssignmentOperator::BitOrAssign => r_value_code.push(Instruction::I32Or),
                            AssignmentOperator::BitXorAssign => r_value_code.push(Instruction::I32Xor),
                            _ => {
                                context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                            }
                        }
                    },

                    _ => {
                        context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("assignment operator")))
                    }
                }
                
                vi.append(&mut transform_lvalue_tee(l_value, &mut r_value_code, global_var_map, local_var_map, context));
            }
        },

        Expr::StaticFuncCall(name, args) => {
            for arg in args {
                vi.append(&mut transform_typed_expr(&arg, global_var_map, local_var_map, func_map, context, true));
            }

            call_func(name, func_map, &mut vi, context);
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
                    let idx = local_var_map.get(&v.internal_name).unwrap();
                    vi.push(Instruction::SetLocal(*idx));
                },
                _ => {}
            };
        },

        Expr::GlobalVariableDecl(v) => {
            //first,  run the init expression
            let mut this_vi = transform_typed_expr(&v.init, global_var_map, local_var_map, func_map, context, true);
            vi.append(& mut this_vi);
            //then set the variable
            let idx = global_var_map.get(&v.name).unwrap();
            vi.push(Instruction::SetGlobal(*idx));
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
                },
                Intrinsic::I32Ctz(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context, true);
                    vi.append(& mut this_vi);
                    vi.push(Instruction::I32Ctz);
                },
                Intrinsic::I64Ctz(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context, true);
                    vi.append(& mut this_vi);
                    vi.push(Instruction::I64Ctz);
                },
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

        Expr::NamedMember(lhs, member_name) => {
            let mut this_vi = transform_typed_expr(&lhs, global_var_map, local_var_map, func_map, context, true);
            vi.append(& mut this_vi);

            match &lhs.r#type {
                Type::UserStruct{name: type_name} => {
                    transform_struct_member_get(type_name, member_name, &mut vi, context);
                },
                _ => context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("expr{:#?}", typed_expr.expr)))),
            }
        },

        Expr::ConstructFromObjectLiteral(new_type, oles) => {
            match new_type {
                Type::UserStruct{name: struct_name} => {
                    //first get the mem layout data
                    let mem_layout_elem = context.mem_layout_map.get(struct_name).unwrap();
                    let stml = match mem_layout_elem {
                        UserMemLayout::Struct(stml) => {
                            stml
                        },
                    };

                    //push size, call malloc
                    vi.push(Instruction::I32Const(stml.size.try_into().unwrap()));
                    call_func(&String::from("malloc"), func_map, &mut vi, context);

                    //set the local variable called __scratch_malloc (this should have been created previously) and set the malloc size 
                    let scratch_malloc_idx = local_var_map.get("__scratch_malloc").unwrap();
                    vi.push(Instruction::SetLocal(*scratch_malloc_idx));
                    
                    //now set each member
                    for ole in oles {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        vi.push(Instruction::GetLocal(*scratch_malloc_idx));
                        vi.append(&mut transform_typed_expr(&ole.value, global_var_map, local_var_map, func_map, context, true));
                        transform_struct_member_set(&struct_name, &ole.name, &mut vi, context);
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    vi.push(Instruction::GetLocal(*scratch_malloc_idx));
                },
                _ => unreachable!(),
            }
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
        mem_layout_map: generate_mem_layout_map(&program.type_map),
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