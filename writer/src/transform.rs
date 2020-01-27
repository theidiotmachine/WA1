use ast::prelude::*;

use errs::prelude::*;

use parity_wasm::builder::module;
use parity_wasm::elements::{Module, ValueType, Local, Instructions, Instruction, BlockType, GlobalEntry, GlobalType, InitExpr, DataSegment};
use parity_wasm::builder::{FunctionDefinition, FunctionBuilder};

use std::collections::HashMap;
use std::convert::TryInto;

pub use errs::Error;
pub use types::Type;

use crate::wasm_types::{get_ir_value_type, get_wasm_value_type, get_wasm_func_type, get_wasm_return_type};
use crate::mem::*;

use crate::wasm::wasm_module::{WasmModule};
use crate::wasm::wasm_sections::{WasmLimit};
use crate::wasm::{WasmValueType, WasmResultType, serialize_i32, serialize_i64, serialize_f32, serialize_f64, WasmLocals, WasmFunc, WasmExpr};
use crate::wasm::wasm_instructions::{WasmInstr, opcodes};

struct Context{
    pub errors: Vec<Error>,
    pub mem_layout_map: HashMap<String, UserMemLayout>,
    pub data_section: DataSection,
}

struct CompilerContext{
    pub mem_layout_map: HashMap<String, UserMemLayout>,
    pub global_var_map: HashMap<String, u32>,
    pub func_map: HashMap<String, u32>,
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

fn transform_array_member_set(
    inner_value_type: &ValueType,
    inner_value_type_size: u32,
    idx: u32,
    vi: &mut Vec<Instruction>,
) -> () {
    match inner_value_type {
        ValueType::I32 => vi.push(Instruction::I32Store(inner_value_type_size, inner_value_type_size * idx)),
        ValueType::I64 => vi.push(Instruction::I64Store(inner_value_type_size, inner_value_type_size * idx)),
        ValueType::F32 => vi.push(Instruction::F32Store(inner_value_type_size, inner_value_type_size * idx)),
        ValueType::F64 => vi.push(Instruction::F64Store(inner_value_type_size, inner_value_type_size * idx)),
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
                None => { context.errors.push(Error::VariableNotRecognized(l_value.loc.clone(), name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetGlobal(*idx)]
                }
            }
        },

        LValueExpr::LocalVariableAssign(name) => {
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognized(l_value.loc.clone(), name.clone())); vec![] },
                Some(idx) => {
                    vec![Instruction::GetLocal(*idx)]
                }
            }
        },

        LValueExpr::StaticNamedMemberAssign(_, inner_l_value, member_name) => {
            let mut vi: Vec<Instruction> = vec![];
            vi.append(&mut transform_lvalue_get(inner_l_value, global_var_map, local_var_map, context));

            match &inner_l_value.r#type {
                Type::UnsafeStruct{name: type_name} => {
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
                None => { context.errors.push(Error::VariableNotRecognized(l_value.loc.clone(), name.clone())); },
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
                None => { context.errors.push(Error::VariableNotRecognized(l_value.loc.clone(), name.clone())); },
                Some(idx) => {
                    vi.push(Instruction::TeeLocal(*idx))
                }
            };
            vi
        },

        LValueExpr::StaticNamedMemberAssign(_, inner_l_value, member_name) => {
            let mut vi: Vec<Instruction> = vec![];
            vi.append(&mut transform_lvalue_get(inner_l_value, global_var_map, local_var_map, context));
            vi.append(r_value_code);
            match &inner_l_value.r#type {
                Type::UnsafeStruct{name: type_name} => {
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
    loc: &SourceLocation,
    vi: &mut Vec<Instruction>,
    context: &mut Context,
) -> () {
    let o_func_id = func_map.get(name);
    if o_func_id.is_some() {
        vi.push(Instruction::Call(*o_func_id.unwrap()));
    } else {
        context.errors.push(Error::FuncNotRecognized(loc.clone(), name.clone()));
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
                None => { context.errors.push(Error::VariableNotRecognized(typed_expr.loc.clone(), lvu.clone())); },
                Some(idx) => {
                    vi.push(Instruction::GetLocal(*idx));
                }
            }
        },

        Expr::GlobalVariableUse(vu) => {
            let o_idx = global_var_map.get(vu);
            match o_idx {
                None => { context.errors.push(Error::VariableNotRecognized(typed_expr.loc.clone(), vu.clone())); },
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
                                BinaryOperator::NotEqual => vi.push(Instruction::F64Ne),
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
                                BinaryOperator::NotEqual => vi.push(Instruction::I32Ne),
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
                                BinaryOperator::NotEqual => vi.push(Instruction::I64Ne),
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

                Type::UnsafePtr => {
                    match typed_expr.r#type {
                        Type::UnsafePtr => {
                            match bo.op {
                                BinaryOperator::Plus => vi.push(Instruction::I32Add),
                                BinaryOperator::Minus => vi.push(Instruction::I32Sub),
                                BinaryOperator::Multiply => vi.push(Instruction::I32Mul),
                                BinaryOperator::Divide => vi.push(Instruction::I32DivU),
                                BinaryOperator::BitAnd => vi.push(Instruction::I32And),
                                BinaryOperator::BitOr => vi.push(Instruction::I32Or),
                                BinaryOperator::BitXor => vi.push(Instruction::I32Xor),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (__ptr ★ ? => __ptr)")))
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
                                BinaryOperator::NotEqual => vi.push(Instruction::I32Ne),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (__ptr ★ ? => boolean)")))
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
                        BinaryOperator::NotEqual => vi.push(Instruction::I32Ne),
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (boolean ★ ? => boolean)")))
                        }
                    }
                },

                Type::UnsafeSizeT => {
                    match typed_expr.r#type {
                        Type::UnsafeSizeT | Type::UnsafePtr => {
                            match bo.op {
                                BinaryOperator::Plus => vi.push(Instruction::I32Add),
                                BinaryOperator::Minus => vi.push(Instruction::I32Sub),
                                BinaryOperator::Multiply => vi.push(Instruction::I32Mul),
                                BinaryOperator::Divide => vi.push(Instruction::I32DivU),
                                BinaryOperator::BitAnd => vi.push(Instruction::I32And),
                                BinaryOperator::BitOr => vi.push(Instruction::I32Or),
                                BinaryOperator::BitXor => vi.push(Instruction::I32Xor),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (__size_t ★ ? => __size_t | __ptr)")))
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
                                BinaryOperator::NotEqual => vi.push(Instruction::I32Ne),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (__size_t ★ ? => boolean)")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (__size_t ★ ? => ?)")))
                        }
                    }
                },

                Type::Option(_) => {
                    match typed_expr.r#type {
                        Type::Boolean => {
                            match bo.op {
                                BinaryOperator::Equal => vi.push(Instruction::I32Eq),
                                BinaryOperator::NotEqual => vi.push(Instruction::I32Ne),
                                _ => {
                                    context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (Option<T> ★ ? => boolean)")))
                                }
                            }
                        },
                        _ => {
                            context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("binary operator (Option<T> ★ ? => ?)")))
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

                Type::UnsafeSizeT => {
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

        Expr::Assignment(_lhs, l_value, r_value) => {
            let mut r_value_code = transform_typed_expr(&r_value, global_var_map, local_var_map, func_map, context, true);
            vi.append(&mut transform_lvalue_tee(l_value, &mut r_value_code, global_var_map, local_var_map, context));
        },

        Expr::ModifyAssignment(op, _lhs, l_value, r_value) => {
            if *op == AssignmentOperator::Assign {
                
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

                    Type::UnsafePtr | Type::UnsafeSizeT => {
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

            call_func(name, func_map, &typed_expr.loc, &mut vi, context);
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
            match &v.init {
                Some(expr) => {
                    let mut this_vi = transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context, true);
                    vi.append(& mut this_vi);
                    //then set the variable
                    let idx = global_var_map.get(&v.name).unwrap();
                    vi.push(Instruction::SetGlobal(*idx));
                },
                _ => {}
            }
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
                Type::UnsafeStruct{name: type_name} => {
                    transform_struct_member_get(type_name, member_name, &mut vi, context);
                },
                _ => context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("expr{:#?}", typed_expr.expr)))),
            }
        },

        Expr::ConstructFromObjectLiteral(new_type, oles) => {
            match new_type {
                Type::UnsafeStruct{name: struct_name} => {
                    //first get the mem layout data
                    let mem_layout_elem = context.mem_layout_map.get(struct_name).unwrap();
                    let stml = match mem_layout_elem {
                        UserMemLayout::Struct(stml) => {
                            stml
                        },
                    };

                    //push size, call malloc
                    vi.push(Instruction::I32Const(stml.size.try_into().unwrap()));
                    call_func(&String::from("malloc"), func_map, &typed_expr.loc, &mut vi, context);

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

        Expr::ConstructStaticFromObjectLiteral(new_type, oles) => {
            match new_type {
                Type::UnsafeStruct{name: struct_name} => {
                    //first get the mem layout data
                    let mem_layout_elem = context.mem_layout_map.get(struct_name).unwrap();
                    let stml = match mem_layout_elem {
                        UserMemLayout::Struct(stml) => {
                            stml
                        },
                    };

                    let data_section_entry = context.data_section.allocate_new_data_section_entry(stml.size, stml.alignment);
                    let addr = data_section_entry.addr as i32;

                    //now set each member
                    for ole in oles {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        vi.push(Instruction::I32Const(addr));
                        vi.append(&mut transform_typed_expr(&ole.value, global_var_map, local_var_map, func_map, context, true));
                        transform_struct_member_set(&struct_name, &ole.name, &mut vi, context);
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    vi.push(Instruction::I32Const(addr));
                },
                _ => unreachable!(),
            }
        },

        Expr::ConstructStaticFromArrayLiteral(new_type, exprs) => {
            match new_type {
                Type::UnsafeArray(inner_type) => {
                    let elem_value_type = get_ir_value_type(&inner_type);
                    let elem_size = get_size_for_value_type(&elem_value_type);
                    let full_size = elem_size * exprs.len() as u32;

                    let data_section_entry = context.data_section.allocate_new_data_section_entry(full_size, elem_size);
                    let addr = data_section_entry.addr as i32;

                    let mut idx = 0;
                    for expr in exprs {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        vi.push(Instruction::I32Const(addr));
                        vi.append(&mut transform_typed_expr(&expr, global_var_map, local_var_map, func_map, context, true));
                        transform_array_member_set(&elem_value_type, elem_size, idx, &mut vi);
                        idx += 1;
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    vi.push(Instruction::I32Const(addr));
                },
                _ => unreachable!(),
            }
        }

        Expr::Null => {
            vi.push(Instruction::I32Const(0));
        },

        Expr::SizeOf(t) => {
            match t {
                Type::UnsafeStruct{name: struct_name} => {
                    //first get the mem layout data
                    let mem_layout_elem = context.mem_layout_map.get(struct_name).unwrap();
                    let stml = match mem_layout_elem {
                        UserMemLayout::Struct(stml) => {
                            stml
                        },
                    };

                    vi.push(Instruction::I32Const(stml.size.try_into().unwrap()));
                },
                _ => { context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("__sizeof on {}", t)))); }
            }
        },

        _ => { context.errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("{:#?}", typed_expr.expr)))); },
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
    let sb = sb.with_return_type(get_ir_return_type(&func.decl.return_type));
    let mut params: Vec<ValueType> = vec![];
    for arg in &func.decl.args {
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

    let consume_result = func.decl.return_type != Type::RealVoid;
    let mut vi = match &func.body {
        Some(body) => transform_typed_expr(body, global_var_map, &func.local_var_map, func_map, context, consume_result),
        _ => panic!()
    };
        
    vi.push(Instruction::End);
    let fbb = fbb.with_instructions(Instructions::new(vi));

    let fb = fbb.build();

    let fb = if start_function.eq(&func.decl.name) {
        fb.main()
    } else {
        fb
    };

    fb.build()
}

fn compile_func_call(
    name: &String,
    loc: &SourceLocation,
    context: &CompilerContext,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) -> () {
    let o_func_id = context.func_map.get(name);
    if o_func_id.is_some() {
        wasm_expr.data.push(WasmInstr::Call(*o_func_id.unwrap()));
    } else {
        errors.push(Error::FuncNotRecognized(loc.clone(), name.clone()));
    };
}

fn compile_binary_operator(
    bin_op: &BinaryOperator, 
    lhs: &TypedExpr,
    rhs: &TypedExpr,
    return_type: &Type,
    loc: &SourceLocation,
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    compile_expr(&lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
    compile_expr(&rhs, context, local_var_map, true, wasm_module, wasm_expr, errors);

    match lhs.r#type {
        Type::Number => {
            // number * ? => ?
            match return_type {
                Type::Number => {
                    // number * ? => number
                    match bin_op {
                        BinaryOperator::Plus => wasm_expr.data.push(WasmInstr::F64Add),
                        BinaryOperator::Minus => wasm_expr.data.push(WasmInstr::F64Sub),
                        BinaryOperator::Multiply => wasm_expr.data.push(WasmInstr::F64Mul),
                        BinaryOperator::Divide => wasm_expr.data.push(WasmInstr::F64Div),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (number ★ ? => number)")))
                        }
                    }
                },
                Type::Boolean => {
                    // number * ? => boolean
                    match bin_op {
                        BinaryOperator::GreaterThan => wasm_expr.data.push(WasmInstr::F64Gt),
                        BinaryOperator::GreaterThanEqual => wasm_expr.data.push(WasmInstr::F64Ge),
                        BinaryOperator::LessThan=> wasm_expr.data.push(WasmInstr::F64Lt),
                        BinaryOperator::LessThanEqual => wasm_expr.data.push(WasmInstr::F64Le),
                        BinaryOperator::Equal => wasm_expr.data.push(WasmInstr::F64Eq),
                        BinaryOperator::NotEqual => wasm_expr.data.push(WasmInstr::F64Ne),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (number ★ ? => boolean)")))
                        }
                    }
                },        
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (number ★ ? => ?)")))
                }
            }
        },

        Type::Int => {
            // int * ? => ?
            match return_type {
                Type::Int => {
                    // int * ? => int
                    match bin_op {
                        BinaryOperator::Plus => wasm_expr.data.push(WasmInstr::I32Add),
                        BinaryOperator::Minus => wasm_expr.data.push(WasmInstr::I32Sub),
                        BinaryOperator::Multiply => wasm_expr.data.push(WasmInstr::I32Mul),
                        BinaryOperator::Divide => wasm_expr.data.push(WasmInstr::I32DivS),
                        BinaryOperator::BitAnd => wasm_expr.data.push(WasmInstr::I32And),
                        BinaryOperator::BitOr => wasm_expr.data.push(WasmInstr::I32Or),
                        BinaryOperator::BitXor => wasm_expr.data.push(WasmInstr::I32Xor),

                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (int ★ ? => int)")))
                        }
                    }
                },
                Type::Boolean => {
                    // int * ? => boolean
                    match bin_op {
                        BinaryOperator::GreaterThan => wasm_expr.data.push(WasmInstr::I32GtS),
                        BinaryOperator::GreaterThanEqual => wasm_expr.data.push(WasmInstr::I32GeS),
                        BinaryOperator::LessThan=> wasm_expr.data.push(WasmInstr::I32LtS),
                        BinaryOperator::LessThanEqual => wasm_expr.data.push(WasmInstr::I32LeS),
                        BinaryOperator::Equal => wasm_expr.data.push(WasmInstr::I32Eq),
                        BinaryOperator::NotEqual => wasm_expr.data.push(WasmInstr::I32Ne),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (int ★ ? => boolean)")))
                        }
                    }
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (int ★ ? => ?)")))
                }
            }
        },

        Type::BigInt => {
            // bigint * ? => ?
            match return_type {
                Type::BigInt => {
                    // bigint * ? => bigint
                    match bin_op {
                        BinaryOperator::Plus => wasm_expr.data.push(WasmInstr::I64Add),
                        BinaryOperator::Minus => wasm_expr.data.push(WasmInstr::I64Sub),
                        BinaryOperator::Multiply => wasm_expr.data.push(WasmInstr::I64Mul),
                        BinaryOperator::Divide => wasm_expr.data.push(WasmInstr::I64DivS),
                        BinaryOperator::BitAnd => wasm_expr.data.push(WasmInstr::I64And),
                        BinaryOperator::BitOr => wasm_expr.data.push(WasmInstr::I64Or),
                        BinaryOperator::BitXor => wasm_expr.data.push(WasmInstr::I64Xor),

                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (bigint ★ ? => bigint)")))
                        }
                    }
                },
                Type::Boolean => {
                    // bigint * ? => boolean
                    match bin_op {
                        BinaryOperator::GreaterThan => wasm_expr.data.push(WasmInstr::I64GtS),
                        BinaryOperator::GreaterThanEqual => wasm_expr.data.push(WasmInstr::I64GeS),
                        BinaryOperator::LessThan=> wasm_expr.data.push(WasmInstr::I64LtS),
                        BinaryOperator::LessThanEqual => wasm_expr.data.push(WasmInstr::I64LeS),
                        BinaryOperator::Equal => wasm_expr.data.push(WasmInstr::I64Eq),
                        BinaryOperator::NotEqual => wasm_expr.data.push(WasmInstr::I64Ne),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (bigint ★ ? => boolean)")))
                        }
                    }
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (bigint ★ ? => ?)")))
                }
            }
        },

        Type::UnsafePtr => {
            match return_type {
                Type::UnsafePtr => {
                    match bin_op {
                        BinaryOperator::Plus => wasm_expr.data.push(WasmInstr::I32Add),
                        BinaryOperator::Minus => wasm_expr.data.push(WasmInstr::I32Sub),
                        BinaryOperator::Multiply => wasm_expr.data.push(WasmInstr::I32Mul),
                        BinaryOperator::Divide => wasm_expr.data.push(WasmInstr::I32DivU),
                        BinaryOperator::BitAnd => wasm_expr.data.push(WasmInstr::I32And),
                        BinaryOperator::BitOr => wasm_expr.data.push(WasmInstr::I32Or),
                        BinaryOperator::BitXor => wasm_expr.data.push(WasmInstr::I32Xor),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (__ptr ★ ? => __ptr)")))
                        }
                    }
                },
                Type::Boolean => {
                    match bin_op {
                        BinaryOperator::GreaterThan => wasm_expr.data.push(WasmInstr::I32GtU),
                        BinaryOperator::GreaterThanEqual => wasm_expr.data.push(WasmInstr::I32GeU),
                        BinaryOperator::LessThan=> wasm_expr.data.push(WasmInstr::I32LtU),
                        BinaryOperator::LessThanEqual => wasm_expr.data.push(WasmInstr::I32LeU),
                        BinaryOperator::Equal => wasm_expr.data.push(WasmInstr::I32Eq),
                        BinaryOperator::NotEqual => wasm_expr.data.push(WasmInstr::I32Ne),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (__ptr ★ ? => boolean)")))
                        }
                    }
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator")))
                }
            }
        },

        Type::Boolean => {
            // boolean * ? => ?
            match bin_op {
                //these are the bit instructions, but our strong typing should mean that this is safe
                BinaryOperator::LogicalAnd => wasm_expr.data.push(WasmInstr::I32And),
                BinaryOperator::LogicalOr => wasm_expr.data.push(WasmInstr::I32Or),
                BinaryOperator::Equal => wasm_expr.data.push(WasmInstr::I32Eq),
                BinaryOperator::NotEqual => wasm_expr.data.push(WasmInstr::I32Ne),
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (boolean ★ ? => boolean)")))
                }
            }
        },

        Type::UnsafeSizeT => {
            match return_type {
                Type::UnsafeSizeT | Type::UnsafePtr => {
                    match bin_op {
                        BinaryOperator::Plus => wasm_expr.data.push(WasmInstr::I32Add),
                        BinaryOperator::Minus => wasm_expr.data.push(WasmInstr::I32Sub),
                        BinaryOperator::Multiply => wasm_expr.data.push(WasmInstr::I32Mul),
                        BinaryOperator::Divide => wasm_expr.data.push(WasmInstr::I32DivU),
                        BinaryOperator::BitAnd => wasm_expr.data.push(WasmInstr::I32And),
                        BinaryOperator::BitOr => wasm_expr.data.push(WasmInstr::I32Or),
                        BinaryOperator::BitXor => wasm_expr.data.push(WasmInstr::I32Xor),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (__size_t ★ ? => __size_t | __ptr)")))
                        }
                    }
                },
                Type::Boolean => {
                    match bin_op {
                        BinaryOperator::GreaterThan => wasm_expr.data.push(WasmInstr::I32GtU),
                        BinaryOperator::GreaterThanEqual => wasm_expr.data.push(WasmInstr::I32GeU),
                        BinaryOperator::LessThan=> wasm_expr.data.push(WasmInstr::I32LtU),
                        BinaryOperator::LessThanEqual => wasm_expr.data.push(WasmInstr::I32LeU),
                        BinaryOperator::Equal => wasm_expr.data.push(WasmInstr::I32Eq),
                        BinaryOperator::NotEqual => wasm_expr.data.push(WasmInstr::I32Ne),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (__size_t ★ ? => boolean)")))
                        }
                    }
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (__size_t ★ ? => ?)")))
                }
            }
        },

        Type::Option(_) => {
            match return_type {
                Type::Boolean => {
                    match bin_op {
                        BinaryOperator::Equal => wasm_expr.data.push(WasmInstr::I32Eq),
                        BinaryOperator::NotEqual => wasm_expr.data.push(WasmInstr::I32Ne),
                        _ => {
                            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (Option<T> ★ ? => boolean)")))
                        }
                    }
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (Option<T> ★ ? => ?)")))
                }
            }
        },

        _ => {
            errors.push(Error::NotYetImplemented(loc.clone(), String::from("binary operator (? ★ ? => ?)")))
        }
    };
}

fn compile_unary_operator(
    uo: &UnaryOperatorApplication,
    return_type: &Type,
    loc: &SourceLocation,
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    compile_expr(&uo.expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
    
    match return_type {
        Type::Int => {
            match uo.op {
                UnaryOperator::BitNot => {
                    wasm_expr.data.push(WasmInstr::I32Const(-1));
                    wasm_expr.data.push(WasmInstr::I32Xor);
                },

                UnaryOperator::Plus => {},
                UnaryOperator::Minus => {
                    wasm_expr.data.push(WasmInstr::I32Const(-1));
                    wasm_expr.data.push(WasmInstr::I32Mul);
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("unary operator")))
                }
            }
        },

        Type::UnsafeSizeT => {
            match uo.op {
                UnaryOperator::BitNot => {
                    wasm_expr.data.push(WasmInstr::I32Const(-1));
                    wasm_expr.data.push(WasmInstr::I32Xor);
                },

                UnaryOperator::Plus => {},
                UnaryOperator::Minus => {
                    wasm_expr.data.push(WasmInstr::I32Const(-1));
                    wasm_expr.data.push(WasmInstr::I32Mul);
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("unary operator")))
                }
            }
        },

        Type::Number => {
            match uo.op {
                UnaryOperator::Plus => {},
                UnaryOperator::Minus => {
                    wasm_expr.data.push(WasmInstr::F64Neg);
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("unary operator")))
                }
            }
        },

        Type::Boolean => {
            match uo.op {
                UnaryOperator::LogicalNot => {
                    wasm_expr.data.push(WasmInstr::I32Eqz);
                },
                _ => {
                    errors.push(Error::NotYetImplemented(loc.clone(), String::from("unary operator")))
                }
            }
        },

        _ => {
            errors.push(Error::NotYetImplemented(loc.clone(), String::from("unary operator")))
        }
    }
}

fn compile_struct_member_set(
    type_name: &String,
    member_name: &String,
    context: &CompilerContext,
    wasm_expr: &mut WasmExpr,
) -> () {
    let mem_layout = context.mem_layout_map.get(type_name).unwrap();
    match mem_layout {
        UserMemLayout::Struct(struct_mem_layout) => {
            let mem_layout_elem = struct_mem_layout.members.get(member_name).unwrap();
            let align = stupid_log2(mem_layout_elem.align);
            match mem_layout_elem.value_type {
                ValueType::I32 => wasm_expr.data.push(WasmInstr::I32Store(align, mem_layout_elem.offset)),
                ValueType::I64 => wasm_expr.data.push(WasmInstr::I64Store(align, mem_layout_elem.offset)),
                ValueType::F32 => wasm_expr.data.push(WasmInstr::F32Store(align, mem_layout_elem.offset)),
                ValueType::F64 => wasm_expr.data.push(WasmInstr::F64Store(align, mem_layout_elem.offset)),
            }
        },
    }
}

fn compile_struct_member_get(
    type_name: &String,
    member_name: &String,
    context: &CompilerContext,
    wasm_expr: &mut WasmExpr,
) {
    let mem_layout = context.mem_layout_map.get(type_name).unwrap();
    match mem_layout {
        UserMemLayout::Struct(struct_mem_layout) => {
            let mem_layout_elem = struct_mem_layout.members.get(member_name).unwrap();
            let align = stupid_log2(mem_layout_elem.align);
            match mem_layout_elem.value_type {
                ValueType::I32 => wasm_expr.data.push(WasmInstr::I32Load(align, mem_layout_elem.offset)),
                ValueType::I64 => wasm_expr.data.push(WasmInstr::I64Load(align, mem_layout_elem.offset)),
                ValueType::F32 => wasm_expr.data.push(WasmInstr::F32Load(align, mem_layout_elem.offset)),
                ValueType::F64 => wasm_expr.data.push(WasmInstr::F64Load(align, mem_layout_elem.offset)),
            }
        },
    }
}

fn compile_array_member_set(
    inner_value_type: &ValueType,
    inner_value_type_size: u32,
    idx: u32,
    wasm_expr: &mut WasmExpr,
) -> () {
    match inner_value_type {
        ValueType::I32 => wasm_expr.data.push(WasmInstr::I32Store(inner_value_type_size, inner_value_type_size * idx)),
        ValueType::I64 => wasm_expr.data.push(WasmInstr::I64Store(inner_value_type_size, inner_value_type_size * idx)),
        ValueType::F32 => wasm_expr.data.push(WasmInstr::F32Store(inner_value_type_size, inner_value_type_size * idx)),
        ValueType::F64 => wasm_expr.data.push(WasmInstr::F64Store(inner_value_type_size, inner_value_type_size * idx)),
    }
}

/// Because a memory set is (add, val), we need to compute the address first. 
fn compile_l_value_pre(
    l_value: &TypedLValueExpr, 
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    match &l_value.expr {
        LValueExpr::StaticNamedMemberAssign(lhs, _, _) => {
            compile_expr(lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
        },
        _ => {}
    }
}

fn compile_l_value_post(
    lhs: &TypedExpr,
    l_value: &TypedLValueExpr, 
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    consume_result: bool,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    match &l_value.expr {
        LValueExpr::GlobalVariableAssign(name) => {
            let o_idx = context.global_var_map.get(name);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognized(l_value.loc.clone(), name.clone())); },
                Some(idx) => {
                    wasm_expr.data.push(WasmInstr::SetGlobal(*idx));
                    if consume_result {
                        wasm_expr.data.push(WasmInstr::GetGlobal(*idx));
                    }
                }
            };
        },

        LValueExpr::LocalVariableAssign(name) => {
            let o_idx = local_var_map.get(name);
            match o_idx {
                None => { errors.push(Error::VariableNotRecognized(l_value.loc.clone(), name.clone())); },
                Some(idx) => {
                    if consume_result {
                        wasm_expr.data.push(WasmInstr::TeeLocal(*idx));
                    } else {
                        wasm_expr.data.push(WasmInstr::SetLocal(*idx))
                    }
                }
            };
        },

        LValueExpr::StaticNamedMemberAssign(_, inner_l_value, member_name) => {
            match &inner_l_value.r#type {
                Type::UnsafeStruct{name: type_name} => {
                    compile_struct_member_set(type_name, member_name, context, wasm_expr);

                    if consume_result {
                        compile_expr(lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
                    }
                },
                _ => errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from(format!("expr{:#?}", l_value)))),
            }
        },

        _ => { errors.push(Error::NotYetImplemented(l_value.loc.clone(), String::from("lvalue get"))); }
    }
}

fn compile_assign_operator(
    lhs: &TypedExpr,
    l_value: &TypedLValueExpr, 
    r_value: &TypedExpr,
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    consume_result: bool,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    compile_l_value_pre(l_value, context, local_var_map, wasm_module, wasm_expr, errors);
    compile_expr(&r_value, context, local_var_map, true, wasm_module, wasm_expr, errors);
    compile_l_value_post(lhs, l_value, context, local_var_map, consume_result, wasm_module, wasm_expr, errors);
}

fn bin_op_for_ass_op(ass_op: &AssignmentOperator) -> BinaryOperator {
    match ass_op {
        AssignmentOperator::Assign => panic!(),
        AssignmentOperator::BitAndAssign => BinaryOperator::BitAnd,
        AssignmentOperator::BitOrAssign => BinaryOperator::BitOr,
        AssignmentOperator::BitXorAssign => BinaryOperator::BitXor,
        AssignmentOperator::DivideAssign => BinaryOperator::Divide,
        AssignmentOperator::ExponentAssign => BinaryOperator::Exponent,
        AssignmentOperator::MinusAssign => BinaryOperator::Minus,
        AssignmentOperator::ModAssign => BinaryOperator::Mod,
        AssignmentOperator::MultiplyAssign => BinaryOperator::Multiply,
        AssignmentOperator::PlusAssign => BinaryOperator::Plus,
    }
}

fn compile_modify_assign_operator(
    ass_op: &AssignmentOperator,
    lhs: &TypedExpr,
    l_value: &TypedLValueExpr, 
    r_value: &TypedExpr,
    loc: &SourceLocation,
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    consume_result: bool,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    let bin_op = bin_op_for_ass_op(ass_op);
    compile_l_value_pre(l_value, context, local_var_map, wasm_module, wasm_expr, errors);
    compile_binary_operator(&bin_op, lhs, r_value, &lhs.r#type, loc, context, local_var_map, wasm_module, wasm_expr, errors);
    compile_l_value_post(lhs, l_value, context, local_var_map, consume_result, wasm_module, wasm_expr, errors);
}

fn compile_intrinsic(
    i: &Intrinsic,
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    match i {
        Intrinsic::MemorySize => {
            wasm_expr.data.push(WasmInstr::CurrentMemory(0));
        },
        Intrinsic::MemoryGrow(sz_expr) => {
            compile_expr(&sz_expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::GrowMemory(0));
        },
        Intrinsic::Trap => {
            wasm_expr.data.push(WasmInstr::Unreachable)
        },
        Intrinsic::I32Ctz(expr) => {
            compile_expr(&expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::I32Ctz);
        },
        Intrinsic::I64Ctz(expr) => {
            compile_expr(&expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::I64Ctz);
        },
    }
}

fn compile_expr(
    typed_expr: &TypedExpr,
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    consume_result: bool,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) {
    let mut ret_val = true;

    match &typed_expr.expr {
        Expr::FloatLiteral(v) => wasm_expr.data.push(WasmInstr::F64Const(*v)),

        Expr::LocalVariableUse(lvu) => {
            let o_idx = local_var_map.get(lvu);
            match o_idx {
                None => errors.push(Error::VariableNotRecognized(typed_expr.loc.clone(), lvu.clone())),
                Some(idx) => wasm_expr.data.push(WasmInstr::GetLocal(*idx)),
            }
        },

        Expr::GlobalVariableUse(vu) => {
            let o_idx = context.global_var_map.get(vu);
            match o_idx {
                None => errors.push(Error::VariableNotRecognized(typed_expr.loc.clone(), vu.clone())),
                Some(idx) => wasm_expr.data.push(WasmInstr::GetGlobal(*idx)),
            }
        },
        
        Expr::IntLiteral(v) => wasm_expr.data.push(WasmInstr::I32Const(*v)),
        
        Expr::BigIntLiteral(v) => wasm_expr.data.push(WasmInstr::I64Const(*v)),

        Expr::BinaryOperator(bo) => compile_binary_operator(&bo.op, &bo.lhs, &bo.rhs, &typed_expr.r#type, &typed_expr.loc, context, local_var_map, wasm_module, wasm_expr, errors),

        Expr::UnaryOperator(uo) => compile_unary_operator(uo, &typed_expr.r#type, &typed_expr.loc, context, local_var_map, wasm_module, wasm_expr, errors),

        Expr::Parens(p) => compile_expr(&p, context, local_var_map, consume_result, wasm_module, wasm_expr, errors),

        Expr::BoolLiteral(b) => {
            if *b {
                wasm_expr.data.push(WasmInstr::I32Const(1))
            } else {
                wasm_expr.data.push(WasmInstr::I32Const(0))
            };
        },

        Expr::Assignment(lhs, l_value, r_value) => {
            compile_assign_operator(lhs, l_value, r_value, context, local_var_map, consume_result, wasm_module, wasm_expr, errors);
            ret_val = false;
        },
        
        Expr::ModifyAssignment(ass_op, lhs, l_value, r_value) => {
            compile_modify_assign_operator(ass_op, lhs, l_value, r_value, &typed_expr.loc, context, local_var_map, consume_result, wasm_module, wasm_expr, errors);
            ret_val = false;
        },

        Expr::StaticFuncCall(name, args) => {
            for arg in args {
                compile_expr(&arg, context, local_var_map, true, wasm_module, wasm_expr, errors);
            }

            compile_func_call(name, &typed_expr.loc, context, wasm_expr, errors);
        },

        Expr::IntToNumber(p) => {
            compile_expr(&p, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::F64ConvertSI32);
        },

        Expr::IntToBigInt(p) => {
            compile_expr(&p, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::I64ExtendSI32);
        },

        Expr::Return(bo_expr) => {
            match &**bo_expr {
                None => wasm_expr.data.push(WasmInstr::Return),
                Some(expr) => {
                    compile_expr(&expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
                    wasm_expr.data.push(WasmInstr::Return);
                }
            }
        },

        Expr::Break => wasm_expr.data.push(WasmInstr::Br(1)),

        Expr::Continue => wasm_expr.data.push(WasmInstr::Br(0)),

        Expr::IfThen(c, t) => {
            compile_expr(&c, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::If(WasmResultType::Empty));
            compile_expr(&**t, context, local_var_map, false, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::End);
        },

        Expr::IfThenElse(c, t, e) => {
            compile_expr(&c, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::If(get_wasm_return_type(&typed_expr.r#type)));
            compile_expr(&**t, context, local_var_map, consume_result, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::Else);
            compile_expr(&**e, context, local_var_map, consume_result, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::End);
        },

        Expr::While(c, b) => {
            // a br of 1 will be break
            wasm_expr.data.push(WasmInstr::Block(WasmResultType::Empty));

            // a br of 0 will be continue
            wasm_expr.data.push(WasmInstr::Loop(WasmResultType::Empty));

            compile_expr(&c, context, local_var_map, true, wasm_module, wasm_expr, errors);
            
            // if the condition failed, we bail
            // the lack of a br_if_not is irritating. This should be a single
            // instruction really
            wasm_expr.data.push(WasmInstr::I32Eqz);
            wasm_expr.data.push(WasmInstr::BrIf(1));

            //run the body
            compile_expr(&**b, context, local_var_map, false, wasm_module, wasm_expr, errors);
            
            // jump back to the start of the loop
            wasm_expr.data.push(WasmInstr::Br(0));
            
            wasm_expr.data.push(WasmInstr::End);
            wasm_expr.data.push(WasmInstr::End);
        },

        Expr::VariableDecl(v) => {
            //first,  run the init expression
            match &v.init {
                Some(expr) => {
                    compile_expr(&expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
            
                    //then set the variable
                    let idx = local_var_map.get(&v.internal_name).unwrap();
                    wasm_expr.data.push(WasmInstr::SetLocal(*idx));
                },
                _ => {}
            };
        },

        Expr::GlobalVariableDecl(v) => {
            //first,  run the init expression
            match &v.init {
                Some(expr) => {
                    compile_expr(&expr, context, local_var_map, true, wasm_module, wasm_expr, errors);

                    //then set the variable
                    let idx = context.global_var_map.get(&v.name).unwrap();
                    wasm_expr.data.push(WasmInstr::SetGlobal(*idx));
                },
                _ => {}
            }
        },

        Expr::Block(b) => {
            let mut i = b.len();
            for elem in b {
                i -= 1;
                compile_expr(&elem, context, local_var_map, i == 0 && consume_result, wasm_module, wasm_expr, errors);
            }
            //this is a horrible hack to stop a double drop
            ret_val = false;
        },

        Expr::Intrinsic(i) => compile_intrinsic(i, context, local_var_map, wasm_module, wasm_expr, errors),

        Expr::FuncDecl(fd) => {
            if fd.closure.len() > 0 {
                errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from("closure")));
            }
            //kinda wrong for the moment, but
            ret_val = false;
        },

        Expr::FreeTypeWiden(t) => compile_expr(&t, context, local_var_map, consume_result, wasm_module, wasm_expr, errors),

        Expr::StructDecl(_) => {
            //all done at compile time
            ret_val = false;
        },

        Expr::NamedMember(lhs, member_name) => {
            compile_expr(&lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);

            match &lhs.r#type {
                Type::UnsafeStruct{name: type_name} => compile_struct_member_get(type_name, member_name, context, wasm_expr),
                _ => errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("expr{:#?}", typed_expr.expr)))),
            }
        },

        Expr::ConstructFromObjectLiteral(new_type, oles) => {
            match new_type {
                Type::UnsafeStruct{name: struct_name} => {
                    //first get the mem layout data
                    let mem_layout_elem = context.mem_layout_map.get(struct_name).unwrap();
                    let stml = match mem_layout_elem {
                        UserMemLayout::Struct(stml) => {
                            stml
                        },
                    };

                    //push size, call malloc
                    wasm_expr.data.push(WasmInstr::I32Const(stml.size.try_into().unwrap()));
                    compile_func_call(&String::from("malloc"), &typed_expr.loc, context, wasm_expr, errors);

                    //set the local variable called __scratch_malloc (this should have been created previously) and set the malloc size 
                    let scratch_malloc_idx = local_var_map.get("__scratch_malloc").unwrap();
                    wasm_expr.data.push(WasmInstr::SetLocal(*scratch_malloc_idx));
                    
                    //now set each member
                    for ole in oles {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        wasm_expr.data.push(WasmInstr::GetLocal(*scratch_malloc_idx));
                        compile_expr(&ole.value, context, local_var_map, true, wasm_module, wasm_expr, errors);
                        compile_struct_member_set(&struct_name, &ole.name, context, wasm_expr);
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    wasm_expr.data.push(WasmInstr::GetLocal(*scratch_malloc_idx));
                },
                _ => unreachable!(),
            }
        },

        Expr::ConstructStaticFromObjectLiteral(new_type, oles) => {
            match new_type {
                Type::UnsafeStruct{name: struct_name} => {
                    //first get the mem layout data
                    let mem_layout_elem = context.mem_layout_map.get(struct_name).unwrap();
                    let stml = match mem_layout_elem {
                        UserMemLayout::Struct(stml) => {
                            stml
                        },
                    };

                    let data_section_entry = wasm_module.data_section.allocate_new_data_section_entry(stml.size, stml.alignment);
                    let addr = data_section_entry.addr as i32;

                    //now set each member
                    for ole in oles {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        wasm_expr.data.push(WasmInstr::I32Const(addr));
                        compile_expr(&ole.value, context, local_var_map, true, wasm_module, wasm_expr, errors);
                        compile_struct_member_set(&struct_name, &ole.name, context, wasm_expr);
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    wasm_expr.data.push(WasmInstr::I32Const(addr));
                },
                _ => unreachable!(),
            }
        },

        Expr::ConstructStaticFromArrayLiteral(new_type, exprs) => {
            match new_type {
                Type::UnsafeArray(inner_type) => {
                    let elem_value_type = get_ir_value_type(&inner_type);
                    let elem_size = get_size_for_value_type(&elem_value_type);
                    let full_size = elem_size * exprs.len() as u32;

                    let data_section_entry = wasm_module.data_section.allocate_new_data_section_entry(full_size, elem_size);
                    let addr = data_section_entry.addr as i32;

                    let mut idx = 0;
                    for expr in exprs {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        wasm_expr.data.push(WasmInstr::I32Const(addr));
                        compile_expr(&expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
                        compile_array_member_set(&elem_value_type, elem_size, idx, wasm_expr);
                        idx += 1;
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    wasm_expr.data.push(WasmInstr::I32Const(addr));
                },
                _ => unreachable!(),
            }
        }

        Expr::Null => wasm_expr.data.push(WasmInstr::I32Const(0)),

        Expr::SizeOf(t) => {
            match t {
                Type::UnsafeStruct{name: struct_name} => {
                    //first get the mem layout data
                    let mem_layout_elem = context.mem_layout_map.get(struct_name).unwrap();
                    let stml = match mem_layout_elem {
                        UserMemLayout::Struct(stml) => {
                            stml
                        },
                    };

                    wasm_expr.data.push(WasmInstr::I32Const(stml.size.try_into().unwrap()));
                },
                _ => { errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("__sizeof on {}", t)))); }
            }
        },
        
        _ => { errors.push(Error::NotYetImplemented(typed_expr.loc.clone(), String::from(format!("{:#?}", typed_expr.expr)))); },
    }

    if !consume_result && typed_expr.r#type != Type::RealVoid && ret_val{
        wasm_expr.data.push(WasmInstr::Drop);
    }
}

fn compile_func(
    func: &Func,
    start_function: &String,
    context: &CompilerContext,
    m: &mut WasmModule,
    errors: &mut Vec<Error>
) {
    let type_idx = m.type_section.new_func_type(get_wasm_func_type(&func.get_func_type()));
    let wanted_func_idx = *(context.func_map.get(&func.decl.name).unwrap());
    let got_func_idx = m.func_section.func_idx();
    if wanted_func_idx != got_func_idx {
        panic!();
    }
    let func_idx = wanted_func_idx;

    m.func_section.new_func(type_idx);
    if start_function.eq(&func.decl.name) {
        m.start_section.set_start(func_idx);
    }

    let mut locals: Vec<WasmLocals> = vec![];
    let mut o_current_locals: Option<WasmLocals> = None;
    for lv in &func.local_vars {
        if !lv.arg {
            let wasm_type = get_wasm_value_type(&lv.r#type);
            match &mut o_current_locals {
                None => {
                    o_current_locals = Some(WasmLocals::new(&wasm_type));
                }
                Some(current_locals) => {
                    if current_locals.t == wasm_type {
                        current_locals.n += 1;
                    } else {
                        locals.push(current_locals.clone());
                        o_current_locals = Some(WasmLocals::new(&wasm_type));
                    }
                }
            }
        }
    }

    match o_current_locals {
        Some(current_locals) => locals.push(current_locals),
        _ => {}
    }

    let mut expr: WasmExpr = WasmExpr::new();
    match &func.body {
        Some(body) => {
            let consume_result = func.decl.return_type != Type::RealVoid;
            compile_expr(body, context, &func.local_var_map, consume_result, m, &mut expr, errors);
        },
        None => panic!()
    }
    
    m.code_section.new_func(WasmFunc{locals, expr});
}

pub fn compile(program: &Program, errors: &mut Vec<Error>) -> WasmModule {
    let mut m = WasmModule::new();

    for g in &program.globals {
        if !g.import {
            let wt = get_wasm_value_type(&g.r#type);

            let mut init_expr = vec![];
            match wt {
                WasmValueType::F32 => {
                    init_expr.push(opcodes::F32CONST);
                    serialize_f32(0.0, &mut init_expr);
                },
                WasmValueType::F64 => {
                    init_expr.push(opcodes::F64CONST);
                    serialize_f64(0.0, &mut init_expr);
                },
                WasmValueType::I32 => {
                    init_expr.push(opcodes::I32CONST);
                    serialize_i32(0, &mut init_expr);
                },
                WasmValueType::I64 => {
                    init_expr.push(opcodes::I64CONST);
                    serialize_i64(0, &mut init_expr);
                },
            };
            init_expr.push(opcodes::END);
            m.global_section.new_global(&wt, false, &mut init_expr);
        } else {
            let bits: Vec<&str> = g.name.as_str().split(".").collect();
            let module = bits[0];
            let field = bits[1];
            m.import_section.new_global(&module.to_owned(), &field.to_owned(), &get_wasm_value_type(&g.r#type), false);
        }
    }

    let context = CompilerContext{
        mem_layout_map: generate_mem_layout_map(&program.type_map),
        //data_section: DataSection::new(),
        func_map: program.func_map.clone(),
        global_var_map: program.global_var_map.clone(),
    };

    for func in &program.funcs {
        if !func.decl.import {
            compile_func(func, &program.start, &context, &mut m, errors);
        } else {
            let bits: Vec<&str> = func.decl.name.as_str().split(".").collect();
            let module = bits[0];
            let field = bits[1];
            let type_idx = m.type_section.new_func_type(get_wasm_func_type(&func.get_func_type()));
            m.import_section.new_func(&module.to_owned(), &field.to_owned(), type_idx);
        }
        if func.decl.export {
            m.export_section.new_func(&func.decl.name, *(program.func_map.get(&func.decl.name).unwrap()));
        }
    }

    if !m.data_section.is_empty() {
        m.mem_section.new_mem_type(WasmLimit::Min{n: m.data_section.size});
    }

    m
}

pub fn transform(program: &Program, errors: &mut Vec<Error>) -> Module {
    let mut m = module();

    for g in &program.globals {
        if !g.import {
            let vt = get_ir_value_type(&g.r#type);
            let instruction = match vt {
                ValueType::F32 => Instruction::F32Const(0),
                ValueType::F64 => Instruction::F64Const(0),
                ValueType::I32 => Instruction::I32Const(0),
                ValueType::I64 => Instruction::I64Const(0),
            };

            m = m.with_global(GlobalEntry::new(GlobalType::new(vt, true), InitExpr::new(vec![instruction, Instruction::End])));
        } else {
            let bits: Vec<&str> = g.name.as_str().split(".").collect();
            let module = bits[0];
            let field = bits[1];
            m = m.import().field(field).module(module).external().global(get_ir_value_type(&g.r#type), true).build();
        }
    }

    let mut context = Context{
        errors: vec![],
        mem_layout_map: generate_mem_layout_map(&program.type_map),
        data_section: DataSection::new(),
    };

    let mut idx = 0;
    for func in &program.funcs {
        if !func.decl.import {
            m.push_function(transform_func(func, &program.start, &program.global_var_map, &program.func_map, &mut context));
        } else {
            let bits: Vec<&str> = func.decl.name.as_str().split(".").collect();
            let module = bits[0];
            let field = bits[1];
            m = m.import().field(field).module(module).external().func(idx).build();
        }
        if func.decl.export {
            m = m.export().field(&func.decl.name).internal().func(*(program.func_map.get(&func.decl.name).unwrap())).build();
        }
        idx += 1;
    }
    errors.append(& mut context.errors);

    if context.data_section.has_data() {
        m = m.memory().with_min(context.data_section.size).build();
        m = m.with_data_segment(DataSegment::new(0, Some(InitExpr::new(vec![Instruction::I32Const(0), Instruction::End])), context.data_section.generate_data_section()));
    }

    m.build()
}

#[cfg(test)]
mod test {
}