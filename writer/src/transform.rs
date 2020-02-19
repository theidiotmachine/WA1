use ast::prelude::*;

use errs::prelude::*;

use parity_wasm::elements::{Instructions, Instruction};

use std::collections::HashMap;
use std::convert::TryInto;

pub use errs::Error;
pub use types::Type;

use crate::wasm_types::{get_wasm_value_type, get_wasm_func_type, get_wasm_return_type};
use crate::mem::*;

use crate::wasm::wasm_module::{WasmModule};
use crate::wasm::wasm_sections::{WasmLimit};
use crate::wasm::{WasmValueType, WasmResultType};
use crate::wasm::wasm_code::{WasmLocals, WasmFunc, WasmExpr};
use crate::wasm::wasm_instructions::{WasmInstr, opcodes};
use crate::wasm::wasm_serialize::{serialize_i32, serialize_i64, serialize_f32, serialize_f64};

#[derive(PartialEq, Copy, Clone)]
pub enum TranslationUnitType{
    Simple,
    LinkedSourceFile,
    LinkedEntryPoint
}

#[derive(PartialEq, Copy, Clone)]
pub enum OutputType{
    Standalone,
    StaticLibrary
}

struct CompilerContext{
    pub mem_layout_map: HashMap<String, UserMemLayout>,
    pub global_var_map: HashMap<String, u32>,
    pub func_map: HashMap<String, u32>,
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
                WasmValueType::I32 => wasm_expr.data.push(WasmInstr::I32Store(align, mem_layout_elem.offset)),
                WasmValueType::I64 => wasm_expr.data.push(WasmInstr::I64Store(align, mem_layout_elem.offset)),
                WasmValueType::F32 => wasm_expr.data.push(WasmInstr::F32Store(align, mem_layout_elem.offset)),
                WasmValueType::F64 => wasm_expr.data.push(WasmInstr::F64Store(align, mem_layout_elem.offset)),
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
                WasmValueType::I32 => wasm_expr.data.push(WasmInstr::I32Load(align, mem_layout_elem.offset)),
                WasmValueType::I64 => wasm_expr.data.push(WasmInstr::I64Load(align, mem_layout_elem.offset)),
                WasmValueType::F32 => wasm_expr.data.push(WasmInstr::F32Load(align, mem_layout_elem.offset)),
                WasmValueType::F64 => wasm_expr.data.push(WasmInstr::F64Load(align, mem_layout_elem.offset)),
            }
        },
    }
}

fn compile_array_member_set(
    inner_value_type: &WasmValueType,
    inner_value_type_size: u32,
    idx: u32,
    wasm_expr: &mut WasmExpr,
) -> () {
    match inner_value_type {
        WasmValueType::I32 => wasm_expr.data.push(WasmInstr::I32Store(inner_value_type_size, inner_value_type_size * idx)),
        WasmValueType::I64 => wasm_expr.data.push(WasmInstr::I64Store(inner_value_type_size, inner_value_type_size * idx)),
        WasmValueType::F32 => wasm_expr.data.push(WasmInstr::F32Store(inner_value_type_size, inner_value_type_size * idx)),
        WasmValueType::F64 => wasm_expr.data.push(WasmInstr::F64Store(inner_value_type_size, inner_value_type_size * idx)),
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
                    let addr = data_section_entry.addr;

                    //now set each member
                    for ole in oles {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        wasm_expr.data.push(WasmInstr::U32ConstStaticMemAddr(addr));
                        compile_expr(&ole.value, context, local_var_map, true, wasm_module, wasm_expr, errors);
                        compile_struct_member_set(&struct_name, &ole.name, context, wasm_expr);
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    wasm_expr.data.push(WasmInstr::U32ConstStaticMemAddr(addr));
                },
                _ => unreachable!(),
            }
        },

        Expr::ConstructStaticFromArrayLiteral(new_type, exprs) => {
            match new_type {
                Type::UnsafeArray(inner_type) => {
                    let elem_value_type = get_wasm_value_type(&inner_type);
                    let elem_size = get_size_for_value_type(&elem_value_type);
                    let full_size = elem_size * exprs.len() as u32;

                    let data_section_entry = wasm_module.data_section.allocate_new_data_section_entry(full_size, elem_size);
                    let addr = data_section_entry.addr;

                    let mut idx = 0;
                    for expr in exprs {
                        //sets are, irritatingly, the address, the value, and then the instruction
                        wasm_expr.data.push(WasmInstr::U32ConstStaticMemAddr(addr));
                        compile_expr(&expr, context, local_var_map, true, wasm_module, wasm_expr, errors);
                        compile_array_member_set(&elem_value_type, elem_size, idx, wasm_expr);
                        idx += 1;
                    }

                    //now leave the return value, which is the pointer to the newly created thing
                    wasm_expr.data.push(WasmInstr::U32ConstStaticMemAddr(addr));
                },
                _ => unreachable!(),
            }
        }

        Expr::Null => wasm_expr.data.push(WasmInstr::I32Const(0)),
        Expr::UnsafeNull => wasm_expr.data.push(WasmInstr::I32Const(0)),

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

fn register_func(
    func_decl: &FuncDecl,
    tu_type: TranslationUnitType,
    output_type: OutputType,
    module_name: &String,
    start_function: &String,
    func_idx: u32,
    m: &mut WasmModule,
) {
    let type_idx = m.type_section.new_func_type(get_wasm_func_type(&func_decl.get_func_type()));

    m.func_section.new_func(type_idx);
    //Only register a start function if we are not linking
    //if start_function.eq(&func_decl.name) && output_type == OutputType::Standalone {
        //m.start_section.set_start(func_idx);
    //}

    if func_decl.export {
        if tu_type == TranslationUnitType::LinkedEntryPoint || tu_type == TranslationUnitType::LinkedSourceFile {
            m.export_section.new_func(&format!("{}.{}", module_name, func_decl.name), func_idx);
        } else {
            m.export_section.new_func(&func_decl.name, func_idx);
        }
        match &mut m.object_file_sections {
            Some(wrf) => {
                if tu_type == TranslationUnitType::LinkedSourceFile {
                    wrf.linking_section.symbol_table.new_local_exported_function(func_idx, &format!("{}.{}", module_name, func_decl.name));
                } else { //RelocationMode::StaticEntryPoint
                    wrf.linking_section.symbol_table.new_full_exported_function(func_idx, &format!("{}.{}", module_name, func_decl.name));
                }
            },
            _ => {}
        }
    } else {
        match &mut m.object_file_sections {
            Some(wrf) => {
                if start_function.eq(&func_decl.name) {
                    wrf.linking_section.symbol_table.new_start_function(func_idx, &func_decl.name);
                    if output_type == OutputType::Standalone {
                        wrf.linking_section.init_funcs.new_init_func(wrf.linking_section.symbol_table.funcs[func_idx as usize]);
                    }
                } else {
                    wrf.linking_section.symbol_table.new_local_function(func_idx, &func_decl.name);
                }
            },
            _ => {}
        }
    }
}

fn compile_func(
    func: &Func,
    context: &CompilerContext,
    m: &mut WasmModule,
    errors: &mut Vec<Error>
) {
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

/// Compile an AST to a Wasm Module IR type.
pub fn compile(
    ast: &AST, 
    tu_type: TranslationUnitType, 
    output_type: OutputType,
    module_name: &String, 
    errors: &mut Vec<Error>
) -> WasmModule {
    let mut m = WasmModule::new(tu_type);
    let mut global_var_map: HashMap<String, u32> = HashMap::new();
    let mut global_idx: u32 = 0;

    for g in &ast.global_imports {
        let bits: Vec<&str> = g.name.as_str().split(".").collect();
        let module = bits[0];
        let field = bits[1];
        m.import_section.new_global(&module.to_owned(), &field.to_owned(), &get_wasm_value_type(&g.r#type), false);

        match &mut m.object_file_sections {
            Some(wrf) => {
                if g.export {
                    wrf.linking_section.symbol_table.new_imported_exported_global(global_idx);
                } else {
                    wrf.linking_section.symbol_table.new_imported_global(global_idx);
                }
            },
            _ => {}
        }
        global_var_map.insert(g.name.clone(), global_idx);
        global_idx += 1;
    }

    for g in &ast.global_decls {
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
        if g.export {
            m.export_section.new_global(&g.name, global_idx);
        }
        match &mut m.object_file_sections {
            Some(wrf) => {
                if g.export {
                    if tu_type == TranslationUnitType::LinkedSourceFile {
                        wrf.linking_section.symbol_table.new_local_exported_global(global_idx, &format!("{}.{}", module_name, g.name));
                    } else {
                        wrf.linking_section.symbol_table.new_full_exported_global(global_idx, &format!("{}.{}", module_name, g.name));
                    }
                } else {
                    wrf.linking_section.symbol_table.new_local_global(global_idx, &g.name);
                }
            },
            _ => {}
        }

        global_var_map.insert(g.name.clone(), global_idx);
        global_idx += 1;
    }

    let mut func_map: HashMap<String, u32> = HashMap::new();
    let mut func_idx: u32 = 0;

    for func in &ast.func_imports {
        let bits: Vec<&str> = func.name.as_str().split(".").collect();
        let module = bits[0];
        let field = bits[1];
        let type_idx = m.type_section.new_func_type(get_wasm_func_type(&func.get_func_type()));
        if tu_type == TranslationUnitType::Simple {
            m.import_section.new_func(&module.to_owned(), &field.to_owned(), type_idx);
        } else {
            m.import_section.new_func(&module.to_owned(), &func.name, type_idx);
        }
        
        if func.export {
            m.export_section.new_func(&func.name, func_idx);

            match &mut m.object_file_sections {
                Some(wrf) => {
                    wrf.linking_section.symbol_table.new_imported_exported_function(func_idx);
                },
                _ => {}
            }
        } else {
            match &mut m.object_file_sections {
                Some(wrf) => {
                    wrf.linking_section.symbol_table.new_imported_function(func_idx);
                },
                _ => {}
            }
        }

        func_map.insert(func.name.clone(), func_idx);
        func_idx += 1;
    }

    for func in &ast.func_decls {
        register_func(&func.decl, tu_type, output_type, module_name, &ast.start, func_idx, &mut m);

        func_map.insert(func.decl.name.clone(), func_idx);
        func_idx += 1;
    }

    let context = CompilerContext{
        mem_layout_map: generate_mem_layout_map(&ast.type_map),
        func_map: func_map.clone(),
        global_var_map: global_var_map.clone(),
    };

    for func in &ast.func_decls {
        compile_func(func, &context, &mut m, errors);
    }

    if !m.data_section.is_empty() {
        m.mem_section.new_mem_type(WasmLimit::Min{n: m.data_section.size});
    }

    m
}


#[cfg(test)]
mod test {
}