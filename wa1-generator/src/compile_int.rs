use std::collections::HashMap;
use std::{u32};

use wa1_ast::prelude::*;
pub use wa1_errs::Error;
use wa1_types::prelude::*;

use crate::wasm::wasm_code::WasmExpr;
use crate::wasm::wasm_module::{WasmModule};
use crate::transform::{CompilerContext, compile_expr};
use crate::wasm::wasm_instructions::{WasmInstr};

pub (crate) fn compile_int_member_func(
    lhs: &TypedExpr, 
    component: &String, 
    args: &Vec<TypedExpr>, 
    lower: i128, 
    upper: i128,
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) -> () {
    compile_expr(lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
    match component.as_str() {
        "countLeadingZeros" => {
            if upper > U_32_MAX {
                wasm_expr.data.push(WasmInstr::I64Clz);
            } else {
                wasm_expr.data.push(WasmInstr::I32Clz);
            }
        },
        "countTrailingZeros" => {
            if upper > U_32_MAX {
                wasm_expr.data.push(WasmInstr::I64Ctz);
            } else {
                wasm_expr.data.push(WasmInstr::I32Ctz);
            }
        },
        "shiftLeft" => {
            compile_expr(&args[0], context, local_var_map, true, wasm_module, wasm_expr, errors);
            if upper > U_32_MAX {
                wasm_expr.data.push(WasmInstr::I64Shl);
            } else {
                wasm_expr.data.push(WasmInstr::I32Shl);
            }
        },
        "shiftRight" => {
            compile_expr(&args[0], context, local_var_map, true, wasm_module, wasm_expr, errors);
            if upper > U_32_MAX {
                if lower >= 0 {
                    wasm_expr.data.push(WasmInstr::I64ShrU);
                } else {
                    wasm_expr.data.push(WasmInstr::I64ShrS);
                }
            } else {
                if lower >= 0 {
                    wasm_expr.data.push(WasmInstr::I32ShrU);
                } else {
                    wasm_expr.data.push(WasmInstr::I32ShrS);
                }
            }
        },
        "truncateToSigned" => {
            if upper > U_32_MAX {
                wasm_expr.data.push(WasmInstr::I64Const(0x7fff_ffff_ffff_ffff));
                wasm_expr.data.push(WasmInstr::I64And);
            } else {
                wasm_expr.data.push(WasmInstr::I32Const(0x7fff_ffff));
                wasm_expr.data.push(WasmInstr::I32And);
            }
        },

        "unsignedNegate" => {
            if upper > U_32_MAX {
                wasm_expr.data.push(WasmInstr::I64Const(-1));
                wasm_expr.data.push(WasmInstr::I64Mul);
            } else {
                wasm_expr.data.push(WasmInstr::I32Const(-1));
                wasm_expr.data.push(WasmInstr::I32Mul);
            }
        },
        _ => unreachable!(),
    }
}