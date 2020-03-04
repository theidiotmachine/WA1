use std::collections::HashMap;

use ast::prelude::*;
pub use errs::Error;

use crate::wasm::wasm_code::WasmExpr;
use crate::wasm::wasm_module::{WasmModule};
use crate::transform::{CompilerContext, compile_expr};
use crate::wasm::wasm_instructions::{WasmInstr};

pub (crate) fn compile_unsafe_size_t_member_func(
    lhs: &TypedExpr, 
    component: &String, 
    args: &Vec<TypedExpr>, 
    context: &CompilerContext,
    local_var_map: &HashMap<String, u32>,
    wasm_module: &mut WasmModule,
    wasm_expr: &mut WasmExpr,
    errors: &mut Vec<Error>
) -> () {
    match component.as_str() {
        "countLeadingZeros" => {
            compile_expr(lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::I32Clz);
        },
        "countTrailingZeros" => {
            compile_expr(lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::I32Ctz);
        },
        "shiftLeft" => {
            compile_expr(lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
            compile_expr(&args[0], context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::I32Shl);
        },
        "shiftRight" => {
            compile_expr(lhs, context, local_var_map, true, wasm_module, wasm_expr, errors);
            compile_expr(&args[0], context, local_var_map, true, wasm_module, wasm_expr, errors);
            wasm_expr.data.push(WasmInstr::I32ShrU);
        },
        _ => unreachable!(),
    }
}