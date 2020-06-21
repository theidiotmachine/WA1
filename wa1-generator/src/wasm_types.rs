use std::collections::HashMap;

use wa1_ast::prelude::*;

use crate::wasm::{WasmValueType, WasmFuncType, WasmResultType};

pub use wa1_types::{Type, FuncType, Bittage, get_bittage, PTR_MAX};

pub(crate) fn get_wasm_value_type(
    r#type: &Type,
    type_map: &HashMap<String, TypeDecl>
) -> WasmValueType {
    match r#type {
        Type::Number => WasmValueType::F64,
        Type::Int(lower, upper) => {
            match get_bittage(*lower, *upper) {
                Bittage::S32 | Bittage::U32 => WasmValueType::I32,
                Bittage::S64 | Bittage::U64 => WasmValueType::I64,
                Bittage::OOR => panic!()
            }
        },
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::FakeVoid => WasmValueType::I32, 
        Type::Bool => WasmValueType::I32,
        //FIXME64BIT
        Type::UnsafePtr => WasmValueType::I32,
        Type::UnsafeStruct{name: _} => WasmValueType::I32,
        Type::UnsafeArray(_) => WasmValueType::I32,
        Type::UnsafeOption(inner_type) | Type::UnsafeSome(inner_type) => {
            match **inner_type {
                Type::UnsafeStruct{name: _} => WasmValueType::I32,
                //FIXME64BIT
                Type::UnsafePtr => WasmValueType::I32,
                Type::Never => WasmValueType::I32,
                Type::Int(0, upper) => {
                    if upper == PTR_MAX {
                        WasmValueType::I32    
                    } else {
                        panic!()
                    }
                },
                _ => { panic!() }
            }
        },
        Type::UnsafeNull => {
            WasmValueType::I32
        },
        Type::UserType{name: _, type_args: _, inner} => {
            //let t = type_map.get(name).unwrap();
            //match &t {
                //TypeDecl::Type{name: _, inner, type_args: _, export: _, member_funcs: _, constructor: _, under_construction: _} => {
                    get_wasm_value_type(inner, type_map)
                //},
                //_ => unreachable!()
            //}
        },
        _ => panic!()
    }
}

pub(crate) fn get_wasm_return_type(r#type: &Type) -> WasmResultType {
    match r#type {
        Type::RealVoid => WasmResultType::Empty,
        Type::Number => WasmResultType::F64,
        Type::Int(lower, upper) => {
            match get_bittage(*lower, *upper) {
                Bittage::S32 | Bittage::U32 => WasmResultType::I32,
                Bittage::S64 | Bittage::U64 => WasmResultType::I64,
                Bittage::OOR => panic!()
            }
        },
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::FakeVoid => WasmResultType::I32, 
        Type::Bool => WasmResultType::I32,
        //FIXME64BIT
        Type::UnsafePtr => WasmResultType::I32,
        Type::UnsafeStruct{name: _} => WasmResultType::I32,
        Type::UnsafeArray(_) => WasmResultType::I32,
        Type::UnsafeOption(inner_type) => {
            match **inner_type {
                Type::UnsafeStruct{name: _} => WasmResultType::I32,
                Type::Never => WasmResultType::I32,
                //FIXME64BIT
                Type::UnsafePtr => WasmResultType::I32,
                _ => panic!()                
            }
        },
        _ => panic!()
    }
}

pub (crate) fn get_wasm_func_type(
    func_type: &FuncType,
    type_map: &HashMap<String, TypeDecl>
) -> WasmFuncType {
    let mut inputs: Vec<WasmValueType> = vec![];
    for i in &func_type.in_types {
        inputs.push(get_wasm_value_type(&i.r#type, type_map));
    }
    let mut outputs: Vec<WasmValueType> = vec![];
    if func_type.out_type.r#type != Type::RealVoid {
        outputs.push(get_wasm_value_type(&func_type.out_type.r#type, type_map));
    }
    WasmFuncType{ inputs: inputs, outputs: outputs }
}