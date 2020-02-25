use crate::wasm::{WasmValueType, WasmFuncType, WasmResultType};

pub use types::{Type, FuncType};

pub(crate) fn get_wasm_value_type(r#type: &Type) -> WasmValueType {
    match r#type {
        Type::Number => WasmValueType::F64,
        Type::Int => WasmValueType::I32,
        Type::BigInt => WasmValueType::I64,
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::FakeVoid => WasmValueType::I32, 
        Type::Boolean => WasmValueType::I32,
        //FIXME64BIT
        Type::UnsafePtr => WasmValueType::I32,
        Type::UnsafeSizeT => WasmValueType::I32,
        Type::UnsafeStruct{name: _} => WasmValueType::I32,
        Type::UnsafeArray(_) => WasmValueType::I32,
        Type::UnsafeOption(inner_type) => {
            match **inner_type {
                Type::UnsafeStruct{name: _} => WasmValueType::I32,
                //FIXME64BIT
                Type::UnsafePtr => WasmValueType::I32,
                Type::Never => WasmValueType::I32,
                _ => panic!()                
            }
        },
        _ => panic!()
    }
}

pub(crate) fn get_wasm_return_type(r#type: &Type) -> WasmResultType {
    match r#type {
        Type::RealVoid => WasmResultType::Empty,
        Type::Number => WasmResultType::F64,
        Type::Int => WasmResultType::I32,
        Type::BigInt => WasmResultType::I64,
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::FakeVoid => WasmResultType::I32, 
        Type::Boolean => WasmResultType::I32,
        //FIXME64BIT
        Type::UnsafePtr => WasmResultType::I32,
        Type::UnsafeSizeT => WasmResultType::I32,
        Type::UnsafeStruct{name: _} => WasmResultType::I32,
        Type::UnsafeArray(_) => WasmResultType::I32,
        Type::Option(inner_type) => {
            match **inner_type {
                Type::UnsafeStruct{name: _} => WasmResultType::I32,
                Type::Never => WasmResultType::I32,
                _ => panic!()                
            }
        },
        _ => panic!()
    }
}

pub (crate) fn get_wasm_func_type(func_type: &FuncType) -> WasmFuncType {
    let mut inputs: Vec<WasmValueType> = vec![];
    for i in &func_type.in_types {
        inputs.push(get_wasm_value_type(i));
    }
    let mut outputs: Vec<WasmValueType> = vec![];
    if func_type.out_type != Type::RealVoid {
        outputs.push(get_wasm_value_type(&func_type.out_type));
    }
    WasmFuncType{ inputs: inputs, outputs: outputs }
}