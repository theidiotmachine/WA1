use parity_wasm::elements::{ValueType};

pub use types::Type;

pub(crate) fn get_ir_value_type(r#type: &Type) -> ValueType {
    match r#type {
        Type::Number => ValueType::F64,
        Type::Int => ValueType::I32,
        Type::BigInt => ValueType::I64,
        //if we end up at runtime generating such an argument, just pass an empty int and be done. It's probably a generic.
        Type::FakeVoid => ValueType::I32, 
        Type::Boolean => ValueType::I32,
        //32 bit pointers!
        Type::Ptr(_) => ValueType::I32,
        Type::UserStruct{name: _} => ValueType::I32,
        _ => panic!()
    }
}