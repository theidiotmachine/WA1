
pub mod wasm_instructions;
pub mod wasm_module;
pub mod wasm_sections;
pub mod wasm_object_file;
pub mod wasm_serialize;
pub mod wasm_code;

use crate::wasm::wasm_serialize::{serialize_u32};

#[derive(Debug, Clone, PartialEq)]
pub enum WasmValueType{
    I32,
    I64,
    F32,
    F64,
}

impl WasmValueType{
    fn serialize(&self, data: &mut Vec<u8>) {
        match self {
            WasmValueType::I32 => data.push(0x7F),
            WasmValueType::I64 => data.push(0x7E),
            WasmValueType::F32 => data.push(0x7D),
            WasmValueType::F64 => data.push(0x7C),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WasmResultType{
    I32,
    I64,
    F32,
    F64,
    Empty,
}

impl WasmResultType{
    pub fn serialize(&self, data: &mut Vec<u8>) {
        match self {
            WasmResultType::I32 => data.push(0x7F),
            WasmResultType::I64 => data.push(0x7E),
            WasmResultType::F32 => data.push(0x7D),
            WasmResultType::F64 => data.push(0x7C),
            WasmResultType::Empty => data.push(0x40),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WasmFuncType{
    pub inputs: Vec<WasmValueType>,
    pub outputs: Vec<WasmValueType>,
}

impl WasmFuncType{
    fn serialize(&self, data: &mut Vec<u8>) {
        data.push(0x60);
        let inputs_len = self.inputs.len();
        let outputs_len = self.outputs.len();
                
        serialize_u32(inputs_len as u32, data);
        for input in &self.inputs {
            input.serialize(data);
        }
        serialize_u32(outputs_len as u32, data);
        for output in &self.outputs {
            output.serialize(data);
        }
    }
}

