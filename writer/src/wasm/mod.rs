/** This file is a derived work of the parity-wasm project. 
 *
 * `parity-wasm` is primarily distributed under the terms of both the MIT
 * license and the Apache License (Version 2.0), at your choice.
 */
pub mod wasm_instructions;
pub mod wasm_module;
pub mod wasm_sections;

use crate::wasm::wasm_instructions::{WasmInstr, opcodes};

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

#[derive(Debug, Clone, PartialEq)]
pub struct WasmLocals{
    pub n: u32,
    pub t: WasmValueType,
}

impl WasmLocals{
    pub fn new(t: &WasmValueType) -> WasmLocals {
        WasmLocals{n: 1, t: t.clone()}
    }

    pub fn serialize(&self, out: &mut Vec<u8>) {
        serialize_u32(self.n, out);
        self.t.serialize(out);
    }
}

/// corresponds to https://webassembly.github.io/spec/core/binary/instructions.html#binary-expr. As a result, does not have 0x0B at the end.
/// Perhaps this is over-clever.
#[derive(Debug, Clone, PartialEq)]
pub struct WasmExpr{
    pub data: Vec<WasmInstr>,
}

impl WasmExpr{
    pub fn new() -> WasmExpr { WasmExpr{data: vec![] } }

    pub fn push(&mut self, x: WasmInstr) {
        self.data.push(x);
    }

    pub fn serialize(&self, out: &mut Vec<u8>) {
        for i in &self.data {
            i.serialize(out);
        }
        out.push(opcodes::END);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WasmFunc{
    pub locals: Vec<WasmLocals>,
    pub expr: WasmExpr,
}

impl WasmFunc{
    pub fn serialize(&self, out: &mut Vec<u8>) {
        let mut data: Vec<u8> = vec![];
        serialize_u32(self.locals.len() as u32, &mut data);
        for local in &self.locals {
            local.serialize(&mut data);
        }
        self.expr.serialize(&mut data);

        serialize_u32(data.len() as u32, out);
        out.append(&mut data);
    }
}

pub fn serialize_i32(x: i32, data: &mut Vec<u8>) {
    let mut buf = [0u8; 1];
    let mut v = x;
    let mut more = true;
    while more {
        buf[0] = (v & 0b0111_1111) as u8;
        v >>= 7;
        if (v == 0 && buf[0] & 0b0100_0000 == 0) || (v == -1 && buf[0] & 0b0100_0000 == 0b0100_0000)  {
            more = false
        } else {
            buf[0] |= 0b1000_0000
        }

        data.push(buf[0]);
    }
}

pub fn serialize_u32(x: u32, data: &mut Vec<u8>) {
    let mut buf: u8;
    let mut v = x;
    loop {
        buf = (v & 0b0111_1111) as u8;
        v >>= 7;
        if v > 0 {
            buf |= 0b1000_0000;
        }
        data.push(buf);
        if v == 0 { break; }
    }
}

pub fn get_serialized_size_u32(x: u32) -> u32 {
    let mut out: u32 = 0;
    let mut v = x;
    loop {
        v >>= 7;
        out += 1;
        if v == 0 { break; }
    }
    out
}

pub fn serialize_i64(x: i64, data: &mut Vec<u8>) {
    let mut buf = [0u8; 1];
    let mut v = x;
    let mut more = true;
    while more {
        buf[0] = (v & 0b0111_1111) as u8;
        v >>= 7;
        if (v == 0 && buf[0] & 0b0100_0000 == 0) || (v == -1 && buf[0] & 0b0100_0000 == 0b0100_0000)  {
            more = false
        } else {
            buf[0] |= 0b1000_0000
        }

        data.push(buf[0]);
    }
}

pub fn serialize_f32(x: f32, data: &mut Vec<u8>) {
    let bits = x.to_bits();
    let bytes = bits.to_le_bytes();
    for i in 0..4 {
        data.push(bytes[i]);
    }
}

pub fn serialize_f64(x: f64, data: &mut Vec<u8>) {
    let bits = x.to_bits();
    let bytes = bits.to_le_bytes();
    for i in 0..8 {
        data.push(bytes[i]);
    }
}

pub fn serialize_string(x: &String, data: &mut Vec<u8>) {
    serialize_u32(x.len() as u32, data);
    let mut bytes = x.clone().into_bytes();
    data.append(&mut bytes);
}