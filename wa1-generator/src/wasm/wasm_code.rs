use crate::wasm::{WasmValueType};
use crate::wasm::wasm_serialize::{serialize_u32, get_serialized_size_u32};
use crate::wasm::wasm_instructions::{WasmInstr, opcodes};
use crate::wasm::wasm_object_file::{WasmRelocationEntry, WasmObjectModuleFragment};

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

    pub fn serialize_reloc(&self, reloc: &WasmObjectModuleFragment, reloc_entries: &mut Vec<WasmRelocationEntry>, out: &mut Vec<u8>) {
        for i in &self.data {
            i.serialize_reloc(reloc, reloc_entries, out);
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
    fn serialize_locals(&self, data: &mut Vec<u8>) {
        serialize_u32(self.locals.len() as u32, data);
        for local in &self.locals {
            local.serialize(data);
        }
    }
    pub fn serialize(&self, out: &mut Vec<u8>) {
        let mut data: Vec<u8> = vec![];
        self.serialize_locals(&mut data);
        self.expr.serialize(&mut data);

        serialize_u32(data.len() as u32, out);
        out.append(&mut data);
    }

    pub fn serialize_reloc(&self, reloc: &WasmObjectModuleFragment, base_offset: u32, out: &mut Vec<u8>) -> Vec<WasmRelocationEntry>{
        let mut data: Vec<u8> = vec![];
        self.serialize_locals(&mut data);

        let mut reloc_entries: Vec<WasmRelocationEntry> = vec![];
        self.expr.serialize_reloc(reloc, &mut reloc_entries, &mut data);

        // we need to shift all the reloc entries by the size of the function. Not brilliant.
        // at that point, may as well roll the base offset in, too
        let len_sz = get_serialized_size_u32(data.len() as u32);
        for reloc_entry in &mut reloc_entries {
            reloc_entry.offset += len_sz + base_offset;
        }

        serialize_u32(data.len() as u32, out);
        out.append(&mut data);

        reloc_entries
    }
}


