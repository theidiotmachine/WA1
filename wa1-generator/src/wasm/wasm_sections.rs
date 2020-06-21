use crate::wasm::{WasmValueType, WasmFuncType};
use crate::wasm::wasm_code::{WasmFunc, WasmExpr};
use crate::wasm::wasm_serialize::{serialize_string, serialize_u32, get_serialized_size_u32, serialize_u32_pad};
use crate::wasm::wasm_instructions::{WasmInstr};
use crate::wasm::wasm_object_file::{WasmObjectModuleFragment};

pub struct WasmTypeSection{
    data: Vec<u8>,
    func_types: Vec<WasmFuncType>,
}

impl WasmTypeSection{
    pub fn new() -> WasmTypeSection {
        WasmTypeSection{ data: vec![], func_types: vec![]} 
    }

    pub fn new_func_type(&mut self, func_type: WasmFuncType) -> u32 {
        let o_pos = self.func_types.iter().position(|x| *x == func_type);
        match o_pos {
            Some(pos) => pos as u32,
            None => {
                let out = self.func_types.len();
                self.func_types.push(func_type.clone());
                func_type.serialize(&mut self.data);
                out as u32
            },
        }
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(1);

        let sz_sz = get_serialized_size_u32(self.func_types.len() as u32);
        serialize_u32(self.data.len() as u32 + sz_sz, out);
        serialize_u32(self.func_types.len() as u32, out);
        out.append(&mut self.data);
    }
}

pub struct WasmImportSection{
    n: u32,
    data: Vec<u8>,
}

impl WasmImportSection{
    pub fn new() -> WasmImportSection {
        WasmImportSection{ n: 0, data: vec![] }
    }

    fn new_import(&mut self, module: &String, name: &String) {
        self.n += 1;
        serialize_string(module, &mut self.data);
        serialize_string(name, &mut self.data);
    }

    pub fn new_global(&mut self, module: &String, name: &String, wasm_type: &WasmValueType, is_constant: bool) {
        self.new_import(module, name);
        self.data.push(0x03);
        wasm_type.serialize(&mut self.data);
        if is_constant {
            self.data.push(0x00);
        } else {
            self.data.push(0x01);
        }
    }

    pub fn new_func(&mut self, module: &String, name: &String, type_idx: u32) {
        self.new_import(module, name);
        self.data.push(0x00);
        serialize_u32(type_idx, &mut self.data);
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(2);

        let sz_sz = get_serialized_size_u32(self.n as u32);
        serialize_u32(self.data.len() as u32 + sz_sz, out);
        
        serialize_u32(self.n, out);
        out.append(&mut self.data);
    }

    pub fn is_empty(&self) -> bool {
        self.n == 0
    }
}

pub struct WasmFuncSection{
    n: u32,
    data: Vec<u8>,
}

impl WasmFuncSection{
    pub fn new() -> WasmFuncSection {
        WasmFuncSection{ n: 0, data: vec![] }
    }

    pub fn new_func(&mut self, type_idx: u32) {
        self.n += 1;
        serialize_u32(type_idx, &mut self.data);
    }

    pub fn func_idx(&self) -> u32 { self.n }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(3);

        let sz_sz = get_serialized_size_u32(self.n as u32);
        serialize_u32(self.data.len() as u32 + sz_sz, out);
        
        serialize_u32(self.n, out);
        out.append(&mut self.data);
    }
}

pub enum WasmLimit{
    Min{n: u32},
    MinMax{n: u32, m: u32}
}

pub struct WasmMemSection{
    pub mt: Vec<WasmLimit>,
    data: Vec<u8>,
}

impl WasmMemSection{
    pub fn new() -> WasmMemSection {
        WasmMemSection{mt: vec![], data: vec![]}
    }

    pub fn new_mem_type(&mut self, l: WasmLimit) {
        match &l {
            WasmLimit::Min{n} => {
                self.data.push(0x00);
                serialize_u32(*n, &mut self.data);
            },
            WasmLimit::MinMax{n, m} => {
                self.data.push(0x01);
                serialize_u32(*n, &mut self.data);
                serialize_u32(*m, &mut self.data);
            }
        }
        self.mt.push(l);
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(5);

        let sz_sz = get_serialized_size_u32(self.mt.len() as u32);
        serialize_u32(self.data.len() as u32 + sz_sz, out);
        
        serialize_u32(self.mt.len() as u32, out);
        out.append(&mut self.data);
    }
}

pub struct WasmGlobalSection{
    pub n: u32,
    pub data: Vec<u8>,
}

impl WasmGlobalSection{
    pub fn new() -> WasmGlobalSection {
        WasmGlobalSection{ n: 0, data: vec![] }
    }

    pub fn new_global(&mut self, wasm_type: &WasmValueType, is_constant: bool, init_expr: &mut Vec<u8>) {
        self.n += 1;
        wasm_type.serialize(&mut self.data);
        self.data.push(if is_constant {0x00} else {0x01});
        self.data.append(init_expr);
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(6);
        
        let sz_sz = get_serialized_size_u32(self.n as u32);
        serialize_u32(self.data.len() as u32 + sz_sz, out);
        
        serialize_u32(self.n, out);
        out.append(&mut self.data);
    }

    pub fn is_empty(&self) -> bool {
        self.n == 0
    }
}

pub struct WasmExportSection{
    n: u32,
    data: Vec<u8>,
}

impl WasmExportSection{
    pub fn new() -> WasmExportSection {
        WasmExportSection{ n: 0, data: vec![] }
    }

    fn new_export(&mut self, name: &String) {
        self.n += 1;
        serialize_string(name, &mut self.data);
    }

    pub fn new_func(&mut self, name: &String, func_idx: u32) {
        self.new_export(name);
        self.data.push(0x00);
        serialize_u32(func_idx, &mut self.data);
    }

    pub fn new_global(&mut self, name: &String, global_idx: u32) {
        self.new_export(name);
        self.data.push(0x03);
        serialize_u32(global_idx, &mut self.data);
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(7);
        
        let sz_sz = get_serialized_size_u32(self.n as u32);
        serialize_u32(self.data.len() as u32 + sz_sz, out);
        
        serialize_u32(self.n, out);
        out.append(&mut self.data);
    }

    pub fn is_empty(&self) -> bool {
        self.n == 0
    }
}

pub struct WasmStartSection{
    data: Vec<u8>,
}

impl WasmStartSection{
    pub fn new() -> WasmStartSection {
        WasmStartSection{ data: vec![] }
    }

    pub fn set_start(&mut self, func_idx: u32) {
        serialize_u32(func_idx, &mut self.data);
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(8);
        serialize_u32(self.data.len() as u32, out);
        out.append(&mut self.data);
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

pub struct WasmCodeSection{
    n: u32,
    funcs: Vec<WasmFunc>,
}

impl WasmCodeSection{
    pub fn new() -> WasmCodeSection {
        WasmCodeSection{ n: 0, funcs: vec![] }
    }

    pub fn new_func(&mut self, func: WasmFunc) {
        self.n += 1;
        self.funcs.push(func);
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(10);
        
        let len_sz = get_serialized_size_u32(self.n as u32);
        let mut final_data: Vec<u8> = vec![];

        for f in &self.funcs {
            f.serialize(&mut final_data);
        }

        serialize_u32(final_data.len() as u32 + len_sz, out);
        serialize_u32(self.n, out);
        
        out.append(&mut final_data);
    }

    pub fn serialize_reloc(&self, reloc: &mut WasmObjectModuleFragment, out: &mut Vec<u8>) {
        out.push(10);
        
        let len_sz = 5;
        let mut final_data: Vec<u8> = vec![];

        for f in &self.funcs {
            let offset = final_data.len() as u32;
            let mut this_reloc_entries = f.serialize_reloc(reloc, offset, &mut final_data);
            reloc.code_reloc_section.entries.append(&mut this_reloc_entries);
        }

        serialize_u32(final_data.len() as u32 + len_sz, out);
        //ugh, if we fix this, it makes the reloc calc much easier. see MAGIC_RELOC_OFFSET
        serialize_u32_pad(self.n, out);
        
        out.append(&mut final_data);
    }
}

#[derive(Debug, Clone)]
pub struct WasmDataSectionEntry{
    //FIXME64BIT
    pub size: u32,
    pub align: u32,
    //FIXME64BIT
    pub addr: u32, 
    pub data: Vec<u8>,
}


pub struct WasmDataSection{
    pub size: u32,
    pub entries: Vec<WasmDataSectionEntry>,
}

impl WasmDataSection{
    pub fn new() -> WasmDataSection {
        WasmDataSection{ size: 8, entries: vec![
            WasmDataSectionEntry{
                size: 8,
                align: 1,
                addr: 1, 
                data: vec![0xba, 0xad, 0xf0, 0x0d, 0xba, 0xad, 0xf0, 0x0d],
            }
        ] }
    }

    //FIXME64BIT
    pub fn allocate_new_data_section_entry(&mut self, size: u32, align: u32) -> &WasmDataSectionEntry {
        let mask = align - 1; 
        let new_size = (self.size + mask) & !(mask);

        let pad = new_size - self.size;
        if pad > 0 {
            let pad_entry = WasmDataSectionEntry{size: pad, align: align, addr: size, data: vec![0; pad as usize]};
            self.size += pad;
            self.entries.push(pad_entry);
        }
        
        let out = WasmDataSectionEntry{size: size, align: align, addr: self.size, data: vec![0; size as usize]};
        self.size += size;
        self.entries.push(out);
        self.entries.last().unwrap()
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        out.push(11);

        //memidx and vec len, 1 byte each, 5 bytes for the data vec len
        let mut section_sz: u32 = 7; 

        let mut e = WasmExpr::new();
        e.push(WasmInstr::I32Const(0));
        let mut expr_data: Vec<u8> = vec![];
        e.serialize(&mut expr_data);

        section_sz += expr_data.len() as u32;
        section_sz += self.size;

        serialize_u32(section_sz, out);
        //1 data element
        out.push(1);

        //from mem index 0
        out.push(0);
        //location expr
        out.append(&mut expr_data);
        //data itself
        serialize_u32_pad(self.size, out);
        for e in &self.entries {
            out.append(& mut e.data.clone());
        }
    }

    pub fn register_data_symbol(&self, module_name: &String, reloc: &mut WasmObjectModuleFragment) {
        reloc.linking_section.symbol_table.new_data(&format!(".data.{}", module_name), 0, 0, self.size);
    }

    pub fn is_empty(&self) -> bool {
        self.entries.len() == 1
    }
}
