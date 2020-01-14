use std::collections::HashMap;
use std::cmp;

use types::StructType;
use ast::prelude::TypeDecl;

use crate::wasm_types::get_ir_value_type;
use parity_wasm::elements::{ValueType};

pub fn stupid_log2(x: u32) -> u32 {
    //I am ashamed to say I have no idea how to do this properly in Rust
    match x {
        16 => 4,
        8 => 3,
        4 => 2,
        2 => 1,
        _ => panic!(),
    }
}

pub struct MemLayoutElem{
    pub align: u32,
    pub offset: u32,
    pub value_type: ValueType,
}

pub enum UserMemLayout{
    Struct(StructMemLayout)
}

pub struct StructMemLayout {
    pub size: u32,
    pub members: HashMap<String, MemLayoutElem>,
    pub alignment: u32,
}

pub fn get_size_for_value_type(value_type: &ValueType) -> u32 {
    match value_type {
        ValueType::F64 => 8,
        ValueType::F32 => 4,
        ValueType::I64 => 8,
        ValueType::I32 => 4,
    }
}

fn generate_struct_mem_layout(struct_type: &StructType) -> StructMemLayout {
    let mut offset: u32 = 0;
    let mut alignment = 4;
    let mut out_members: HashMap<String, MemLayoutElem> = HashMap::new();
    for mem in &struct_type.members {
        let mem_value_type = get_ir_value_type(&mem.r#type);
        
        let sz = get_size_for_value_type(&mem_value_type);

        alignment = cmp::max(alignment, sz);

        let mask = sz - 1; 
        offset = (offset + mask) & !(mask);
        out_members.insert(mem.name.clone(), MemLayoutElem{offset, align: sz, value_type: mem_value_type});
        offset += sz;
    }

    StructMemLayout{size: offset, members: out_members, alignment: alignment}
}

pub fn generate_mem_layout_map(type_map: &HashMap<String, TypeDecl>) -> HashMap<String, UserMemLayout>{
    let mut out: HashMap<String, UserMemLayout> = HashMap::new();
    for t in type_map {
        match t.1 {
            TypeDecl::Struct{name: _, struct_type, under_construction: _, export: _} => {
                out.insert(t.0.clone(), UserMemLayout::Struct(generate_struct_mem_layout(&struct_type)));
            },
            _ => panic!()
        }
    }
    out
}

#[derive(Debug, Clone)]
pub struct DataSectionEntry{
    //FIXME64BIT
    pub size: u32,
    pub align: u32,
    //FIXME64BIT
    pub addr: u32, 
    pub data: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct DataSection{
    //FIXME64BIT
    pub size: u32,
    pub entries: Vec<DataSectionEntry>,
}

impl DataSection {
    //FIXME64BIT
    pub fn allocate_new_data_section_entry(&mut self, size: u32, align: u32) -> &DataSectionEntry {
        let mask = align - 1; 
        let new_size = (self.size + mask) & !(mask);

        let pad = new_size - self.size;
        if pad > 0 {
            let pad_entry = DataSectionEntry{size: pad, align: align, addr: size, data: vec![0; pad as usize]};
            self.size += pad;
            self.entries.push(pad_entry);
        }
        
        let out = DataSectionEntry{size: size, align: align, addr: self.size, data: vec![0; size as usize]};
        self.size += size;
        self.entries.push(out);
        self.entries.last().unwrap()
    }

    pub fn generate_data_section(&self) -> Vec<u8> {
        let mut out = Vec::new();
        for e in &self.entries {
            out.append(& mut e.data.clone());
        }

        out
    }

    pub fn new() -> DataSection {
        DataSection{size: 8, entries: vec![
            DataSectionEntry{
                size: 8,
                align: 1,
                addr: 1, 
                data: vec![0xba, 0xad, 0xf0, 0x0d, 0xba, 0xad, 0xf0, 0x0d ],
            }
        ]}
    }

    pub fn has_data(&self) -> bool {
        self.entries.len() > 1
    }
}

