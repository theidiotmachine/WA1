use std::collections::HashMap;
use std::cmp;

use types::StructType;
use ast::prelude::TypeDecl;

use crate::wasm::{WasmValueType};
use crate::wasm_types::get_wasm_value_type;

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
    pub value_type: WasmValueType,
}

pub enum UserMemLayout{
    Struct(StructMemLayout)
}

pub struct StructMemLayout {
    pub size: u32,
    pub members: HashMap<String, MemLayoutElem>,
    pub alignment: u32,
}

pub fn get_size_for_value_type(value_type: &WasmValueType) -> u32 {
    match value_type {
        WasmValueType::F64 => 8,
        WasmValueType::F32 => 4,
        WasmValueType::I64 => 8,
        WasmValueType::I32 => 4,
    }
}

fn generate_struct_mem_layout(struct_type: &StructType) -> StructMemLayout {
    let mut offset: u32 = 0;
    let mut alignment = 4;
    let mut out_members: HashMap<String, MemLayoutElem> = HashMap::new();
    for mem in &struct_type.members {
        let mem_value_type = get_wasm_value_type(&mem.r#type);
        
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
            TypeDecl::Alias{name: _, of: _, export: _} => {
                //no mem map
            },
            _ => panic!()
        }
    }
    out
}

