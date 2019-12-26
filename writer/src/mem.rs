use std::collections::HashMap;
use std::cmp;

use types::StructType;
use ast::prelude::UserType;

use crate::wasm_types::get_ir_value_type;
use parity_wasm::elements::{ValueType};

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

fn generate_struct_mem_layout(struct_type: &StructType) -> StructMemLayout {
    let mut offset: u32 = 0;
    let mut alignment = 4;
    let mut out_members: HashMap<String, MemLayoutElem> = HashMap::new();
    for mem in &struct_type.members {
        let mem_value_type = get_ir_value_type(&mem.r#type);
        
        let sz = match mem_value_type {
            ValueType::F64 => 8,
            ValueType::F32 => 4,
            ValueType::I64 => 8,
            ValueType::I32 => 4,
        };

        alignment = cmp::max(alignment, sz);

        let mask = sz - 1; 
        offset = (offset + mask) & !(mask);
        out_members.insert(mem.name.clone(), MemLayoutElem{offset, align: sz, value_type: mem_value_type});
        offset += sz;
    }

    StructMemLayout{size: offset, members: out_members, alignment: alignment}
}

pub fn generate_mem_layout_map(type_map: &HashMap<String, UserType>) -> HashMap<String, UserMemLayout>{
    let mut out: HashMap<String, UserMemLayout> = HashMap::new();
    for t in type_map {
        match t.1 {
            UserType::Struct(struct_type) => {
                out.insert(t.0.clone(), UserMemLayout::Struct(generate_struct_mem_layout(&struct_type)));
            },
            _ => panic!()
        }
    }
    out
}