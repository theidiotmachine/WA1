use crate::wasm::wasm_serialize::{serialize_string, serialize_u32, serialize_u32_pad};

/// This is an implementation of https://github.com/WebAssembly/tool-conventions/blob/master/Linking.md
/// 
/// It contains copies of fragments of the code and comments in that file.

#[derive(Debug, Clone, Copy)]
enum WasmSymInfoKind{
    ///SYMTAB_FUNCTION
    SymTabFunction = 0,
    ///SYMTAB_DATA
    SymTabData = 1,
    ///SYMTAB_GLOBAL
    SymTabGlobal = 2,
    ///SYMTAB_SECTION
    //SymTabSection = 3,
    ///SYMTAB_EVENT
    SymTabEvent = 4,
    ///SYMTAB_TABLE
    SymTabTable = 5,
}

/// Indicating that this is a weak symbol. When linking multiple modules defining the same symbol, all weak definitions are 
/// discarded if any strong definitions exist; then if multiple weak definitions exist all but one (unspecified) are 
/// discarded; and finally it is an error if more than one definition remains.
const WASM_SYM_BINDING_WEAK: u32 = 0x01;
/// Indicating that this is a local symbol (this is exclusive with WASM_SYM_BINDING_WEAK). Local symbols are not to be 
/// exported, or linked to other modules/sections. The names of all non-local symbols must be unique, but the names of local 
/// symbols are not considered for uniqueness. A local function or global symbol cannot reference an import.
const WASM_SYM_BINDING_LOCAL: u32 = 0x02;
/// Indicating that this is a hidden symbol. Hidden symbols are not to be exported when performing the final link, but may 
/// be linked to other modules.
const WASM_SYM_VISIBILITY_HIDDEN: u32 = 0x04;
/// Indicating that this symbol is not defined. For non-data symbols, this must match whether the symbol is an import or is 
/// defined; for data symbols, determines whether a segment is specified.
const WASM_SYM_UNDEFINED: u32 = 0x10;
/// The symbol is intended to be exported from the wasm module to the host environment. This differs from the visibility 
/// flags in that it effects the static linker.
const WASM_SYM_EXPORTED: u32 = 0x20;
/// The symbol uses an explicit symbol name, rather than reusing the name from a wasm import. This allows it to remap imports 
/// from foreign WebAssembly modules into local symbols with different names.
const WASM_SYM_EXPLICIT_NAME: u32 = 0x40;
/// The symbol is intended to be included in the linker output, regardless of whether it is used by the program.
//const WASM_SYM_NO_STRIP: u32 = 0x80;

/// Represents a syminfo structure,
pub struct WasmSymInfo{
    kind: WasmSymInfoKind,

    /// Whether this symbol references an import or a locally declared symbol
    pub is_import: bool,

    /// A field of the WASM_SYM_ flags
    pub flags: u32,
    
    /// Used only if kind == SYMTAB_DATA; or if a symbols refers to an import, and the WASM_SYM_EXPLICIT_NAME flag is not 
    /// set, then the name is taken from the import; otherwise this specifies the symbol's name.
    pub name: Option<String>,

    /// The index of the Wasm object corresponding to the symbol, which references an import if and only if the WASM_SYM_UNDEFINED flag is set
    pub index: u32,

    /// Used only if kind == SYMTAB_DATA. The index of the data segment; provided if the symbol is defined
    pub data_segment_index: u32,
    /// Used only if kind == SYMTAB_DATA. The offset within the segment; provided if the symbol is defined; must be <= the segment's size
    pub data_offset: u32,
    /// Used only if kind == SYMTAB_DATA. The size (which can be zero); provided if the symbol is defined; offset + size must be <= the segment's size
    pub data_size: u32,

    ///For section symbols: the index of the target section
    pub section: u32
}

impl WasmSymInfo {
    pub fn serialize(&self, out: &mut Vec<u8>){
        out.push(self.kind as u8);
        serialize_u32(self.flags, out);

        match &self.kind{
            WasmSymInfoKind::SymTabData => {
                match &self.name {
                    Some(name) => serialize_string(&name, out),
                    _ => {}
                }
                if self.flags & WASM_SYM_UNDEFINED == 0 {
                    serialize_u32(self.data_segment_index, out);
                    serialize_u32(self.data_offset, out);
                    serialize_u32(self.data_size, out);
                }
            },
            /*
            WasmSymInfoKind::SymTabSection => {
                serialize_u32(self.section, out);
            },
            */
            WasmSymInfoKind::SymTabFunction | WasmSymInfoKind::SymTabGlobal | WasmSymInfoKind::SymTabEvent | WasmSymInfoKind::SymTabTable => {
                serialize_u32(self.index, out);
                if self.is_import { 
                    if (self.flags & WASM_SYM_EXPLICIT_NAME) == WASM_SYM_EXPLICIT_NAME {
                        match &self.name {
                            Some(name) => serialize_string(&name, out),
                            _ => {panic!();}
                        }
                    }
                } else {
                    match &self.name {
                        Some(name) => serialize_string(&name, out),
                        _ => {
                            panic!();
                        }
                    }
                }
            }
        }
    }
}

/// Extra metadata about the data segments.
//const WASM_SEGMENT_INFO: u8 = 5;
/// Specifies a list of constructor functions to be called at startup. These constructors will be called in priority order after memory has been initialized.
const WASM_INIT_FUNCS: u8 = 6; 
/// Specifies the COMDAT groups of associated linking objects, which are linked only once and all together.
//const WASM_COMDAT_INFO: u8 = 7; 
/// Specifies extra information about the symbols present in the module.
const WASM_SYMBOL_TABLE: u8 = 8; 

pub struct WasmInitFuncs{
    pub functions: Vec<u32>,
}

impl WasmInitFuncs{
    pub fn serialize(&self, out: &mut Vec<u8>){
        out.push(WASM_INIT_FUNCS);
        let mut final_data: Vec<u8> = vec![];

        let mut i = 0;
        serialize_u32(self.functions.len() as u32, &mut final_data);
        for f in &self.functions {
            //This is the priority. We simply run them in the order provided.
            serialize_u32(i, &mut final_data);
            serialize_u32(*f, &mut final_data);
            i += 1;
        }

        serialize_u32(final_data.len() as u32, out);
        out.append(&mut final_data);
    }

    pub fn new_init_func(&mut self, func_sym_idx: u32) {
        self.functions.push(func_sym_idx);
    }
}

pub struct WasmSymbolTable{
    /// Sequence of syminfo
    pub infos: Vec<WasmSymInfo>,

    /// Map of global index to sym index
    pub globals: Vec<u32>,
    /// Map of func index to sym index
    pub funcs: Vec<u32>,
    /// Sym indexes of data. Will probably be of length 1
    pub data: Vec<u32>,
}

impl WasmSymbolTable{

    fn new_func_symbol_index(&mut self, index: u32) -> u32 {
        let out = self.infos.len() as u32;
        if self.funcs.len() <= index as usize {
            self.funcs.resize((index + 1) as usize, 0);
        }
        self.funcs[index as usize] = out;
        out
    }

    fn new_global_symbol_index(&mut self, index: u32) -> u32 {
        let out = self.infos.len() as u32;
        if self.globals.len() <= index as usize {
            self.globals.resize((index + 1) as usize, 0);
        }
        self.globals[index as usize] = out;
        out
    }

    fn new_data_symbol_index(&mut self, index: u32) -> u32 {
        let out = self.infos.len() as u32;
        if self.data.len() <= index as usize {
            self.data.resize((index + 1) as usize, 0);
        }
        self.data[index as usize] = out;
        out
    }

    /// A function that is exported to another module, but not out of wasm
    pub fn new_local_exported_function(&mut self, index: u32, name: &String) {
        self.new_func_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabFunction, is_import: false, flags: 0, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A function that is exported out of wasm
    pub fn new_full_exported_function(&mut self, index: u32, name: &String) {
        self.new_func_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabFunction, is_import: false, flags: WASM_SYM_EXPORTED, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A function that is not exported 
    pub fn new_local_function(&mut self, index: u32, name: &String) {
        self.new_func_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabFunction, is_import: false, flags: WASM_SYM_BINDING_LOCAL, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A function that is not exported 
    pub fn new_weak_function(&mut self, index: u32, name: &String) {
        self.new_func_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabFunction, is_import: false, flags: WASM_SYM_BINDING_WEAK, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// The start function
    pub fn new_start_function(&mut self, index: u32, name: &String) {
        self.new_func_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabFunction, is_import: false, flags: WASM_SYM_VISIBILITY_HIDDEN, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// An imported function
    pub fn new_imported_function(&mut self, index: u32) {
        self.new_func_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabFunction, is_import: true, flags: WASM_SYM_UNDEFINED
             , name: None, 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A function imported and then exported. Only exists in entry point files
    pub fn new_imported_exported_function(&mut self, index: u32) {
        self.new_func_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabFunction, is_import: true, flags: WASM_SYM_EXPORTED + WASM_SYM_UNDEFINED, name: None, 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A global that is exported to another module, but not out of wasm
    pub fn new_local_exported_global(&mut self, index: u32, name: &String) {
        self.new_global_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabGlobal, is_import: false, flags: 0, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A global that is exported out of wasm
    pub fn new_full_exported_global(&mut self, index: u32, name: &String) {
        self.new_global_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabGlobal, is_import: false, flags: WASM_SYM_EXPORTED, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A global that is not exported 
    pub fn new_local_global(&mut self, index: u32, name: &String) {
        self.new_global_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabGlobal, is_import: false, flags: WASM_SYM_BINDING_LOCAL, name: Some(name.clone()), 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// An imported global
    pub fn new_imported_global(&mut self, index: u32) {
        self.new_global_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabGlobal, is_import: true, flags: WASM_SYM_UNDEFINED, name: None, 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    /// A global imported and then exported. Only exists in entry point files
    pub fn new_imported_exported_global(&mut self, index: u32) {
        self.new_global_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabGlobal, is_import: true, flags: WASM_SYM_EXPORTED + WASM_SYM_UNDEFINED, name: None, 
            index, data_segment_index: 0, data_offset: 0, data_size: 0, section: 0
        })
    }

    pub fn new_data(&mut self, name: &String, index: u32, offset: u32, size: u32) {
        self.new_data_symbol_index(index);
        self.infos.push(WasmSymInfo{
            kind: WasmSymInfoKind::SymTabData, is_import: false, flags: WASM_SYM_EXPORTED, name: Some(name.clone()), 
            index: 0, data_segment_index: index, data_offset: offset, data_size: size, section: 0
        })
    }

    pub fn serialize(&self, out: &mut Vec<u8>){
        out.push(WASM_SYMBOL_TABLE);
        let mut final_data: Vec<u8> = vec![];
        serialize_u32(self.infos.len() as u32, &mut final_data);
        for syminfo in &self.infos {
            syminfo.serialize(&mut final_data);
        }
        
        serialize_u32(final_data.len() as u32, out);
        out.append(&mut final_data);
    }

    pub fn is_empty(&self) -> bool {
        self.infos.len() == 0
    }
}

/// The linking section has one of each subsection in it. This is a simplified implementation of the standard which allows many of each.
pub struct WasmLinkingSection{
    pub init_funcs: WasmInitFuncs,
    pub symbol_table: WasmSymbolTable,
}

impl WasmLinkingSection{
    pub fn serialize(&self, out: &mut Vec<u8>){
        out.push(0);

        let mut final_data: Vec<u8> = vec![];
        serialize_string(&String::from("linking"), &mut final_data);

        //version number
        serialize_u32(2, &mut final_data);
        //These must be in this order: the init funcs references the symbols
        self.symbol_table.serialize(&mut final_data);
        self.init_funcs.serialize(&mut final_data);
        
        serialize_u32(final_data.len() as u32, out);
        out.append(&mut final_data);        
    }

    pub fn is_empty(&self) -> bool {
        self.symbol_table.is_empty()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WasmRelocationType{
    ///R_WASM_FUNCTION_INDEX_LEB.
    ///A function index encoded as a 5-byte varuint32. Used for the immediate argument of a call instruction.
    RWasmFunctionIndexLEB = 0,
    ///R_WASM_TABLE_INDEX_SLEB.
    ///A function table index encoded as a 5-byte varint32. Used to refer to the immediate argument of a i32.const instruction, e.g. taking the address of a function.
    RWasmTableIndexSLEB = 1,
    ///R_WASM_TABLE_INDEX_I32.
    /// A function table index encoded as a uint32, e.g. taking the address of a function in a static data initializer.
    RWasmTableIndexI32 = 2,
    /// R_WASM_MEMORY_ADDR_LEB.
    /// A linear memory index encoded as a 5-byte varuint32. Used for the immediate argument of a load or store instruction, e.g. directly loading from or storing to a C++ global.
    RWasmMemoryAddrLEB = 3,
    ///R_WASM_MEMORY_ADDR_SLEB.
    /// A linear memory index encoded as a 5-byte varint32. Used for the immediate argument of a i32.const instruction, e.g. taking the address of a C++ global.
    RWasmMemoryAddrSLEB = 4,
    ///R_WASM_MEMORY_ADDR_I32.
    /// A linear memory index encoded as a uint32, e.g. taking the address of a C++ global in a static data initializer.
    RWasmMemoryAddrI32 = 5,
    ///R_WASM_TYPE_INDEX_LEB.
    /// A type table index encoded as a 5-byte varuint32, e.g. the type immediate in a call_indirect.
    RWasmTypeIndexLEB = 6,
    ///R_WASM_GLOBAL_INDEX_LEB.
    /// A global index encoded as a 5-byte varuint32, e.g. the index immediate in a get_global.
    RWasmGlobalIndexLEB = 7,
    ///R_WASM_FUNCTION_OFFSET_I32.
    /// a byte offset within code section for the specific function encoded as a uint32. The offsets start at the actual function code excluding its size field.
    RWasmFunctionOffsetI32 = 8,
    ///R_WASM_SECTION_OFFSET_I32.
    /// an byte offset from start of the specified section encoded as a uint32.
    RWasmSectionOffsetI32 = 9,
    ///R_WASM_EVENT_INDEX_LEB.
    /// An event index encoded as a 5-byte varuint32. Used for the immediate argument of a throw and if_except instruction.
    RWasmEventIndexLEB = 10,
    ///R_WASM_TABLE_NUMBER_LEB.
    /// A table number encoded as a 5-byte varuint32. Used for the table immediate argument in the table.* instructions.
    RWasmTableNumberLEB = 13,
}

pub struct WasmRelocationEntry{
    /// The relocation type
    pub relocation_type: WasmRelocationType,
    /// Offset of the value to rewrite
    pub offset: u32,
    /// The index of the symbol used (or, for R_WASM_TYPE_INDEX_LEB relocations, the index of the type)
    pub index: u32,
    /// For R_WASM_MEMORY_ADDR_LEB, R_WASM_MEMORY_ADDR_SLEB, R_WASM_MEMORY_ADDR_I32, 
    /// R_WASM_FUNCTION_OFFSET_I32, and R_WASM_SECTION_OFFSET_I32 relocations the following field is additionally present:
    /// addend to add to the address
    pub addend: u32,
}

impl WasmRelocationEntry{
    pub fn new_call(offset: u32, index: u32) -> WasmRelocationEntry {
        WasmRelocationEntry{relocation_type: WasmRelocationType::RWasmFunctionIndexLEB, offset: offset, index: index, addend: 0}
    }

    pub fn new_global_use(offset: u32, index: u32) -> WasmRelocationEntry {
        WasmRelocationEntry{relocation_type: WasmRelocationType::RWasmGlobalIndexLEB, offset: offset, index: index, addend: 0}
    }

    pub fn new_static_mem_const(offset: u32, index: u32, addend: u32) -> WasmRelocationEntry {
        WasmRelocationEntry{relocation_type: WasmRelocationType::RWasmMemoryAddrSLEB, offset: offset, index: index, addend: addend}
    }

    pub fn serialize(&self, out: &mut Vec<u8>){
        out.push(self.relocation_type as u8);
        serialize_u32(self.offset, out);
        serialize_u32_pad(self.index, out);
        if self.relocation_type == WasmRelocationType::RWasmMemoryAddrLEB || self.relocation_type == WasmRelocationType::RWasmMemoryAddrSLEB 
            || self.relocation_type == WasmRelocationType::RWasmMemoryAddrI32
            || self.relocation_type == WasmRelocationType::RWasmFunctionOffsetI32 || self.relocation_type == WasmRelocationType::RWasmSectionOffsetI32 {
            serialize_u32(self.addend, out);
        }
    }
}

pub struct WasmRelocSection{
    /// Sequence of relocation entries
    pub entries: Vec<WasmRelocationEntry>,
}

impl WasmRelocSection{
    pub fn serialize(&self, section_index: u32, out: &mut Vec<u8>){
        out.push(0);

        let mut final_data: Vec<u8> = vec![];
        serialize_string(&String::from("reloc."), &mut final_data);

        serialize_u32(section_index, &mut final_data);
        serialize_u32(self.entries.len() as u32, &mut final_data);
        for entry in &self.entries {
            entry.serialize(&mut final_data);
        }

        serialize_u32(final_data.len() as u32, out);
        out.append(&mut final_data);        
    }

    pub fn is_empty(&self) -> bool {
        self.entries.len() == 0
    }
}

/// This is a module fragment that contains all the custom sections used by the linker.
pub struct WasmObjectModuleFragment{
    pub linking_section: WasmLinkingSection,
    pub code_reloc_section: WasmRelocSection,
}

impl WasmObjectModuleFragment{
    pub fn new() -> WasmObjectModuleFragment {
        WasmObjectModuleFragment{
            linking_section: WasmLinkingSection{
                init_funcs: WasmInitFuncs{
                    functions: vec![]
                },
                symbol_table: WasmSymbolTable{
                    infos: vec![], funcs: vec![], globals: vec![], data: vec![]
                }
            },
            code_reloc_section: WasmRelocSection{
                entries: vec![],
            }
        }
    }

    pub fn serialize(&self, code_section_index: u32,out: &mut Vec<u8>){
        if !self.linking_section.is_empty() {
            self.linking_section.serialize(out);
        }
        if !self.code_reloc_section.is_empty() {
            self.code_reloc_section.serialize(code_section_index, out);
        }
    }
}