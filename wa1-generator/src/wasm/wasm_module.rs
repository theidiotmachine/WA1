use crate::wasm::wasm_sections::*;
use crate::wasm::wasm_object_file::WasmObjectModuleFragment;
use crate::generate::TranslationUnitType;

/// IR type that represents a wasm module. Serialization of this type
/// will produce a wasm binary.
pub struct WasmModule {
    pub type_section: WasmTypeSection,
    pub import_section: WasmImportSection,
    pub func_section: WasmFuncSection,
    pub mem_section: WasmMemSection,
    pub global_section: WasmGlobalSection,
    pub export_section: WasmExportSection,
    pub start_section: WasmStartSection,
    pub code_section: WasmCodeSection,
    pub data_section: WasmDataSection,
    pub object_file_sections: Option<WasmObjectModuleFragment>,
}

impl WasmModule{
    pub fn new(tu_type: TranslationUnitType) -> WasmModule {
        WasmModule{
            type_section: WasmTypeSection::new(),
            import_section: WasmImportSection::new(),
            func_section: WasmFuncSection::new(),
            mem_section: WasmMemSection::new(),
            global_section: WasmGlobalSection::new(),
            export_section: WasmExportSection::new(),
            start_section: WasmStartSection::new(),
            code_section: WasmCodeSection::new(),
            data_section: WasmDataSection::new(),
            object_file_sections: if tu_type == TranslationUnitType::Simple { None } else { Some(WasmObjectModuleFragment::new()) },
        }
    }

    pub fn serialize(&mut self, module_name: &String, out: &mut Vec<u8>) {
        let mut dumb = vec![0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00];
        out.append(&mut dumb);
        
        let mut code_section_index = 1;
        self.type_section.serialize(out);

        if !self.import_section.is_empty() {
            self.import_section.serialize(out);
            code_section_index += 1;
        }

        self.func_section.serialize(out);
        code_section_index += 1;

        if !self.data_section.is_empty() {
            self.mem_section.serialize(out);
            code_section_index += 1;
        }

        if !self.global_section.is_empty() {
            self.global_section.serialize(out);
            code_section_index += 1;
        }

        if !self.export_section.is_empty() {
            self.export_section.serialize(out);
            code_section_index += 1;
        }

        if !self.start_section.is_empty() {
            self.start_section.serialize(out);
            code_section_index += 1;
        }

        if !self.data_section.is_empty() {
            //this is irritating. We need the symbol table finished before we go into the code 
            // serialization, so we take time out of our busy schedule to register a data 
            // symbol, oh data did you not think to be more considerate
            match &mut self.object_file_sections {
                None => {},
                Some(reloc) => {
                    self.data_section.register_data_symbol(module_name, reloc);
                }
            }
        }

        match &mut self.object_file_sections {
            None => {
                self.code_section.serialize(out);
            },
            Some(reloc) => {
                self.code_section.serialize_reloc(reloc, out);
            }
        }

        if !self.data_section.is_empty() {
            self.data_section.serialize(out);
        }

        match &self.object_file_sections {
            None => {},
            Some(reloc) => {
                reloc.serialize(code_section_index, out);
            }
        }
    }
}