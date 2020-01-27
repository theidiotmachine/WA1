use crate::wasm::wasm_sections::*;

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
}

impl WasmModule{
    pub fn new() -> WasmModule {
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
        }
    }

    pub fn serialize(&mut self, out: &mut Vec<u8>) {
        let mut dumb = vec![0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00];
        out.append(&mut dumb);
        self.type_section.serialize(out);
        if !self.import_section.is_empty() {
            self.import_section.serialize(out);
        }
        self.func_section.serialize(out);
        if !self.data_section.is_empty() {
            self.mem_section.serialize(out);
        }
        if !self.global_section.is_empty() {
            self.global_section.serialize(out);
        }
        if !self.export_section.is_empty() {
            self.export_section.serialize(out);
        }
        self.start_section.serialize(out);
        self.code_section.serialize(out);
        if !self.data_section.is_empty() {
            self.data_section.serialize(out);
        }
    }
}