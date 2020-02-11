use serde::{Serialize, Deserialize};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct SourceFile {
    pub file_name: PathBuf,
    pub is_unsafe: bool,
}


#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BuildConfig{
    pub entry_point: SourceFile,
    pub source_files: Vec<SourceFile>,
    pub src_path: PathBuf,
    pub out_path: PathBuf,
    pub module_name: String,
    pub wasm_exe: String,
}