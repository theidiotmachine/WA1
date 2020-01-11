use serde::{Serialize, Deserialize};
//use parity_wasm::elements::{Module};

use ast::{Exports, Program};

#[derive(Debug, Deserialize, Serialize)]
pub struct ObjectFile{
    pub version: u32,
    pub exports: Exports,
    pub program: Program,
    pub wasm: Vec<u8>
}
