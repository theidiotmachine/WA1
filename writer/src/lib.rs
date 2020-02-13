extern crate parser;

mod transform;
pub use transform::{compile, TranslationUnitType, OutputType};

pub mod prelude {
    pub use super::{compile, TranslationUnitType, OutputType};
    pub use super::wasm::wasm_module::{WasmModule};
}

mod mem;
mod wasm_types;

pub use errs::Error;
pub use errs::pretty_print_errs;

mod wasm;