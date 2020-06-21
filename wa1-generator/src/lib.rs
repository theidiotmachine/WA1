extern crate wa1_parser;

mod transform;
mod compile_int;
mod compile_ptr;
pub use transform::{compile, TranslationUnitType, OutputType};

pub mod prelude {
    pub use super::{compile, TranslationUnitType, OutputType};
    pub use super::wasm::wasm_module::{WasmModule};
}

mod mem;
mod wasm_types;

pub use wa1_errs::Error;
pub use wa1_errs::pretty_print_errs;

mod wasm;