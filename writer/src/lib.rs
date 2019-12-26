extern crate parser;
use ast::prelude::*;

mod transform;
use transform::transform;

mod mem;
mod wasm_types;

use parity_wasm::elements::serialize_to_file;
use std::path::Path;

pub use errs::Error;
pub use errs::pretty_print_errs;

/// Generate an s expression string from a program, to be serialised
/// into a file
pub fn write(program: Program, out_file: &String) {
    let mut errors: Vec<Error> = vec![];
    let module = transform(program, &mut errors);
    if !errors.is_empty() {
        println!("Compile failed.");
        pretty_print_errs(&errors);
        panic!()
    }
    let string = out_file;
    let from_string = Path::new(&string);
    serialize_to_file(from_string, module).unwrap();
}
