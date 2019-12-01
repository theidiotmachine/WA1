extern crate parser;
use ast::prelude::*;

mod transform;
use transform::transform;

use parity_wasm::elements::serialize_to_file;
use std::path::Path;

pub use errs::Error;

/// Generate an s expression string from a program, to be serialised
/// into a file
pub fn write(program: Program) {
    let mut errors: Vec<Error> = vec![];
    let module = transform(program, &mut errors);
    if !errors.is_empty() {
        println!("{:#?}", errors);
        panic!()
    }
    let string = String::from("out.wasm");
    let from_string = Path::new(&string);
    serialize_to_file(from_string, module);
}
