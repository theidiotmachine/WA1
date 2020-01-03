extern crate clap;
use clap::{Arg, App};
use std::fs;
extern crate parser;
use parser::*;
extern crate writer;

extern crate errs;
pub use errs::Error;
pub use errs::pretty_print_errs;

fn main() {
    let matches = App::new("wa1")
        .version("0.0.1")
        .about("WA1 compiler")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .required(true)
            .index(1))
        .arg(Arg::with_name("OUTPUT")
            .short("o")
            .long("output")
            .value_name("OUTPUT")
            .help("output file")
            .takes_value(true))
        .arg(Arg::with_name("unsafe")
            .help("unsafe parse mode")
            .long("unsafe")
        )
        .get_matches();

    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wasm");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Building {} to {}", &input, &output);

    let is_unsafe = matches.is_present("unsafe");

    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input).as_str());

    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let o_script = parser.parse(is_unsafe);
    match o_script {
        Err(errs) => {
            println!("Parse failed.");
            pretty_print_errs(&errs);
        },
        _ => {
            let script = o_script.unwrap();
            //println!("{:#?}", script);
            writer::write(script, &output.to_owned());  
        }
    }
}