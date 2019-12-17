extern crate clap;
use clap::{Arg, App};
use std::fs;
extern crate parser;
use parser::*;
extern crate writer;

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
        .get_matches();

    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wasm");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Building {} to {}", &input, &output);

    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input).as_str());

    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let o_script = parser.parse();
    match o_script {
        Err(errs) => {
            println!("Parse failed.");
            for err in errs {
                println!("{}", err);    
            }
        },
        _ => {
            let script = o_script.unwrap();
            println!("{:#?}", script);
            writer::write(script, &output.to_owned());  
        }
    }
}