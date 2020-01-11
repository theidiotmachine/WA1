extern crate clap;
use clap::{Arg, App, SubCommand, ArgMatches};
use std::fs;
extern crate parser;
use parser::*;
extern crate writer;

extern crate errs;
pub use errs::Error;
pub use errs::pretty_print_errs;

fn simple_parse(matches: &ArgMatches) -> i32 {
    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wasm");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Parsing {} to {}", &input, &output);
    let is_unsafe = matches.is_present("unsafe");

    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input).as_str());

    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let o_script = parser.parse_full(is_unsafe, false);
    match o_script {
        Err(errs) => {
            println!("Parse failed.");
            pretty_print_errs(&errs);
            1
        },
        _ => {
            let script = o_script.unwrap();
            let o_ok = writer::write(script, &output.to_owned());
            match o_ok {
                Err(errs) => {
                    println!("Parse failed.");
                    pretty_print_errs(&errs);
                    1
                },      
                _ => {
                    println!("Success");
                    0
                }
            }
        }
    }
}

fn pic_parse(matches: &ArgMatches) -> i32 {
    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wa1o");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Parsing {} to {}", &input, &output);
    let is_unsafe = matches.is_present("unsafe");

    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input).as_str());

    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let o_script = parser.parse_full(is_unsafe, true);
    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let exports = parser.parse_phase_1(is_unsafe);
    
    match o_script {
        Err(errs) => {
            println!("Parse failed.");
            pretty_print_errs(&errs);
            1
        },
        _ => {
            let script = o_script.unwrap();
            panic!();
            //
            let o_ok = writer::write(script, &output.to_owned());
            match o_ok {
                Err(errs) => {
                    println!("Parse failed.");
                    pretty_print_errs(&errs);
                    1
                },      
                _ => {
                    println!("Success");
                    0
                }
            }
        }
    }
}

fn export_parse(matches: &ArgMatches) -> i32 {
    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wa1e");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Parsing exports of {} to {}", &input, &output);
    let is_unsafe = matches.is_present("unsafe");

    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input).as_str());

    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let exports = parser.parse_phase_1(is_unsafe);
    let r_string = serde_json::to_string(&exports);
    match r_string {
        Err(err) => {
            println!("Export parse failed - {}.", err);
            1
        }, 
        Ok(s) => {
            let r = fs::write(output, s);
            match r {
                Err(err) => {
                    println!("Export parse failed - {}.", err);
                    1
                },
                _ => {
                    println!("Success");
                    0
                }
            }
        }
    }   
}

fn main() {
    let matches = App::new("wa1")
        .version("0.0.1")
        .about("WA1 compiler")
        .subcommands(
            vec![
                SubCommand::with_name("simple-parse")
                .about("Directly generate a simple WASM file.")
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
                    .help("parse unsafe code")
                    .long("unsafe"))
                ,

                SubCommand::with_name("pic-parse")
                .about("Generate a compilation unit that can be linked.")
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
                    .help("parse unsafe code")
                    .long("unsafe"))
                ,
                    
                SubCommand::with_name("export-parse")
                .about("Mini parse of exports")
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
                    .help("parse unsafe code")
                    .long("unsafe")),
            ]
        )
        .get_matches();
        
    let rv = match matches.subcommand() {
        ("simple-parse",    Some(sub_m)) => {simple_parse(&sub_m)}, // parse was used
        ("export-parse",    Some(sub_m)) => {export_parse(&sub_m)}, // export parse was used
        ("pic-parse",       Some(sub_m)) => {pic_parse(&sub_m)}, // export parse was used
        _ => {
            1
        },
    };

    ::std::process::exit(rv);
}