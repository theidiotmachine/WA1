extern crate clap;
use clap::{Arg, App, SubCommand, ArgMatches};
use std::{fs};
use std::path::PathBuf;
use ast::{Exports, Program};

extern crate parser;
use parser::*;
extern crate writer;
pub use writer::prelude::*;

extern crate errs;
pub use errs::Error;
pub use errs::pretty_print_errs;

pub mod build_config;
use build_config::BuildConfig;
use parity_wasm::elements::{Module};
use parity_wasm::elements::serialize_to_file;

struct CachingImporter{
    pub config_path: PathBuf,
    pub src_path: PathBuf,
    pub out_path: PathBuf,
}

impl Importer for CachingImporter {
    fn import(&mut self, path_name: &String) -> Option<Exports> {
        let mut import_path = self.config_path.clone().join(self.src_path.clone()).join(path_name.clone());
        import_path.set_extension("wa1");

        let mut output_path = self.config_path.clone().join(self.out_path.clone()).join(path_name.clone());
        output_path.set_extension("wsbe");

        let r_output_metadata = output_path.metadata();
        match r_output_metadata {
            Err(_) => {
                let o_exports = parse_exports(&import_path, false);
                match o_exports {
                    Some(exports) => {
                        write_exports(&output_path, &exports);
                        Some(exports)
                    },
                    None => None 
                }
            },
            Ok(output_metadata) => {
                let r_input_metadata = import_path.metadata();
                match r_input_metadata {
                    Err(_) => {
                        None
                    },  
                    Ok(input_metadata) => {
                        if input_metadata.modified().unwrap() > output_metadata.modified().unwrap() {
                            let o_exports = parse_exports(&import_path, false);
                            match o_exports {
                                Some(exports) => {
                                    write_exports(&output_path, &exports);
                                    Some(exports)
                                },
                                None => None 
                            }
                        } else {
                            Some(read_exports(&output_path))
                        }
                    }
                }
            }
        }
    }
}


fn build_simple(matches: &ArgMatches) -> i32 {
    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wasm");
    let output = PathBuf::from(matches.value_of("OUTPUT").unwrap_or(default_output.as_str()));
    let is_unsafe = matches.is_present("unsafe");
    let mut importer = CachingImporter{config_path: PathBuf::new(), src_path: PathBuf::new(), out_path: PathBuf::new()};
            
    compile_if_changed(&output, &PathBuf::from(input), is_unsafe, false, &mut importer)
}

fn parse_exports(input: &PathBuf, is_unsafe: bool) -> Option<Exports> {
    let r_input_contents = fs::read_to_string(input);
    match r_input_contents{
        Ok(input_contents) => {
            let mut parser = Parser::new(input_contents.as_str()).unwrap();
            Some(parser.parse_phase_1(is_unsafe))
        },
        Err(err) => {
            println!("ERROR: {}", err);
            None
        }
    }
}

fn write_exports(output: &PathBuf, exports: &Exports) -> i32 {
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

fn read_exports(exports_file: &PathBuf) -> Exports {
    let input_contents = fs::read_to_string(exports_file).expect(format!("Couldn't read {}", exports_file.to_string_lossy()).as_str());
    let exports: Exports = serde_json::from_str(&input_contents).unwrap();
    exports
}


fn parse(sf_full_path: &PathBuf, is_unsafe: bool, is_pic: bool, importer: &mut dyn Importer) -> Option<Program> {
    let r_input_contents = fs::read_to_string(sf_full_path.clone());
    match r_input_contents {
        Ok(input_contents) => {
            let mut parser = Parser::new(input_contents.as_str()).unwrap();
            let o_script = parser.parse_full(is_unsafe, is_pic, importer);
            match o_script {
                Err(errs) => {
                    println!("Parse failed.");
                    pretty_print_errs(&errs);
                    None
                },
                Ok(script) => {
                    Some(script)
                }
            }
            
        },
        Err(err) => {
            println!("ERROR: couldn't open {} because {}", sf_full_path.to_string_lossy(), err);
            None
        }
    }
}

fn compile(program: &Program) -> Option<Module> {
    let mut errs: Vec<Error> = vec![];
    let m = transform(program, &mut errs);
    if errs.len() > 0 {
        pretty_print_errs(&errs);
        None
    } else {
        Some(m)
    }
}

fn write_wasm(of_full_path: &PathBuf, module: Module) -> i32 {
    let r = serialize_to_file(of_full_path, module);
    match r {
        Ok(_) => 0,
        Err(e) => {
            println!("ERROR: {}", e);
            1
        }
    }
}

fn compile_if_changed(of_full_path: &PathBuf, sf_full_path: &PathBuf, is_unsafe: bool, is_pic: bool, importer: &mut dyn Importer) -> i32 {
    let of_full_path_string = of_full_path.to_string_lossy();
    let sf_full_path_string = sf_full_path.to_string_lossy();
    println!("Parsing {} to {}", &sf_full_path_string, &of_full_path_string);
    
    let r_output_metadata = of_full_path.metadata();
    match r_output_metadata {
        Err(_) => {
            let o_program = parse(sf_full_path, is_unsafe, is_pic, importer);
            match o_program {
                None => 1,
                Some(program) => {
                    let o_module = compile(&program);
                    match o_module {
                        Some(module) => write_wasm(of_full_path, module),
                        None => 1
                    }
                }
            }
        },
        Ok(output_metadata) => {
            let r_input_metadata = sf_full_path.metadata();
            match r_input_metadata {
                Err(e) => {
                    println!("ERROR: Can't load {} because {}", sf_full_path_string, e);
                    1
                },
                Ok(input_metadata) => {
                    if input_metadata.modified().unwrap() > output_metadata.modified().unwrap() {
                        let o_program = parse(sf_full_path, is_unsafe, is_pic, importer);
                        match o_program {
                            None => 1,
                            Some(program) => {
                                let o_module = compile(&program);
                                match o_module {
                                    Some(module) => write_wasm(of_full_path, module),
                                    None => 1
                                }
                            }
                        }           
                    } else {
                        0
                    }
                }                
            }
        }
    }
                
}

fn build(matches: &ArgMatches) -> i32 {
    let config = matches.value_of("CONFIG").unwrap();
    let config_contents = fs::read_to_string(config).expect(format!("Couldn't read {}", config).as_str());
    let r_build_config: Result<BuildConfig, serde_json::Error> = serde_json::from_str(&config_contents);    
    match r_build_config {
        Err(e) => {
            println!("ERROR: Failed to parse {}, because {}", config, e);
            1
        },
        Ok(build_config) => {
            let c = PathBuf::from(config);
            let o_config_path = c.parent();
            let config_path: PathBuf = match o_config_path {
                None => PathBuf::new(),
                Some(parent) => parent.to_owned()
            };
            let mut importer = CachingImporter{config_path: config_path.clone(), src_path: build_config.src_path.clone(), out_path: build_config.out_path.clone()};
            let mut out = 0;
            for sf in build_config.source_files {
                let sf_full_path = config_path.clone().join(build_config.src_path.clone()).join(sf.file_name.clone());
                let mut of_full_path = config_path.clone().join(build_config.out_path.clone()).join(sf.file_name.clone());
                of_full_path.set_extension("wasm");
                let this_out = compile_if_changed(&of_full_path, &sf_full_path, sf.is_unsafe, true, &mut importer);
                if this_out != 0 {
                    out = this_out;
                }
            }

            let epf_full_path = config_path.clone().join(build_config.src_path.clone()).join(build_config.entry_point.file_name.clone());
            let of_full_path = config_path.clone().join(build_config.out_path.clone()).join(build_config.out_file_name.clone());
            let this_out = compile_if_changed(&of_full_path, &epf_full_path, build_config.entry_point.is_unsafe, true, &mut importer);
            if this_out != 0 {
                out = this_out;
            }
            out
        }
    }
    
}

fn main() {
    let matches = App::new("wa1")
        .version("0.0.2")
        .about("Wasabi compiler")
        .subcommands(
            vec![
                SubCommand::with_name("build-simple")
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

                SubCommand::with_name("build")
                .about("Build from an input configuration")
                .arg(Arg::with_name("CONFIG")
                    .help("Input configuration")
                    .required(true)
                    .index(1))
                ,
            ]
        )
        .get_matches();
        
    let rv = match matches.subcommand() {
        ("build-simple",    Some(sub_m)) => {build_simple(&sub_m)},
        ("build",           Some(sub_m)) => {build(&sub_m)},
        _ => {
            1
        },
    };

    ::std::process::exit(rv);
}