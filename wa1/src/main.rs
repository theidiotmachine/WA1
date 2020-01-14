extern crate clap;
use clap::{Arg, App, SubCommand, ArgMatches};
use std::{fs, env};
use std::path::PathBuf;
use ast::Exports;

extern crate parser;
use parser::*;
extern crate writer;

extern crate errs;
pub use errs::Error;
pub use errs::pretty_print_errs;

pub mod build_config;
use build_config::BuildConfig;

struct SimpleImporter{
    pub current_path: PathBuf,
    pub input_path: PathBuf,
}

impl SimpleImporter {
    fn new(input_path: &PathBuf) -> SimpleImporter {
        let e_path = env::current_dir();
        let path = match e_path {
            Err(err) => {
                println!("Internal error - {}.", err);
                panic!();
            },
            Ok(path) => path
        };
        let o_parent = input_path.parent();
        match o_parent {
            Some(parent) => SimpleImporter{current_path: path, input_path: parent.to_owned()},
            None => SimpleImporter{current_path: path, input_path: PathBuf::new()},
        }
    }
}

impl Importer for SimpleImporter {
    fn import(&mut self, path_name: &String) -> Option<Exports> {
        let mut import_path = self.current_path.clone().join(self.input_path.clone()).join(path_name);
        import_path.set_extension("wa1");
        let r_input_contents = fs::read_to_string(import_path.clone());
        match r_input_contents {
            Ok(input_contents) => {
                
                let mut parser = Parser::new(input_contents.as_str()).unwrap();
                Some(parser.parse_phase_1(true))
            },
            Err(_) => {
                None
            }
        }
    }
}

fn simple_parse(matches: &ArgMatches) -> i32 {
    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wasm");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Parsing {} to {}", &input, &output);
    let is_unsafe = matches.is_present("unsafe");
    
    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input).as_str());

    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let o_script = parser.parse_full(is_unsafe, false, &mut SimpleImporter::new(&PathBuf::from(input)));
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
    let default_output = input.replace("wa1", "wsbo");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Parsing {} to {}", &input, &output);
    let is_unsafe = matches.is_present("unsafe");
    
    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input).as_str());

    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    let o_script = parser.parse_full(is_unsafe, true, &mut SimpleImporter::new(&PathBuf::from(input)));
    
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

fn generate_exports_from_file(input: &PathBuf, is_unsafe: bool) -> Exports {
    let input_contents = fs::read_to_string(input).expect(format!("Couldn't read {}", input.to_string_lossy()).as_str());
    let mut parser = Parser::new(input_contents.as_str()).unwrap();
    parser.parse_phase_1(is_unsafe)
}

fn write_exports_to_file(output: &PathBuf, exports: &Exports) -> i32 {
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

fn read_exports_from_file(exports_file: &PathBuf) -> Exports {
    let input_contents = fs::read_to_string(exports_file).expect(format!("Couldn't read {}", exports_file.to_string_lossy()).as_str());
    let exports: Exports = serde_json::from_str(&input_contents).unwrap();
    exports
}

fn export_parse(matches: &ArgMatches) -> i32 {
    let input = matches.value_of("INPUT").unwrap();
    let default_output = input.replace("wa1", "wsbe");
    let output = matches.value_of("OUTPUT").unwrap_or(default_output.as_str());
    println!("Parsing exports of {} to {}", &input, &output);
    let is_unsafe = matches.is_present("unsafe");

    let exports = generate_exports_from_file(&PathBuf::from(input).to_owned(), is_unsafe);
    write_exports_to_file(&PathBuf::from(output).to_owned(), &exports)
}

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
                println!("*_* no output metadata");
                let exports = generate_exports_from_file(&import_path, false);
                write_exports_to_file(&output_path, &exports);
                Some(exports)
            },
            Ok(output_metadata) => {
                println!("*_* output metadata");
                let r_input_metadata = import_path.metadata();
                match r_input_metadata {
                    Err(_) => {
                        None
                    },  
                    Ok(input_metadata) => {
                        if input_metadata.modified().unwrap() > output_metadata.modified().unwrap() {
                            let exports = generate_exports_from_file(&import_path, false);
                            write_exports_to_file(&output_path, &exports);
                            Some(exports)
                        } else {
                            Some(read_exports_from_file(&output_path))
                        }
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
            for sf in build_config.source_files {
                let sf_full_path = config_path.clone().join(build_config.src_path.clone()).join(sf.file_name.clone());
                let mut of_full_path = config_path.clone().join(build_config.out_path.clone()).join(sf.file_name.clone());
                of_full_path.set_extension("wsbo");
                let of_full_path_string = of_full_path.to_string_lossy();
                let sf_full_path_string = sf_full_path.to_string_lossy();
                println!("Parsing {} to {}", &sf_full_path_string, &of_full_path_string);
    
                let input_contents = fs::read_to_string(sf_full_path.clone()).expect(format!("Couldn't read {}", sf_full_path_string).as_str());

                let mut parser = Parser::new(input_contents.as_str()).unwrap();
                let o_script = parser.parse_full(sf.is_unsafe, true, &mut importer);
                match o_script {
                    Err(errs) => {
                        println!("Parse failed.");
                        pretty_print_errs(&errs);
                        return 1;
                    },
                    _ => {
                        let script = o_script.unwrap();

                        /*
                        let o_ok = writer::write(script, &output.to_owned());
                        match o_ok {
                            Err(errs) => {
                                println!("Parse failed.");
                                pretty_print_errs(&errs);
                                return 1;
                            },      
                            _ => {
                                println!("Success");
                            }
                        }
                        */
                    }
                }
            }

            let epf_full_path = config_path.clone().join(build_config.src_path.clone()).join(build_config.entry_point.clone());
            let of_full_path = config_path.clone().join(build_config.out_path.clone()).join(build_config.out_file_name.clone());
            let of_full_path_string = of_full_path.to_string_lossy();
            let epf_full_path_string = epf_full_path.to_string_lossy();
            println!("Parsing {} to {}", &epf_full_path_string, &of_full_path_string);
    
            let input_contents = fs::read_to_string(epf_full_path.clone()).expect(format!("Couldn't read {}", epf_full_path_string).as_str());
            let mut parser = Parser::new(input_contents.as_str()).unwrap();
            let o_script = parser.parse_full(false, false, &mut importer);
            match o_script {
                Err(errs) => {
                    println!("Parse failed.");
                    pretty_print_errs(&errs);
                return 1;
                },
                _ => {
                    let script = o_script.unwrap();
                }
            }
            0
        }
    }
    
}

fn main() {
    let matches = App::new("wa1")
        .version("0.0.2")
        .about("Wasabi compiler")
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
        ("simple-parse",    Some(sub_m)) => {simple_parse(&sub_m)},
        ("export-parse",    Some(sub_m)) => {export_parse(&sub_m)}, 
        ("pic-parse",       Some(sub_m)) => {pic_parse(&sub_m)},
        ("build",           Some(sub_m)) => {build(&sub_m)},
        _ => {
            1
        },
    };

    ::std::process::exit(rv);
}