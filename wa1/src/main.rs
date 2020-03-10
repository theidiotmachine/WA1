extern crate clap;
use clap::{Arg, App, SubCommand, ArgMatches};
use std::{fs};
use std::path::{PathBuf, Component};
use std::process::Command;
use ast::{Exports, AST, Imports};

extern crate parser;
use parser::*;
extern crate writer;
pub use writer::prelude::*;

extern crate errs;
pub use errs::Error;
pub use errs::pretty_print_errs;

pub mod build_config;
use build_config::BuildConfig;

struct CachingImporter{
    pub config_path: PathBuf,
    pub src_path: PathBuf,
    pub out_path: PathBuf,
}

fn canon(file_path: &PathBuf, import_path: &PathBuf) -> String {
    let init = file_path.clone().join(import_path.clone());
    let mut out_stack: Vec<String> = vec![];
    for c in init.components() {
        match c {
            Component::CurDir => {},
            Component::ParentDir => {
                out_stack.pop();
            },
            Component::Normal(s) => {
                out_stack.push(s.to_string_lossy().to_string());
            },
            Component::Prefix(_) => {},
            Component::RootDir => {}
        }
    }

    out_stack.join("/")
}

impl Importer for CachingImporter {
    fn import(&mut self, import_path_name: &String, file_name: &String) -> Result<Imports, String> {
        let file_name_path = PathBuf::from(file_name);
        let o_file_path = file_name_path.parent();
        let file_path = match o_file_path{
            Some(file_path) => file_path.to_owned(),
            None => PathBuf::new()
        };

        let mut import_path = self.config_path.clone().join(self.src_path.clone()).join(file_path.clone()).join(import_path_name.clone());
        import_path.set_extension("wa1");

        let o_module_name = import_path.file_stem();
        let module_name = match o_module_name {
            None => return Err(String::from("Can't parse import name")),
            Some(module_name) => module_name.to_string_lossy().to_string()
        };

        let unique_name = canon(&(file_path.to_owned()), &PathBuf::from(import_path_name));

        let mut output_path = self.config_path.clone().join(self.out_path.clone()).join(file_path.clone()).join(import_path_name.clone());
        output_path.set_extension("wsbe");

        let r_output_metadata = output_path.metadata();
        match r_output_metadata {
            Err(e) => {
                let o_exports = parse_exports(&import_path, false);
                match o_exports {
                    Some(exports) => {
                        write_exports(&output_path, &exports);
                        Ok(Imports{exports, module_name: module_name, unique_name})
                    },
                    None => Err(e.to_string()) 
                }
            },
            Ok(output_metadata) => {
                let r_input_metadata = import_path.metadata();
                match r_input_metadata {
                    Err(e) => Err(e.to_string()),
                    Ok(input_metadata) => {
                        if input_metadata.modified().unwrap() > output_metadata.modified().unwrap() {
                            let o_exports = parse_exports(&import_path, false);
                            match o_exports {
                                Some(exports) => {
                                    write_exports(&output_path, &exports);
                                    Ok(Imports{exports, module_name, unique_name})
                                },
                                None => Err(String::from("Failed to parse"))
                            }
                        } else {
                            let r_exports = read_exports(&output_path);
                            match r_exports {
                                Ok(exports) => Ok(Imports{exports, module_name, unique_name}),
                                Err(e) => Err(e) 
                            }
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
    let module_name = output.file_stem().unwrap().to_string_lossy().into_owned();
            
    compile_if_changed(&output, &PathBuf::from(input), &PathBuf::from(input), is_unsafe, TranslationUnitType::Simple, OutputType::Standalone, &module_name,
        StartFuncType::WASMCallCtors, &mut importer
    )
}

fn parse_exports(input: &PathBuf, is_unsafe: bool) -> Option<Exports> {
    let r_input_contents = fs::read_to_string(input);
    match r_input_contents{
        Ok(input_contents) => {
            let mut parser = Parser::new(input_contents.as_str()).unwrap();
            Some(parser.parse_phase_1(is_unsafe, &(input.to_string_lossy().to_string())))
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
                    0
                }
            }
        }
    }
}

fn read_exports(exports_file: &PathBuf) -> Result<Exports, String> {
    let r_input_contents = fs::read_to_string(exports_file);
    match r_input_contents{
        Err(e) => Err(e.to_string()),
        Ok(input_contents) => {
            let r_exports = serde_json::from_str::<Exports>(&input_contents);
            match r_exports {
                Err(e) => Err(e.to_string()),
                Ok(exports) => Ok(exports)
            }
        }
    }
}


fn parse(sf_full_path: &PathBuf, sf_name: &PathBuf, is_unsafe: bool, module_name: &String, start_func_type: StartFuncType,importer: &mut dyn Importer) -> Option<AST> {
    let r_input_contents = fs::read_to_string(sf_full_path.clone());
    match r_input_contents {
        Ok(input_contents) => {
            let mut parser = Parser::new(input_contents.as_str()).unwrap();
            let o_script = parser.parse_full(is_unsafe, importer, module_name, start_func_type, &(sf_name.to_string_lossy().to_string()));
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

fn compile_ast(ast: &AST, tu_type: TranslationUnitType, output_type: OutputType, module_name: &String) -> Option<WasmModule> {
    let mut errs: Vec<Error> = vec![];

    let m = compile(ast, tu_type, output_type, module_name, &mut errs);

    if errs.len() > 0 {
        pretty_print_errs(&errs);
        None
    } else {
        println!("Success");
        Some(m)
    }
}

fn write_wasm(of_full_path: &PathBuf, module_name: &String, module: &mut WasmModule) -> i32 {
    let mut data: Vec<u8> = vec![];
    module.serialize(module_name, &mut data);
    let r = fs::write(of_full_path, data);
            
    match r {
        Ok(_) => 0,
        Err(e) => {
            println!("ERROR: {}", e);
            1
        }
    }
}

fn compile_if_changed(
    of_full_path: &PathBuf, 
    sf_full_path: &PathBuf, 
    sf_name: &PathBuf, 
    is_unsafe: bool, 
    tu_type: TranslationUnitType, 
    output_type: OutputType,
    module_name: &String, 
    start_func_type: StartFuncType,
    importer: &mut dyn Importer
) -> i32 {
    let of_full_path_string = of_full_path.to_string_lossy();
    let sf_full_path_string = sf_full_path.to_string_lossy();

    println!("Parsing {} to {}", &sf_full_path_string, &of_full_path_string);
    
    let r_output_metadata = of_full_path.metadata();
    match r_output_metadata {
        Err(_) => {
            let o_ast = parse(sf_full_path, sf_name, is_unsafe, module_name, start_func_type, importer);
            match o_ast {
                None => 1,
                Some(ast) => {
                    let o_module = compile_ast(&ast, tu_type, output_type, module_name);
                    match o_module {
                        Some(mut module) => write_wasm(of_full_path, module_name, &mut module),
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
                        let o_program = parse(sf_full_path, sf_name, is_unsafe, module_name, start_func_type, importer);
                        match o_program {
                            None => 1,
                            Some(program) => {
                                let o_module = compile_ast(&program, tu_type, output_type, module_name);
                                match o_module {
                                    Some(mut module) => write_wasm(of_full_path, module_name, &mut module),
                                    None => 1
                                }
                            }
                        }           
                    } else {
                        println!("Skipping, up to date");
                        0
                    }
                }                
            }
        }
    }       
}

//const WASM_EXE: &'static str = "/home/ME/Code/Ext/LLVM/llvm-build/bin/wasm-ld";
//const WASM_EXE: &'static str = "wasm-ld-9";

fn build(matches: &ArgMatches) -> i32 {
    let config = matches.value_of("CONFIG").unwrap();
    let config_contents = fs::read_to_string(config).expect(format!("Couldn't read {}", config).as_str());
    let r_build_config: Result<BuildConfig, serde_json::Error> = serde_json::from_str(&config_contents);    
    let output_type: OutputType = OutputType::Standalone;
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

            let out_path = config_path.clone().join(build_config.out_path.clone());

            let r = fs::create_dir_all(out_path.clone());
            match r {
                Ok(_) => {},
                Err(e) => {
                    println!("Failed to create output dir because {}", e);
                    return 1;
                }
            }

            let clean = matches.is_present("clean");
            if clean {
                println!("Cleaning {}", out_path.clone().to_string_lossy());
                let r = fs::remove_dir_all(out_path.clone());
                match r {
                    Ok(_) => {},
                    Err(e) => {
                        println!("Failed to clean because {}", e);
                        return 1;
                    }
                }

                let r = fs::create_dir_all(out_path.clone());
                match r {
                    Ok(_) => {},
                    Err(e) => {
                        println!("Failed to create output dir because {}", e);
                        return 1;
                    }
                }
            }

            //first, build the files
            for sf in &build_config.source_files {
                let sf_full_path = config_path.clone().join(build_config.src_path.clone()).join(sf.file_name.clone());
                let mut of_full_path = config_path.clone().join(build_config.out_path.clone()).join(sf.file_name.clone());
                of_full_path.set_extension("wasm");
                let module_name = sf.file_name.file_stem().unwrap().to_string_lossy().into_owned();
                let this_out = compile_if_changed(
                    &of_full_path, &sf_full_path, &sf.file_name, sf.is_unsafe, TranslationUnitType::LinkedSourceFile, output_type, &module_name, 
                    StartFuncType::Start, &mut importer
                );
                if this_out != 0 {
                    out = this_out;
                }
            }

            //and now build the entry point file
            let epf_full_path = config_path.clone().join(build_config.src_path.clone()).join(build_config.entry_point.file_name.clone());
            let mut of_full_path = config_path.clone().join(build_config.out_path.clone()).join(build_config.entry_point.file_name.clone());
            of_full_path.set_extension("wasm");
            let this_out = compile_if_changed(
                &of_full_path, &epf_full_path, &build_config.entry_point.file_name, build_config.entry_point.is_unsafe, 
                TranslationUnitType::LinkedEntryPoint, output_type, &build_config.module_name, StartFuncType::Start, &mut importer
            );
            if this_out != 0 {
                out = this_out;
            }

            if out == 0 {
                //let's link!
                let mut args: Vec<String> = vec![];
                match output_type{
                    OutputType::StaticLibrary => {
                        args.push(String::from("--no-entry"));
                        args.push(String::from("--relocatable"));
                    },
                    OutputType::Standalone => {
                        args.push(String::from("--export=__wasm_call_ctors"));
                        args.push(String::from("--entry"));
                        args.push(String::from("__wasm_call_ctors"));
                    }
                } 
                
                //args.push(String::from("--verbose"));
                
                let mut of_full_path = config_path.clone().join(build_config.out_path.clone()).join(build_config.module_name.clone());
                of_full_path.set_extension("wasm");

                args.push(String::from("-o"));
                args.push(of_full_path.to_string_lossy().into_owned());
                println!("Linking to {}", of_full_path.to_string_lossy());

                for sf in &build_config.source_files {
                    let mut of_full_path = config_path.clone().join(build_config.out_path.clone()).join(sf.file_name.clone());
                    of_full_path.set_extension("wasm");
                    args.push(format!("{}", of_full_path.to_string_lossy()));
                }
                
                let mut of_full_path = config_path.clone().join(build_config.out_path.clone()).join(build_config.entry_point.file_name.clone());
                of_full_path.set_extension("wasm");
                
                args.push(format!("{}", of_full_path.to_string_lossy()));

                //println!("{} {}", WASM_EXE, args.join(" "));
                let output = Command::new(build_config.wasm_exe).args(args).output().expect("failed to execute process");
                println!("{}", String::from_utf8_lossy(&output.stderr));

                match output.status.code() {
                    Some(code) => {
                        if code == 0 {
                            println!("Success");
                        } else {
                            println!("Linker exited with status code: {}", code);
                        }
                        out = code;
                    },
                    None => {
                        println!("Linker terminated by signal");
                        out = 1;
                    }
                } 
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
                .arg(Arg::with_name("clean")
                    .help("delete all output files")
                    .long("clean"))
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