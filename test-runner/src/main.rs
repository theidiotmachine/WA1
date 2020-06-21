extern crate clap;
use clap::{Arg, App};
use std::fs;
use wasmtime::*;
use wa1_lexer::prelude::*;

fn main() {
    let matches = App::new("wa1 test runner")
        .version("0.0.1")
        .about("WA1 test runner")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to run")
            .required(true)
            .index(1))
        .arg(Arg::with_name("FUNC_CALL")
            .short("f")
            .long("func-call")
            .value_name("FUNC_CALL")
            .help("function to call")
            .takes_value(true))
        .get_matches();

    let input = matches.value_of("INPUT").unwrap();
    println!("Running {}", &input);

    // actually wrong - we should be doing a check. Good enough for now
    let func_call_str = matches.value_of("FUNC_CALL").unwrap();
    
    let wasm = fs::read(input).expect("wasm file");

    //fire up wastime
    //let engine = HostRef::new(Engine::new(Config::new().debug_info(true)));
    let engine = HostRef::new(Engine::default());
    let store = HostRef::new(Store::new(&engine));
    let module = HostRef::new(Module::new(&store, &wasm).expect("wasm module"));
    let instance = Instance::new(&store, &module, &[]).expect("wasm instance");
    
    // this code is VILE!
    let mut name: String = String::from("");
    let mut args: Vec<f64> = vec![];
    let s = Scanner::new(func_call_str);
    for t in s {
        let i = t.unwrap();
        let token = i.token;
        match token {
            Token::Ident(n) => {
                name.push_str(n.as_str());
            },
            Token::Punct(p) => {
                match p {
                    Punct::Period => {
                        name.push('.');
                    },
                    _ => {}
                }
            },
            Token::Number(n) => {
                args.push(n.parse_f64().unwrap());
            },
            _ => {}
        }
    }
    
    println!("** running ctor **");
    let o_ctor_func = instance.find_export_by_name("__wasm_call_ctors");
    match o_ctor_func {
        Some(ctor_func) => {
            let f = ctor_func.func().expect("__wasm_call_ctors must be a function");
            f.borrow().call(&vec![]).expect("success");
        },
        _ => {}
    }

    println!("** {} **", name.as_str());
    let func = instance.find_export_by_name(name.as_str()).expect("answer").func().expect("function");
    let mut args_to_pass: Vec<Val> = vec![];
    for arg in args {
        args_to_pass.push(Val::from(arg));
    }
                    
    let result = func.borrow().call(&args_to_pass).expect("success");
    if result.len() > 0 {
        match result[0]{
            Val::F32(n) => println!("Answer f32: {}", n),
            Val::F64(n) => println!("Answer f64: {}", f64::from_bits(n)),
            Val::I32(n) => println!("Answer i32: {}", n),
            Val::I64(n) => println!("Answer i64: {}", n),
            _ => println!("Unknown answer"),
        }
    } else {
        println!("Answer void");
    }
}
    
