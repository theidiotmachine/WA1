/*
use ress::Tokenizer;

static STRINGS: &[&str] = &[
    "0.2",
];


fn main() {
    for s in STRINGS {
        let d = Tokenizer::new(s).next().unwrap();
        println!("{}, {} {}", s, d.start, d.end);
        core::mem::forget(d);
    }
}
*/

extern crate parser;
use parser::*;
extern crate writer;

fn main() {
    let js = include_str!("one.ws");
    println!("go");
    let mut builder = Builder::new();
    println!("parse");
    let mut p = builder
        .js(js)
        .build()
        .unwrap();
    let script = p.parse().unwrap();
    println!("{:#?}", script);
    writer::write(script, &String::from("out.wasm"));  
}