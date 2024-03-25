#![feature(trait_alias)]
use std::fs;
use std::env;

mod parser;
mod ast;
use chumsky::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("You must provide filecode.");
    }

    let content = fs::read_to_string(args[1].clone()).expect("Cannot read file for some reasons");

    let program = parser::parser().parse(content);
    if let Ok(program) = program {
        println!("{:?}", program);
    } else {
        for err in program.unwrap_err() {
            println!("{:?}", err);
        }
    }
}
