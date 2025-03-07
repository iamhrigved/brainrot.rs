use sigma::Sigma;
use std::env;

mod environment;
mod error;
mod interpreter;
mod libs;
mod parser;
mod scanner;
mod sigma;
mod token;
mod value;

use std::string::String;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut sigma = Sigma::new();

    if args.len() > 1 {
        sigma.run_file(&args[1]);
    } else {
        sigma.run_prompt();
    }
}
