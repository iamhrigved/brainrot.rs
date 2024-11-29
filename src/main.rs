use sigma::Sigma;
use std::env;

pub mod scanner;
pub mod sigma;
pub mod tokens;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut sigma = Sigma::new();

    if args.len() > 1 {
        sigma.run_file(&args[1]);
    } else {
        sigma.run_prompt();
    }
}
