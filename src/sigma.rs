#![allow(clippy::println_empty_string)]
#![allow(clippy::new_without_default)]

use std::fs;
use std::io;
use std::io::Write;
use std::string::String;

use crate::parser::*;

pub struct Sigma {
    had_error: bool,
}

impl Sigma {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    pub fn run_file(&self, file_path: &String) {
        let file_contents: String;

        if !file_path.is_empty() {
            file_contents = fs::read_to_string(file_path).unwrap();
            self.run(file_contents);
        };
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!("🗿 >> ");
            io::stdout().flush().unwrap();

            let mut line = String::new();
            io::stdin().read_line(&mut line).unwrap();

            line = line.trim().to_string();

            if line == ":q" {
                println!("\nBye!");
                break;
            }

            println!(""); // extra line before next prompt appears

            self.run(line);
            self.had_error = false;
        }
    }

    fn run(&self, source: String) {
        let mut parser = Parser::new(source);
        parser.parse();
    }
}
