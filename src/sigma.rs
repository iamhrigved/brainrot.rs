#![allow(clippy::println_empty_string)]
#![allow(clippy::new_without_default)]

use std::fs;
use std::io;
use std::io::Write;
use std::string::String;

use crate::interpreter::Interpreter;
use crate::parser::Parser;

pub struct Sigma {
    interpreter: Interpreter,
    had_error: bool,
}

impl Sigma {
    pub fn new() -> Self {
        let interpreter = Interpreter::new();
        Self {
            interpreter,
            had_error: false,
        }
    }

    pub fn run_file(&mut self, file_path: &str) {
        if !file_path.is_empty() {
            let file_contents = fs::read_to_string(file_path)
                .expect("No such file or directory!")
                .trim()
                .to_string();

            self.run(&file_contents);
        };
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!("🗿 >> ");
            io::stdout().flush().unwrap();

            let mut line = String::new();
            let _ = io::stdin().read_line(&mut line);

            line = line.trim().to_string();

            if line == ":q" {
                println!("Bye!");
                break;
            }
            if line == ":clear" {
                println!("\x1B[2J\x1B[1;1H"); // clear the screen and position the cursor at 1, 1
                continue;
            }

            self.run(&line);
            self.had_error = false;
        }
    }

    fn run(&mut self, source: &str) {
        let mut parser = Parser::new(source);
        let stmts;

        (stmts, self.had_error) = parser.parse();

        if self.had_error {
            return;
        }

        // interpret only when there is no error
        for stmt in &stmts {
            if let Err(err) = self.interpreter.interpret_statement(stmt) {
                err.display(source.lines().nth(err.pos.0 - 1).unwrap());
                break;
            }
        }
    }
}
