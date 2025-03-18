#![allow(clippy::println_empty_string)]
#![allow(clippy::new_without_default)]

use std::fs;
use std::io;
use std::io::Write;

use crate::interpreter::Interpreter;
use crate::parser::Parser;

pub static mut CURRENT_FILENAME: String = String::new();

// SAFETY: no multi-threading
pub fn get_cur_filename() -> String {
    unsafe { CURRENT_FILENAME.clone() }
}
pub fn set_cur_filename(new_filename: String) {
    unsafe { CURRENT_FILENAME = new_filename }
}

pub struct Sigma {
    pub interpreter: Interpreter,
    pub had_error: bool,
}

impl Sigma {
    pub fn new() -> Self {
        let interpreter = Interpreter::new();
        Self {
            interpreter,
            had_error: false,
        }
    }

    pub fn run_file(&mut self, file_path: &str) -> Result<(), ()> {
        let file_contents = match fs::read_to_string(file_path) {
            Ok(file) => file.trim().to_string(),

            Err(_) => return Err(()),
        };

        set_cur_filename(file_path.to_string());

        self.run(&file_contents);

        if self.had_error {
            Err(())
        } else {
            Ok(())
        }
    }

    pub fn run_prompt(&mut self) {
        println!("Welcome to the brainrot programming language!\n");
        println!("Some commands:");
        println!("    :clear -> clear the screen");
        println!("    :q -> quit\n");

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
                self.had_error = true;
                break;
            }
        }
    }
}
