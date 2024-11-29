#![allow(clippy::new_without_default)]

use colored::Colorize;
use std::fs;
use std::io;
use std::io::Write;

use crate::scanner::*;

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

            if line == "exit()" {
                break;
            }

            println!(""); // extra line before next print appears

            self.run(line);
            self.had_error = false;
        }
    }

    fn run(&self, source: String) {
        let mut scanner = Scanner::new();
        scanner.run(source);
    }

    pub fn error(source: String, current: usize, message: String) {
        let (line, col) = Self::get_line_col(source.clone(), current);

        Self::print_error(source, line, col, message);
    }

    fn get_line_col(source: String, index: usize) -> (usize, usize) {
        let col: usize;
        let line: usize;

        let mut line_start = 0;

        // get the line and column number to print
        for (line_index, line_text) in source.lines().enumerate() {
            let line_end = line_start + line_text.len() + 1; // +1 for the newline char

            if index <= line_end {
                line = line_index + 1;
                col = index - line_start + 1;
                return (line, col);
            }

            line_start = line_end;
        }

        panic!("ERROR: Could not find the line ant column!");
    }

    fn print_error(source: String, line: usize, col: usize, message: String) {
        // print the error
        println!("{} {}", "Error:".bright_red().bold(), message);
        println!("{} line {}:{}", " -->".bright_magenta().bold(), line, col);

        // number of digits in the line number
        let digits_line = line.to_string().len();

        // line with error
        let err_line = source.lines().nth(line - 1).unwrap();

        // print "|" character before the erroneous line
        for i in 1..digits_line + 2 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        println!("{}", "|".bright_magenta().bold());
        io::stdout().flush().unwrap();

        // print the erroneous line
        println!(
            "{}    {}",
            format!("{} |", line).bright_magenta().bold(),
            err_line
        );

        // print "|" character after the erroneous line
        for i in 1..digits_line + 2 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        print!("{}", "|".bright_magenta().bold());
        io::stdout().flush().unwrap();

        // print spaces for the marker "^-- Here."
        for i in 0..col + 3 {
            print!(" ");
            io::stdout().flush().unwrap();
        }

        print!("{}", "^-- Here. \n\n".bright_red().bold());
        io::stdout().flush().unwrap();
    }
}
