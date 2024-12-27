use colored::Colorize;
use core::fmt;
use std::io;
use std::io::Write;

pub struct Error {
    pub err_type: ErrorType,
    pub message: String,
    pub err_linenum: usize,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    SyntaxError,
    InterpreterError,
    TypeError,
    VariableNotDeclared,
    VariableNotFound,
    InvalidAssignmentTarget,
    UnexpectedToken,
    UnterminatedString,
    UnsupportedOperation,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut print_err = match self.err_type {
            ErrorType::SyntaxError => {
                format!("{} {}", "Syntax Error:".bright_red().bold(), self.message)
            }
            ErrorType::InterpreterError => {
                format!(
                    "{} {}",
                    "Interpreter Error:".bright_red().bold(),
                    self.message
                )
            }
            ErrorType::TypeError => {
                format!("{} {}", "Type Error:".bright_red().bold(), self.message)
            }
            ErrorType::VariableNotDeclared => {
                format!(
                    "{} {}",
                    "Variable Not Declared:".bright_red().bold(),
                    self.message
                )
            }
            ErrorType::VariableNotFound => {
                format!(
                    "{} {}",
                    "Variable Not Found:".bright_red().bold(),
                    self.message
                )
            }
            ErrorType::InvalidAssignmentTarget => {
                format!(
                    "{} {}",
                    "Invalid Assignment: ".bright_red().bold(),
                    self.message
                )
            }
            ErrorType::UnexpectedToken => {
                format!(
                    "{} {}",
                    "Unexpected Token:".bright_red().bold(),
                    self.message
                )
            }
            ErrorType::UnterminatedString => {
                format!(
                    "{} {}",
                    "Unterminated String:".bright_red().bold(),
                    self.message
                )
            }
            ErrorType::UnsupportedOperation => {
                format!(
                    "{} {}",
                    "Unsopported Operation:".bright_red().bold(),
                    self.message
                )
            }
        };

        print_err.push_str(&format!("\n [line {}]", self.err_linenum));

        write!(f, "{}", print_err)
    }
}

impl Error {
    pub fn new(err_type: ErrorType, message: String, err_linenum: usize) -> Self {
        Self {
            err_type,
            message,
            err_linenum,
        }
    }
    pub fn display(self, line: String) {
        let digits_line = match self.err_linenum {
            0 => 1,
            _ => self.err_linenum.ilog10() + 1,
        };

        // print the error message
        println!("\n{}", self);

        // print "|" character before the erroneous line
        for _ in 0..digits_line + 1 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        println!("{}", "|".bright_magenta().bold());

        // print the erroneous line
        println!(
            "{} {}",
            format!("{} |", self.err_linenum).bright_magenta().bold(),
            line
        );

        // print "|" character after the erroneous line
        for _ in 0..digits_line + 1 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        println!("{}", "|".bright_magenta().bold());
    }
}
