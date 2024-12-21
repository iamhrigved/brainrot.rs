use colored::Colorize;
use core::fmt;
use std::io;
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Error {
    SyntaxError(String),
    UnexpectedToken(String),
    UnterminatedString(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::SyntaxError(msg) => write!(f, "{} {}", "Syntax Error:".bright_red().bold(), msg),
            Error::UnexpectedToken(msg) => {
                write!(f, "{} {}", "Unexpected Token:".bright_red().bold(), msg)
            }
            Error::UnterminatedString(msg) => {
                write!(f, "{} {}", "Unterminated String:".bright_red().bold(), msg)
            }
        }
    }
}

impl Error {
    pub fn display(self, line: String, pos: (usize, usize), len: usize) {
        let digits_line = pos.0.ilog10() + 1;

        // print the error message
        println!("{}", self);
        println!(
            "{} line {}:{}",
            " -->".bright_magenta().bold(),
            pos.0,
            pos.1
        );

        // print "|" character before the erroneous line
        for _ in 0..digits_line + 1 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        println!("{}", "|".bright_magenta().bold());

        // print the erroneous line
        println!(
            "{}   {}",
            format!("{} |", pos.0).bright_magenta().bold(),
            line
        );

        // print "|" character after the erroneous line
        for _ in 0..digits_line + 1 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        print!("{}", "|".bright_magenta().bold());
        io::stdout().flush().unwrap();

        if len != 0 {
            for _ in 0..pos.1 - len + 3 {
                print!(" ");
                io::stdout().flush().unwrap();
            }
            for _ in 0..len {
                print!("{}", "^".bright_magenta().bold());
                io::stdout().flush().unwrap();
            }
            println!(" {}\n", "Here".bright_magenta().bold());
        } else {
            for _ in 0..pos.1 - len + 2 {
                print!(" ");
                io::stdout().flush().unwrap();
            }
            println!("{}\n", "^ Here".bright_magenta().bold());
        }
    }
}
