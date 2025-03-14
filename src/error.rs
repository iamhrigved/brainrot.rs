use core::fmt;
use std::io;
use std::io::Write;

use crate::sigma::get_cur_filename;
use crate::token::Token;

#[derive(Debug)]
pub enum FatalKind {
    ImportError,
    SyntaxError,
    ParseError,
    UnexpectedToken,
    UnterminatedString,
    UnterminatedList,
    UnterminatedBlock,
}

#[derive(Debug)]
pub enum ExceptionKind {
    Exception,
    TypeError,
    NameError,
    IndexError,
    ValueError,
    PropertyError,
    AssertionError,
}

#[derive(Debug)]
pub enum ErrorKind {
    Exception(ExceptionKind),
    Fatal(FatalKind),
}

#[derive(Debug)]
pub struct Error {
    pub err_kind: ErrorKind,
    pub message: Option<String>,
    pub pos: (usize, usize),
    pub len: usize,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match &self.message {
            Some(msg) => msg,
            None => "",
        };
        let mut print_err = match &self.err_kind {
            ErrorKind::Fatal(fatal) => match fatal {
                FatalKind::ImportError => {
                    format!("\x1b[1;91mImportError:\x1b[0m {}", message)
                }
                FatalKind::SyntaxError => {
                    format!("\x1b[1;91mSyntaxError:\x1b[0m {}", message)
                }
                FatalKind::ParseError => {
                    format!("\x1b[1;91mParseError:\x1b[0m {}", message)
                }
                FatalKind::UnexpectedToken => {
                    format!("\x1b[1;91mUnexpectedToken:\x1b[0m {}", message)
                }
                FatalKind::UnterminatedString => {
                    format!("\x1b[1;91mUnterminatedString:\x1b[0m {}", message)
                }
                FatalKind::UnterminatedList => {
                    format!("\x1b[1;91mUnterminatedList:\x1b[0m {}", message)
                }
                FatalKind::UnterminatedBlock => {
                    format!("\x1b[1;91mUnterminatedBlock:\x1b[0m {}", message)
                }
            },
            ErrorKind::Exception(exception) => match exception {
                ExceptionKind::Exception => {
                    format!("\x1b[1;91mException:\x1b[0m {}", message)
                }
                ExceptionKind::TypeError => {
                    format!("\x1b[1;91mTypeError:\x1b[0m {}", message)
                }
                ExceptionKind::NameError => {
                    format!("\x1b[1;91mNameError:\x1b[0m {}", message)
                }
                ExceptionKind::IndexError => {
                    format!("\x1b[1;91mIndexError:\x1b[0m {}", message)
                }
                ExceptionKind::ValueError => {
                    format!("\x1b[1;91mValueError:\x1b[0m {}", message)
                }
                ExceptionKind::PropertyError => {
                    format!("\x1b[1;91mPropertyError:\x1b[0m {}", message)
                }
                ExceptionKind::AssertionError => {
                    format!("\x1b[1;91mAssertionError:\x1b[0m {}", message)
                }
            },
        };

        if message.is_empty() {
            print_err.remove(print_err.len() - 6); // remove the ':' if there is no message
        }

        write!(f, "{}", print_err)
    }
}

impl Error {
    pub fn new(err_kind: ErrorKind, message: String, token: &Token) -> Self {
        Self {
            err_kind,
            message: Some(message),
            pos: token.pos,
            len: token.to_string().len(),
        }
    }
    pub fn new_without_token(
        err_kind: ErrorKind,
        message: String,
        pos: (usize, usize),
        len: usize,
    ) -> Self {
        Self {
            err_kind,
            message: Some(message),
            pos,
            len,
        }
    }
    pub fn new_without_message(err_kind: ErrorKind, token: &Token) -> Self {
        Self {
            err_kind,
            message: None,
            pos: token.pos,
            len: token.to_string().len(),
        }
    }
    pub fn print(&self) -> String {
        format!("{}", self)
    }
    pub fn display(&self, line: &str) {
        let (linenum, colnum) = self.pos;
        let digits_line = match linenum {
            0 => 1,
            _ => linenum.ilog10() + 1,
        };
        let token_len = self.len;

        // print the err message
        println!("{}", self);
        for _ in 0..digits_line {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        println!(
            "\x1b[1;94m-->\x1b[0m {} {}:{}",
            get_cur_filename(),
            linenum,
            colnum
        );

        // print "|" character before the erroneous line
        for _ in 0..digits_line + 1 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        println!("\x1b[1;94m|\x1b[0m");

        // print the erroneous line
        println!("\x1b[1;94m{} |\x1b[0m {}", linenum, line);

        // print "|" character after the erroneous line
        for _ in 0..digits_line + 1 {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        print!("\x1b[1;94m|\x1b[0m");
        io::stdout().flush().unwrap();

        for _ in 0..colnum {
            print!(" ");
            io::stdout().flush().unwrap();
        }
        for _ in 0..token_len {
            print!("\x1b[1;91m^\x1b[0m");
            io::stdout().flush().unwrap();
        }
        println!("\x1b[1;91m Here\x1b[0m\n");
    }
}
