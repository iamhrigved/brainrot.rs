#![allow(clippy::new_without_default)]

use core::f64;
use std::collections::HashMap;
use std::string::String;

use crate::error::Error;
use crate::token::{Token, Token::*};

#[derive(Debug)]
pub struct Scanner {
    source: String,                       // Full source code
    pub keywords: HashMap<String, Token>, // All keywords in Sigma.rs
    pub linenum: usize,                   // Current line
    pub colnum: usize,                    // Current column
    pub token_len: usize,                 // Length of the current token
    start: usize,                         // Token start index
    cur_char: usize,                      // Current position of the scanner
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        let mut keywords = HashMap::new();

        // All keywords
        keywords.insert("if".to_string(), If);
        keywords.insert("else".to_string(), Else);
        keywords.insert("true".to_string(), Boolean(true));
        keywords.insert("false".to_string(), Boolean(false));
        keywords.insert("and".to_string(), And);
        keywords.insert("or".to_string(), Or);
        keywords.insert("let".to_string(), Let);
        keywords.insert("for".to_string(), For);
        keywords.insert("while".to_string(), While);
        keywords.insert("fun".to_string(), Fun);
        keywords.insert("return".to_string(), Return);
        keywords.insert("class".to_string(), Class);
        keywords.insert("this".to_string(), This);
        keywords.insert("print".to_string(), Print);
        keywords.insert("super".to_string(), Super);
        keywords.insert("nil".to_string(), Nil);

        Scanner {
            source,
            keywords,
            start: 0,
            cur_char: 0,
            linenum: 1,
            colnum: 0, // 0 because it changes at character consumption
            token_len: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        let c = match self.consume_char() {
            Some(char) => char,
            None => return Ok(Eof),
        };

        let ret_token = match c {
            // If whitespace, return the next token
            c if c.is_whitespace() => {
                if c == '\n' {
                    self.linenum += 1;
                }

                self.start = self.cur_char;
                self.next_token()
            }

            // Single character lexemes
            '(' => Ok(LeftParen),
            ')' => Ok(RightParen),
            '{' => Ok(LeftBrace),
            '}' => Ok(RightBrace),
            ',' => Ok(Comma),
            '.' => Ok(Dot),
            '-' => Ok(Minus),
            '+' => Ok(Plus),
            ';' => Ok(Semicolon),
            '*' => Ok(Star),
            '?' => Ok(Question),
            ':' => Ok(Colon),

            // Double character lexemes
            '!' => {
                let token = if self.match_next_char('=') {
                    BangEqual
                } else {
                    Bang
                };
                Ok(token)
            }
            '=' => {
                let token = if self.match_next_char('=') {
                    EqualEqual
                } else {
                    Equal
                };
                Ok(token)
            }
            '<' => {
                let token = if self.match_next_char('=') {
                    LessEqual
                } else {
                    Less
                };
                Ok(token)
            }
            '>' => {
                let token = if self.match_next_char('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                Ok(token)
            }

            // Comments
            '/' => {
                if self.match_next_char('/') {
                    while self.peek_char() != '\n' && !self.is_at_end() {
                        self.consume_char();
                    }
                    self.next_token()
                } else {
                    Ok(Slash)
                }
            }

            // Strings
            '"' => self.manage_strings(),

            // Catch all
            c => {
                // Numbers
                if self.is_digit(c) {
                    self.manage_numbers()
                }
                // Identifiers
                else if self.is_alpha(c) {
                    self.manage_identifiers()
                }
                // Default, if no valid lexeme found
                else {
                    Err(Error::UnexpectedToken(
                        format!("Unexpected character '{}'!", c).to_string(),
                    ))
                }
            }
        }?; // < look here

        self.token_len = ret_token.len();
        self.start = self.cur_char;
        Ok(ret_token)
    }

    pub fn get_line(&self, linenum: usize) -> Option<&str> {
        self.source.lines().nth(linenum - 1)
    }

    fn is_at_end(&self) -> bool {
        self.cur_char >= self.source.len()
    }

    // Move one character ahead
    fn consume_char(&mut self) -> Option<char> {
        let char = self.source.chars().nth(self.cur_char)?;

        self.cur_char += 1;

        if char == '\n' {
            self.colnum = 0;
        } else {
            self.colnum += 1;
        }

        Some(char)
    }

    // Matching the next char and advancing one char ahead
    fn match_next_char(&mut self, expected_char: char) -> bool {
        // if already at the end
        if self.is_at_end() {
            return false;
        }

        let next_char = self.source.chars().nth(self.cur_char).unwrap();

        if next_char != expected_char {
            return false;
        }

        self.cur_char += 1;
        true
    }

    // Just peeking the current unconsumed char
    fn peek_char(&self) -> char {
        if self.is_at_end() {
            return '\0';
        };
        self.source.chars().nth(self.cur_char).unwrap()
    }
    fn peek_next_char(&self) -> char {
        if self.cur_char >= self.source.len() {
            return '\0';
        };
        self.source.chars().nth(self.cur_char + 1).unwrap()
    }

    fn manage_strings(&mut self) -> Result<Token, Error> {
        while self.peek_char() != '"' && !self.is_at_end() {
            if self.peek_char() == '\n' {
                self.linenum += 1;
            }
            self.consume_char();
        }

        if self.is_at_end() {
            return Err(Error::UnterminatedString("'\"' expected".to_string()));
        }

        // Also pass the closing "
        self.consume_char();

        let string_text = self
            .source
            .get(self.start + 1..self.cur_char - 1)
            .unwrap()
            .to_string();

        Ok(String(string_text))
    }

    fn manage_numbers(&mut self) -> Result<Token, Error> {
        while self.is_digit(self.peek_char()) {
            self.consume_char();
        }

        if self.peek_char() == '.' && self.is_digit(self.peek_next_char()) {
            self.consume_char(); // Consume the '.'

            while self.is_digit(self.peek_char()) {
                self.consume_char();
            }
        }

        let number_str = self.source.get(self.start..self.cur_char).unwrap();

        let number = number_str.parse::<f64>().unwrap();
        Ok(Number(number))
    }

    fn manage_identifiers(&mut self) -> Result<Token, Error> {
        while self.is_alpha_numeric(self.peek_char()) {
            self.consume_char();
        }

        let text = self
            .source
            .get(self.start..self.cur_char)
            .unwrap()
            .to_string();

        if let Some(token_ref) = self.keywords.get(&text) {
            return Ok(token_ref.to_owned());
        }

        Ok(Identifier(text))
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }
    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }
    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_digit(c) || self.is_alpha(c)
    }
}
