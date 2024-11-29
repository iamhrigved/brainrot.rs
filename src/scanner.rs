#![allow(clippy::new_without_default)]

// TODO: IMPLEMENT LINE AND COL INSTEAD OF LINE AND CURRENT

use core::f64;
use std::collections::HashMap;
use std::string::String;

use crate::sigma::Sigma;
use crate::tokens::TokenType::*;
use crate::tokens::*;

#[derive(Debug)]
pub struct Scanner {
    source: String,                       // Full source code
    tokens: Vec<Token>,                   // List of all the tokens
    keywords: HashMap<String, TokenType>, // All keywords in Sigma.rs
    start: usize,                         // Token start index
    current: usize,                       // Current position of the scanner
    line: usize,                          // Current line
}

impl Scanner {
    pub fn new() -> Scanner {
        let mut keywords = HashMap::new();

        // All keywords
        keywords.insert("if".to_string(), If);
        keywords.insert("else".to_string(), Else);
        keywords.insert("true".to_string(), True);
        keywords.insert("false".to_string(), False);
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
            source: "".to_string(),
            tokens: vec![],
            keywords,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn run(&mut self, source: String) {
        self.source = source;
        self.scan_tokens();
    }

    fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        let last_token = Token::new(Eof, "".to_string(), self.line);
        self.tokens.push(last_token);
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            // Whitespace
            ' ' | '\r' | '\t' => (),
            '\n' => self.line += 1,

            // Single character lexemes
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '-' => self.add_token(Minus),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),

            // Double character lexemes
            '!' => {
                let token = if self.match_next('=') {
                    BangEqual
                } else {
                    Bang
                };
                self.add_token(token)
            }
            '=' => {
                let token = if self.match_next('=') {
                    EqualEqual
                } else {
                    Equal
                };
                self.add_token(token)
            }
            '<' => {
                let token = if self.match_next('=') {
                    LessEqual
                } else {
                    Less
                };
                self.add_token(token)
            }
            '>' => {
                let token = if self.match_next('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                self.add_token(token)
            }

            // Comments
            '/' => {
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Slash)
                }
            }

            // Strings
            '"' => self.manage_strings(),

            // Catch all
            c => {
                // Numbers
                if self.is_digit(c) {
                    self.manage_numbers();
                }
                // Identifiers
                else if self.is_alpha(c) {
                    self.manage_identifiers();
                }
                // Default, if no lexeme found
                else {
                    self.report_error(format!("Unexpected character '{}'!", c).to_string());
                }
            }
        }
    }

    // To add tokens whose Literal type is NullValue
    fn add_token(&mut self, token_type: TokenType) {
        let text = self
            .source
            .get(self.start..self.current)
            .unwrap()
            .to_string();

        self.tokens.push(Token::new(token_type, text, self.line));
    }

    fn report_error(&self, message: String) {
        Sigma::error(self.source.clone(), self.current - 1, message);
    }

    // Move one character ahead
    fn advance(&mut self) -> char {
        let char = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        char
    }

    // Matching the next char and advancing one char ahead
    fn match_next(&mut self, expected_char: char) -> bool {
        // if already at the end
        if self.is_at_end() {
            return false;
        }

        let next_char = self.source.chars().nth(self.current).unwrap();

        if next_char != expected_char {
            return false;
        }

        self.current += 1;
        true
    }

    // Just peeking the current unconsumed char
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        };
        self.source.chars().nth(self.current).unwrap()
    }
    fn peek_next(&self) -> char {
        if self.current >= self.source.len() {
            return '\0';
        };
        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn manage_strings(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.report_error("Unterminated string!".to_string());
            return;
        }

        // Also pass the closing "
        self.advance();

        let string_text = self
            .source
            .get(self.start + 1..self.current - 1)
            .unwrap()
            .to_string();
        self.add_token(String(string_text))
    }

    fn manage_numbers(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance(); // Consume the '.'

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let number: f64 = self
            .source
            .get(self.start..self.current)
            .unwrap()
            .parse()
            .unwrap();

        self.add_token(Number(number));
    }

    fn manage_identifiers(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = self
            .source
            .get(self.start..self.current)
            .unwrap()
            .to_string();

        if let Some(token_type) = self.keywords.get(&text) {
            self.add_token(token_type.clone());
            return;
        }

        self.add_token(Identifier(text));
    }

    fn is_digit(&self, c: char) -> bool {
        '0' <= c && c <= '9'
    }
    fn is_alpha(&self, c: char) -> bool {
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
    }
    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_digit(c) || self.is_alpha(c)
    }
}
