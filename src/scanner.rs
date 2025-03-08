#![allow(clippy::new_without_default)]

use core::f64;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;
use std::string::String;

use crate::error::{Error, ErrorKind, ExceptionKind, FatalKind::*};
use crate::token::{Token, TokenType, TokenType::*};

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str, // Full source code
    chars: Peekable<Chars<'a>>,
    keywords: HashMap<String, TokenType>, // All keywords in Sigma.rs
    start: usize,                         // Token start index
    cur_char: usize,                      // Current position of the scanner
    token_start_colnum: usize,            // Current token's starting colnum
    pub linenum: usize,                   // Current line
    pub colnum: usize,                    // Current column
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut keywords = HashMap::new();

        let mut insert = |tt: TokenType| {
            keywords.insert(tt.to_string(), tt);
        };

        // All keywords
        insert(If);
        insert(Else);
        insert(Boolean(true));
        insert(Boolean(false));
        insert(And);
        insert(Or);
        insert(Try);
        insert(Catch);
        insert(Throw);
        insert(Let);
        insert(As);
        insert(For);
        insert(In);
        insert(While);
        insert(Continue);
        insert(Break);
        insert(Fun);
        insert(Return);
        insert(Class);
        insert(Meth);
        insert(Print);
        insert(Super);
        insert(Nil);

        // Types
        insert(TypeLiteral("Number".to_string()));
        insert(TypeLiteral("Num".to_string()));
        insert(TypeLiteral("String".to_string()));
        insert(TypeLiteral("Str".to_string()));
        insert(TypeLiteral("Boolean".to_string()));
        insert(TypeLiteral("Bool".to_string()));
        insert(TypeLiteral("List".to_string()));
        insert(TypeLiteral("Function".to_string()));
        insert(TypeLiteral("Fun".to_string()));
        insert(TypeLiteral("Range".to_string()));

        // Error
        insert(TokenType::Error);
        insert(TokenType::TypeError);
        insert(TokenType::NameError);
        insert(TokenType::IndexError);
        insert(TokenType::ValueError);
        insert(TokenType::PropertyError);

        // Brainrot
        insert(TokenType::Lit);
        insert(TokenType::Cap);
        insert(TokenType::NoCap);
        insert(TokenType::RN);
        insert(TokenType::Spit);
        insert(TokenType::FkAround);
        insert(TokenType::FindOut);
        insert(TokenType::Yeet);
        insert(TokenType::AightBet);
        insert(TokenType::FrTho);
        insert(TokenType::NahDawg);
        insert(TokenType::Be);
        insert(TokenType::Is);

        Scanner {
            source,
            chars: source.chars().peekable(),
            keywords,
            start: 0,
            cur_char: 0,
            token_start_colnum: 1,
            linenum: 1,
            colnum: 0, // 0 because it changes at character consumption
        }
    }

    pub fn next_token(&mut self) -> Token {
        match self.scan_token() {
            Ok(tt) if matches!(tt, Eof) => Token::new(tt, (self.linenum, self.colnum + 1)),

            // colnum is always at the end of the token after self.scan_token returns
            // we have to subtract its (length - 1) to get to the beginning of the token
            Ok(tt) => Token::new(tt, (self.linenum, self.token_start_colnum)),

            Err(err) => {
                err.display(self.get_line(self.linenum).unwrap());
                self.next_token()
            }
        }
    }

    fn scan_token(&mut self) -> Result<TokenType, Error> {
        let c = match self.consume_char() {
            Some(char) => char,
            None => return Ok(Eof),
        };

        self.token_start_colnum = self.colnum;

        let ret_token_type = match c {
            // If whitespace, return the next token
            c if c.is_whitespace() => {
                self.start = self.cur_char;

                if c == '\n' {
                    self.linenum += 1;
                }

                self.scan_token()
            }

            // Single character lexemes
            '(' => Ok(LeftParen),
            ')' => Ok(RightParen),
            '[' => Ok(LeftBracket),
            ']' => Ok(RightBracket),
            '{' => Ok(LeftBrace),
            '}' => Ok(RightBrace),
            ',' => Ok(Comma),
            '?' => Ok(Question),
            ':' => Ok(Colon),
            ';' => Ok(Semicolon),

            // Double character lexemes
            '+' => {
                if self.match_next_char('+') {
                    Ok(PlusPlus)
                } else if self.match_next_char('=') {
                    Ok(PlusEqual)
                } else {
                    Ok(Plus)
                }
            }
            '-' => {
                if self.match_next_char('-') {
                    Ok(MinusMinus)
                } else if self.match_next_char('=') {
                    Ok(MinusEqual)
                } else {
                    Ok(Minus)
                }
            }
            '*' => {
                if self.match_next_char('*') {
                    Ok(StarStar)
                } else if self.match_next_char('=') {
                    Ok(StarEqual)
                } else {
                    Ok(Star)
                }
            }
            '/' => {
                if self.match_next_char('/') {
                    Ok(SlashSlash)
                } else if self.match_next_char('=') {
                    Ok(SlashEqual)
                } else {
                    Ok(Slash)
                }
            }
            '%' => {
                if self.match_next_char('=') {
                    Ok(ModulusEqual)
                } else {
                    Ok(Modulus)
                }
            }
            '!' => {
                if self.match_next_char('=') {
                    Ok(BangEqual)
                } else {
                    Ok(Bang)
                }
            }
            '=' => {
                if self.match_next_char('=') {
                    Ok(EqualEqual)
                } else {
                    Ok(Equal)
                }
            }
            '<' => {
                if self.match_next_char('=') {
                    Ok(LessEqual)
                } else {
                    Ok(Less)
                }
            }
            '>' => {
                if self.match_next_char('=') {
                    Ok(GreaterEqual)
                } else {
                    Ok(Greater)
                }
            }

            // Logical Operaters
            '&' if self.match_next_char('&') => Ok(AndAnd),
            '|' if self.match_next_char('|') => Ok(PipePipe),

            // Comments
            '#' => {
                while self.peek_char() != '\n' && !self.is_at_end() {
                    self.consume_char();
                }
                self.scan_token()
            }

            // Strings
            '"' => self.manage_strings(),

            // Varargs
            '.' => {
                if self.match_next_char('.') {
                    if self.match_next_char('.') {
                        Ok(Varargs)
                    } else if self.match_next_char('=') {
                        Ok(DotDotEqual)
                    } else {
                        Ok(DotDot)
                    }
                } else {
                    Ok(Dot)
                }
            }

            // Catch all
            c => {
                // Numbers
                if c.is_ascii_digit() {
                    self.manage_numbers()
                }
                // Identifiers
                else if c.is_ascii_alphabetic() || c == '_' {
                    self.manage_identifiers()
                }
                // Default, if no valid lexeme found
                else {
                    Err(Error::new_without_token(
                        ErrorKind::Fatal(UnexpectedToken),
                        format!("Unexpected character '{}'!", c).to_string(),
                        (self.linenum, self.colnum),
                        1,
                    ))
                }
            }
        }?; // < look here

        self.start = self.cur_char;
        Ok(ret_token_type)
    }

    pub fn get_line(&self, linenum: usize) -> Option<&str> {
        self.source.lines().nth(linenum - 1)
    }

    fn is_at_end(&self) -> bool {
        self.cur_char >= self.source.len()
    }

    // Move one character ahead
    fn consume_char(&mut self) -> Option<char> {
        let char = self.chars.next()?;

        self.cur_char += 1;

        // manage colnum
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

        let next_char = self.peek_char();

        if next_char != expected_char {
            return false;
        }

        self.consume_char();
        true
    }

    // Just peeking the current unconsumed char
    fn peek_char(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        };

        *self.chars.peek().unwrap()
    }
    fn peek_next_char(&self) -> char {
        if self.is_at_end() {
            return '\0';
        };

        self.source.chars().nth(self.cur_char + 1).unwrap()
    }

    fn manage_strings(&mut self) -> Result<TokenType, Error> {
        while self.peek_char() != '"' && !self.is_at_end() {
            if self.peek_char() == '\n' {
                self.linenum += 1;
            }
            self.consume_char();
        }

        if self.is_at_end() {
            return Err(Error::new_without_token(
                ErrorKind::Fatal(UnterminatedString),
                "Expected '\"'".to_string(),
                (self.linenum, self.colnum),
                1,
            ));
        }

        // Also pass the closing "
        self.consume_char();

        let mut string_text = self.source[self.start + 1..self.cur_char - 1].to_string();

        // special characters
        string_text = string_text.replace("\\n", "\n");
        string_text = string_text.replace("\\t", "\t");
        string_text = string_text.replace("\\\\", "\\");

        Ok(String(string_text))
    }

    fn manage_numbers(&mut self) -> Result<TokenType, Error> {
        while self.peek_char().is_ascii_digit() {
            self.consume_char();
        }

        // get all the valid numbers
        if self.peek_char() == '.' && self.peek_next_char().is_ascii_digit() {
            self.consume_char(); // Consume the '.'

            while self.peek_char().is_ascii_digit() {
                self.consume_char();
            }
        }

        // if there are some alpha numeric characters after the number: return Err
        if self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
                self.consume_char();
            }
            let invalid_num = self.source[self.start..self.cur_char].to_string();
            return Err(Error::new_without_token(
                ErrorKind::Exception(ExceptionKind::TypeError),
                format!("Invalid Number: `{}`", invalid_num),
                (self.linenum, self.token_start_colnum),
                self.colnum - self.token_start_colnum + 1,
            ));
        }

        let number_str = self.source.get(self.start..self.cur_char).unwrap();

        let number = number_str.parse::<f64>().unwrap();
        Ok(Number(number))
    }

    fn manage_identifiers(&mut self) -> Result<TokenType, Error> {
        while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            self.consume_char();
        }

        let mut text = self.source[self.start..self.cur_char].to_string();

        //
        if self.peek_char() == ' ' && self.peek_next_char() != ' ' {
            self.consume_char(); // consume the ' '

            if let Some(second_half) = Token::get_second_half(&text) {
                // only stop when a space is encountered because the second half can also contain
                // some special characters like `?`
                while self.peek_char() != ' ' {
                    self.consume_char();
                }

                let next = &self.source[self.start + text.len() + 1..self.cur_char]; // +1 for the space

                if next != second_half {
                    return Err(Error::new_without_token(
                        ErrorKind::Fatal(UnexpectedToken),
                        format!(
                            "Expected `{}` after `{}`. Found `{}`.",
                            second_half, text, next
                        ),
                        (self.linenum, self.token_start_colnum + text.len() + 1),
                        next.len(),
                    ));
                }

                text = format!("{} {}", text, next);
            }
        }

        if let Some(token_ref) = self.keywords.get(&text) {
            return Ok(token_ref.to_owned());
        }

        Ok(Identifier(text.to_string()))
    }
}
