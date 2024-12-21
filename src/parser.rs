/*
Extended Backus-Naur Form (EBNF) Notation

CHARACTER LEGEND
      '='   -> definition
      '|'   -> alteration
      ','   -> concatenation
      ';'   -> termination
    [ ... ] -> optional (none or once)
    { ... } -> repetition (none or more)
    ( ... ) -> grouping
    " ... " -> terminal string

EXAMPLE
    digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    digit                = "0" | digit excluding zero ;

    natural number       = digit excluding zero , { digit } ;
    integer              = "0" | [ "-" ], natural number ;

BASIC PARSER
    expression = equality ;
    equality   = comparison , { ( "!=" | "==" ) , comparison } ;
    comparison = term , { ( ">" | "<" | ">=" | "<=" ) , term } ;
    term       = factor , { ( "+" | "-" ) , factor } ;
    factor     = unary , { ( "*" | "/" ) , unary } ;
    unary      = ( "-" | "!" ) , unary | primary ;
    primary    = NUMBER | STRING | "true" | "false" | "nil" | "(" , expression , ")" ;

*/

use crate::error::Error;
use crate::scanner::Scanner;
use crate::token::{Token, Token::*};
use core::fmt;
use std::string::String;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            Value::Number(val) => val.to_string(),
            Value::String(val) => val.to_string(),
            Value::Boolean(val) => val.to_string(),
            Value::Nil => "Nil".to_string(),
        };
        write!(f, "{}", val)
    }
}

#[derive(Debug, Clone)]
// ASTNodes
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Literal(Value),
    Group(Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expr = match self {
            Expr::Unary(operator, right) => {
                format!("({} {})", operator, right)
            }
            Expr::Binary(left, operator, right) => {
                format!("({} {} {})", operator, left, right)
            }
            Expr::Ternary(cond, if_right, if_wrong) => {
                format!("({} ? {} : {})", cond, if_right, if_wrong)
            }
            Expr::Literal(value) => format!("{}", value),
            Expr::Group(expr) => format!("(group {})", expr),
        };

        write!(f, "{}", expr)
    }
}

impl Expr {
    pub fn new_unary(operator: Token, right: Expr) -> Expr {
        Self::Unary(operator, Box::new(right))
    }
    pub fn new_binary(left: Expr, operator: Token, right: Expr) -> Expr {
        Self::Binary(Box::new(left), operator, Box::new(right))
    }
    pub fn new_ternary(cond: Expr, if_right: Expr, if_wrong: Expr) -> Expr {
        Self::Ternary(Box::new(cond), Box::new(if_right), Box::new(if_wrong))
    }
}

pub struct Parser {
    scanner: Scanner,        // Scanner instance
    cur_token: Token,        // The first token which is not yet parsed
    cur_pos: (usize, usize), // The location (line, col) of last parsed token
    token_len: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let mut scanner = Scanner::new(source.to_owned());

        match scanner.next_token() {
            Ok(token) => Self {
                scanner,
                cur_token: token.clone(),
                cur_pos: (1, 1),
                token_len: token.len(),
            },
            Err(err) => {
                err.display(
                    source.lines().nth(0).unwrap().to_string(),
                    (scanner.linenum, scanner.colnum),
                    0,
                );
                Self {
                    scanner,
                    cur_token: Eof,
                    cur_pos: (1, 1),
                    token_len: 3,
                }
            }
        }
    }

    pub fn parse(&mut self) {
        while self.cur_token != Eof {
            match self.expression() {
                Ok(expr) => println!("Expr: {}", expr),
                Err(error) => {
                    self.report_error(error);
                }
            }
        }
    }

    fn consume_token(&mut self) -> Token {
        // store the location and the length of the last parsed token
        self.cur_pos = (self.scanner.linenum, self.scanner.colnum);
        self.token_len = self.scanner.token_len;

        let next_token = match self.scanner.next_token() {
            Ok(token) => token,
            Err(err) => {
                self.report_error(err);
                self.consume_token()
            }
        };

        let cur_token = self.cur_token.clone();
        self.cur_token = next_token;

        cur_token
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.ternary()
    }

    fn ternary(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        if self.match_next(&[Question]).is_some() {
            let if_right = self.expression()?;
            let if_wrong = match self.match_next(&[Colon]) {
                Some(Colon) => self.expression()?,
                _ => {
                    return Err(Error::UnexpectedToken("Expected ':' operator!".to_string()));
                }
            };
            expr = Expr::new_ternary(expr, if_right, if_wrong);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.match_next(&[BangEqual, EqualEqual]) {
            let right = self.comparison()?;
            expr = Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;

        while let Some(operator) = self.match_next(&[Greater, GreaterEqual, Less, LessEqual]) {
            let right = self.term()?;
            expr = Expr::new_binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while let Some(operator) = self.match_next(&[Plus, Minus]) {
            let right = self.factor()?;
            expr = Expr::new_binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.match_next(&[Slash, Star]) {
            let right = self.unary()?;
            expr = Expr::new_binary(expr, operator, right)
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if let Some(operator) = self.match_next(&[Minus, Bang]) {
            let right = self.unary()?;
            return Ok(Expr::new_unary(operator, right));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        match self.consume_token() {
            Boolean(val) => Ok(Expr::Literal(Value::Boolean(val))),
            Number(num) => Ok(Expr::Literal(Value::Number(num))),
            String(str) => Ok(Expr::Literal(Value::String(str))),
            Nil => Ok(Expr::Literal(Value::Nil)),

            LeftParen => {
                let expr = self.expression()?;
                match self.consume_token() {
                    RightParen => Ok(Expr::Group(Box::new(expr))),
                    _ => {
                        let message = "Expected ')'".to_string();
                        Err(Error::SyntaxError(message))
                    }
                }
            }

            t if self.check_binary(&t) => Err(Error::SyntaxError("Operand missing!".to_string())),

            _ => Err(Error::SyntaxError("Expression expected!".to_string())),
        }
    }

    fn match_next(&mut self, matches: &[Token]) -> Option<Token> {
        for token_type in matches {
            if token_type == &self.cur_token {
                return Some(self.consume_token());
            }
        }
        None
    }

    fn check_binary(&self, token: &Token) -> bool {
        matches!(
            token,
            BangEqual
                | EqualEqual
                | Greater
                | GreaterEqual
                | Less
                | LessEqual
                | Plus
                | Minus
                | Slash
                | Star
        )
    }

    fn check_unary(&self, token: &Token) -> bool {
        matches!(token, Minus | Bang)
    }

    fn report_error(&self, error: Error) {
        let line = self.scanner.get_line(self.cur_pos.0).unwrap().to_string();
        error.display(line, self.cur_pos, self.token_len);
    }
}
