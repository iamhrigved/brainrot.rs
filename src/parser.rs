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

PARSING EXPRESSIONS
    expression = assignment ;
    assignment = IDENTIFIER , "=" , assignment | ternary ;
    ternary    = equality , [ "?" , equality , ":" , equality ] ;
    equality   = comparison , { ( "!=" | "==" ) , comparison } ;
    comparison = term , { ( ">" | "<" | ">=" | "<=" ) , term } ;
    term       = factor , { ( "+" | "-" ) , factor } ;
    factor     = unary , { ( "*" | "/" ) , unary } ;
    unary      = ( "-" | "!" ) , unary | primary ;
    primary    = NUMBER | STRING | "true" | "false" | "nil" | "(" , expression , ")" | IDENTIFIER ;

PARSING STATEMENTS
    program     = { declaration } , EOF ;
    declaration = var_decl | statement ;
    var_decl    = "let" , IDENTIFIER , [ "=" , expression ] ;
    statement   = expr_stmt | print_stmt ;
*/

use crate::environment::Environment;
use crate::error::{Error, ErrorType};
use crate::interpreter::Interpreter;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType, TokenType::*};
use std::fmt;
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
            Value::Number(num) => num.to_string(),
            Value::String(str) => str.to_string(),
            Value::Boolean(bool) => bool.to_string(),
            Value::Nil => "Nil".to_string(),
        };
        write!(f, "{}", val)
    }
}

// ASTNodes
#[derive(Debug, Clone)]
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Literal(Value),
    Group(Box<Expr>),
    Variable(Token),
    Assign(Token, Box<Expr>),
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
            Expr::Variable(name) => format!("Var({})", name),
            Expr::Assign(name, expr) => format!("Var({}) = {}", name, expr),
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

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Vec<Expr>),
    VarDecl(String, Option<Box<Expr>>),
    Null,
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stmt = match self {
            Self::Expression(expr) => {
                let mut expr_string = "Expr(".to_string();
                expr_string.push_str(&format!("{})", expr));
                expr_string
            }
            Self::Print(expr_vec) => {
                let mut print_string = "Print(".to_string();
                expr_vec
                    .iter()
                    .for_each(|expr| print_string.push_str(&format!("{expr} ")));
                print_string.pop();
                print_string.push(')');
                print_string
            }
            Self::VarDecl(name, expr_opt) => match expr_opt {
                Some(val) => format!("Let Var({}) = {}", name, val),
                None => format!("Let Var({}) = Nil", name),
            },
            Self::Null => "".to_string(),
        };

        write!(f, "{}", stmt)
    }
}

pub struct Parser {
    scanner: Scanner, // Scanner instance
    cur_token: Token, // The first token which is not yet parsed
    token_len: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let mut scanner = Scanner::new(source.to_owned());
        let cur_token = scanner.next_token();
        let token_len = cur_token.len();

        Self {
            scanner,
            cur_token,
            token_len,
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements: Vec<Stmt> = Vec::new();
        let env = Environment::new();
        let mut interpreter = Interpreter::new(env);
        while self.cur_token.token_type != Eof {
            match self.declaration() {
                Ok(stmt) => {
                    if matches!(stmt, Stmt::Null) {
                        continue;
                    }
                    println!("stmt: {}", stmt);
                    if let Err(err) = interpreter.interpret_statement(stmt.clone()) {
                        self.report_error(err);
                    }
                    statements.push(stmt)
                }
                Err(err) => self.report_error(err),
            }
        }
        statements
    }

    fn consume_token(&mut self) -> Token {
        let next_token = self.scanner.next_token();

        let cur_token = self.cur_token.clone();
        self.cur_token = next_token;

        cur_token
    }

    fn declaration(&mut self) -> Result<Stmt, Error> {
        if self.match_next(&[NewLine]).is_some() {
            return Ok(Stmt::Null);
        }
        if self.match_next(&[Let]).is_some() {
            return self.var_declaration();
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt, Error> {
        let cur_token = self.consume_token();
        let var_name = match cur_token.token_type {
            Identifier(name) => name,
            t => {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    format!("Expected variable name. Found `{}`", t),
                    cur_token.linenum,
                ))
            }
        };

        let mut initializer = None;
        if self.match_next(&[Equal]).is_some() {
            let expr = self.expression()?;
            initializer = Some(Box::new(expr))
        }

        self.consume_token(); // consume NewLine
        Ok(Stmt::VarDecl(var_name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, Error> {
        if self.match_next(&[Print]).is_some() {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;

        self.consume_token(); // consume NewLine
        Ok(Stmt::Expression(Box::new(expr)))
    }

    fn print_statement(&mut self) -> Result<Stmt, Error> {
        let mut expressions: Vec<Expr> = Vec::new();

        loop {
            let token = &self.cur_token.token_type;

            if matches!(token, NewLine | Eof) {
                break;
            }

            let expr = self.expression()?;
            expressions.push(expr)
        }

        self.consume_token(); // consume NewLine

        Ok(Stmt::Print(expressions))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.ternary()?;

        if let Some(eq) = self.match_next(&[Equal]) {
            let value = self.assignment()?;

            match expr {
                Expr::Variable(var) => return Ok(Expr::Assign(var, Box::new(value))),
                e => {
                    return Err(Error::new(
                        ErrorType::InvalidAssignmentTarget,
                        format!("Expected variable. Found `{}`", e),
                        eq.linenum,
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn ternary(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        if self.match_next(&[Question]).is_some() {
            let if_right = self.equality()?;
            let if_wrong = match self.match_next(&[Colon]) {
                Some(token) if token.token_type == Colon => self.equality()?,
                _ => {
                    return Err(Error::new(
                        ErrorType::UnexpectedToken,
                        "Expected ':' operator!".to_string(),
                        self.cur_token.linenum,
                    ));
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

        while let Some(operator) = self.match_next(&[Slash, SlashSlash, Star]) {
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
        let next_token = self.consume_token();
        match next_token.clone().token_type {
            Boolean(val) => Ok(Expr::Literal(Value::Boolean(val))),
            Number(num) => Ok(Expr::Literal(Value::Number(num))),
            String(str) => Ok(Expr::Literal(Value::String(str))),
            Nil => Ok(Expr::Literal(Value::Nil)),

            LeftParen => {
                let expr = self.expression()?;
                let next_token = self.consume_token();
                match next_token.token_type {
                    RightParen => Ok(Expr::Group(Box::new(expr))),
                    _ => {
                        let message = "Expected ')'".to_string();
                        Err(Error::new(
                            ErrorType::SyntaxError,
                            message,
                            next_token.linenum,
                        ))
                    }
                }
            }
            Identifier(_) => Ok(Expr::Variable(next_token)),

            t if self.check_binary(&t) => Err(Error::new(
                ErrorType::SyntaxError,
                "Operand missing!".to_string(),
                next_token.linenum,
            )),

            _ => Err(Error::new(
                ErrorType::SyntaxError,
                "Expression expected!".to_string(),
                next_token.linenum,
            )),
        }
    }

    fn match_next(&mut self, matches: &[TokenType]) -> Option<Token> {
        for token_type in matches {
            if token_type == &self.cur_token.token_type {
                return Some(self.consume_token());
            }
        }
        None
    }

    fn check_binary(&self, token: &TokenType) -> bool {
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

    fn synchronize_parser(&mut self) {
        self.consume_token();

        while self.cur_token.token_type != Eof {
            if self.consume_token().token_type != NewLine {
                return;
            }
        }
    }

    fn report_error(&self, err: Error) {
        let mut line = "Somewhere, idk! ¯\\_(o _ o)_/¯".to_string();
        if err.err_linenum != 0 {
            line = self.scanner.get_line(err.err_linenum).unwrap().to_string();
        }
        err.display(line);
    }
}
