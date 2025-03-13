use std::fmt;
use std::string::String;

use crate::error::{Error, ErrorKind, ExceptionKind, FatalKind::*};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType, TokenType::*};
use crate::value::Value;

type Result<T> = std::result::Result<T, Error>;

// ASTNodes
#[derive(Debug, Clone)]
pub enum Expr {
    Increment(Option<Token>, Box<Expr>, Option<Token>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Range(Box<Expr>, Token, Box<Expr>),   // expr..expr
    Call(Box<Expr>, Vec<Expr>, Token),    // expr, args and variable token (if available) for error
    Get(Box<Expr>, Token),                // target, field
    Set(Box<Expr>, Token, Box<Expr>),     // target, field, expression
    PathGet(Box<Expr>, Token),            // the `::` operator (set)
    PathSet(Box<Expr>, Token, Box<Expr>), // the `::` operator (get)
    Closure(Vec<String>, Vec<(String, Expr)>, bool, Box<Stmt>),
    Literal(Value),
    Group(Box<Expr>),
    Variable(Token),                    // name
    Index(Box<Expr>, Box<Expr>, Token), // object, index and RightBracket for error
    List(Vec<Expr>),
    Assign(Box<Expr>, Token, Box<Expr>), // variable, operator, and expression
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let print_string = match self {
            Self::Increment(l_op_opt, expr, r_op_opt) => match (l_op_opt, r_op_opt) {
                (Some(op), None) | (None, Some(op)) => format!("{}{}", op, expr),

                (_, _) => panic!("ERROR"), // not possible
            },
            Self::Unary(op, expr) => format!("{}{}", op, expr),
            Self::Binary(l, op, r) => format!("{} {} {}", l, op, r),
            Self::Logical(l, op, r) => format!("{} {} {}", l, op, r),
            Self::Ternary(cond, then_branch, else_branch) => {
                format!("{} ? {} : {}", cond, then_branch, else_branch)
            }
            Self::Range(l, _, r) => format!("{}..{}", l, r),
            Self::Call(expr, args, _) => {
                let mut args_string = String::new();

                for arg in args {
                    args_string.push_str(&format!("{}, ", arg))
                }

                if !args.is_empty() {
                    args_string.pop();
                    args_string.pop();
                }

                format!("{}({})", expr, args_string)
            }
            Self::Get(expr, field) => format!("{}.{}", expr, field),
            Self::Set(expr, field, value) => format!("{}.{} = {}", expr, field, value),
            Self::PathGet(expr, field) => format!("{}::{}", expr, field),
            Self::PathSet(expr, field, value) => format!("{}::{} = {}", expr, field, value),
            Self::Closure(args, default_args, has_varargs, _) => {
                let mut args_string = String::new();

                for arg in args {
                    args_string.push_str(&format!("{}, ", arg))
                }

                if *has_varargs {
                    args_string.push_str(" ...")
                }

                for (arg, expr) in default_args {
                    args_string.push_str(&format!("{} = {}, ", arg, expr))
                }

                format!("fun ({}) {{ ## Body ## }}", args_string)
            }
            Self::Literal(val) => format!("{}", val),
            Self::Group(expr) => format!("({})", expr),
            Self::Variable(var_token) => format!("{}", var_token), // name
            Self::Index(expr, index, _) => format!("{}[{}]", expr, index), // object, index and RightBracket for error
            Self::List(expr_list) => {
                let mut list_string = String::new();

                for expr in expr_list {
                    list_string.push_str(&format!("{}, ", expr))
                }

                if !expr_list.is_empty() {
                    list_string.pop();
                    list_string.pop();
                }

                format!("[{}]", list_string)
            }
            Self::Assign(target, op, expr) => format!("{} {} {}", target, op, expr), // variable and expression
        };

        write!(f, "{}", print_string)
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    ClassDecl(String, Vec<Stmt>),
    // optional name, vec<var, expr>, Vec<var, default expr> has_varargs?, body
    FunDecl(
        Option<String>,
        Vec<String>,
        Vec<(String, Expr)>,
        bool,
        Box<Stmt>,
    ),
    VarDecl(String, Option<Expr>),
    // try block, and vector of catch blocks (type of exception to catch, variable, catch block)
    TryCatch(Box<Stmt>, Vec<(Token, Option<String>, Stmt)>),
    Throw(Token, Option<Expr>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Include(Token, Option<String>), // include module as mymodule
    FromInclude(Token, Vec<(Token, Option<String>)>), // from module include class1 as c1, class2 as c2
    For(Option<Box<Stmt>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    ForIn(Token, String, Expr, Box<Stmt>), // for, var, expr, stmt
    While(Expr, Box<Stmt>),
    Return(Token, Expr),
    Continue(Token),
    Break(Token),
    Block(Vec<Stmt>),
    Expression(Expr),
    Null,
}

pub struct Parser<'a> {
    scanner: Scanner<'a>, // Scanner instance
    cur_token: Token,     // The first token which is not yet parsed
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut scanner = Scanner::new(source);
        let cur_token = scanner.next_token();

        Self { scanner, cur_token }
    }

    pub fn parse(&mut self) -> (Vec<Stmt>, bool) {
        let mut statements: Vec<Stmt> = Vec::new();
        let mut had_error = false;
        while self.cur_token.token_type != Eof {
            match self.declaration() {
                Ok(stmt) => {
                    if matches!(stmt, Stmt::Null) {
                        continue;
                    }
                    statements.push(stmt)
                }
                Err(err) => {
                    self.report_error(&err);
                    had_error = true;
                }
            }
        }
        (statements, had_error)
    }

    fn consume_token(&mut self) -> Token {
        let next_token = self.scanner.next_token();

        let cur_token = self.cur_token.clone();
        self.cur_token = next_token;

        cur_token
    }

    fn declaration(&mut self) -> Result<Stmt> {
        match self.cur_token.token_type {
            Eof => {
                self.consume_token();
                Ok(Stmt::Null)
            }
            Class => self.class_declaration(),
            Fun => self.fun_declaration(),
            Let | Lit => self.var_declaration(),
            _ => self.statement(),
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `class`

        let cur_token = self.consume_token();
        let class_name = match cur_token.token_type {
            Identifier(name) => name,

            _ => {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!("Expected class name. Found `{}`", cur_token),
                    &cur_token,
                ))
            }
        };

        if self.match_next(&[LeftBrace]).is_none() {
            return Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                format!(
                    "Expected `{{` after class name. Found `{}` ",
                    self.cur_token
                ),
                &self.cur_token,
            ));
        }

        let mut stmts = Vec::new();

        while !matches!(self.cur_token.token_type, RightBrace | Eof) {
            let decl = match self.cur_token.token_type {
                Let | Lit => self.var_declaration()?,

                Meth => {
                    self.consume_token(); // consume the `meth`

                    let cur_token = self.consume_token();
                    let meth_name = match cur_token.token_type {
                        Identifier(name) => name,

                        _ => {
                            return Err(Error::new(
                                ErrorKind::Fatal(SyntaxError),
                                format!("Expected method name. Found `{}`.", cur_token),
                                &cur_token,
                            ))
                        }
                    };

                    let (params, default_params, has_varargs, body) = self.finish_fun(true)?;

                    Stmt::FunDecl(
                        Some(meth_name),
                        params,
                        default_params,
                        has_varargs,
                        Box::new(body),
                    )
                }

                Fun => {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        "Please use the `meth` keyword for method declarations.".to_string(),
                        &self.cur_token,
                    ))
                }

                _ => {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        format!(
                            "Expected variable or method declaration. Found `{}`.",
                            self.cur_token
                        ),
                        &self.cur_token,
                    ))
                }
            };

            stmts.push(decl);
        }

        if self.match_next(&[RightBrace]).is_none() {
            return Err(Error::new_without_token(
                ErrorKind::Fatal(UnterminatedBlock),
                "Expected `}` after class declaration. Found `Eof` ".to_string(),
                (self.cur_token.pos.0, self.cur_token.pos.1 - 1),
                // go back 1 char because Eof is outside of the program text
                self.cur_token.to_string().len(),
            ));
        }

        Ok(Stmt::ClassDecl(class_name, stmts))
    }

    fn fun_declaration(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `fun`

        let cur_token = self.consume_token();
        let fun_name = match cur_token.token_type {
            Identifier(name) => name,

            _ => {
                return Err(Error::new(
                    ErrorKind::Fatal(ParseError),
                    format!("Expected Function name. Found `{}`.", cur_token),
                    &cur_token,
                ))
            }
        };

        let (params, default_params, has_varargs, body_box) = self.finish_fun(false)?;

        Ok(Stmt::FunDecl(
            Some(fun_name),
            params,
            default_params,
            has_varargs,
            Box::new(body_box),
        ))
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `let` or `lit`

        let cur_token = self.consume_token();
        let var_name = match cur_token.clone().token_type {
            Identifier(name) => name,
            t => {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!("Expected variable name. Found `{}`", t),
                    &cur_token,
                ))
            }
        };

        let mut initializer = None;

        if self.match_next(&[Equal, Be]).is_some() {
            let expr = self.expression()?;
            initializer = Some(expr)
        }

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => Ok(Stmt::VarDecl(var_name, initializer)),

            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token. Found `{}`.", t),
                &t,
            )),
        }
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.cur_token.token_type {
            Include => self.include_statement(),
            From => self.from_include_statement(),
            Try | FkAround => self.try_statement(),
            If | AightBet => self.if_statement(),
            Print | Spit => self.print_statement(),
            For => self.for_statement(),
            While => self.while_statemtent(),
            Throw => self.throw_statement(),
            Return | Yeet => self.return_statement(),
            Continue => self.continue_statement(),
            Break => self.break_statement(),
            LeftBrace => self.block_statement(),

            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => Ok(Stmt::Expression(expr)),
            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token. Found `{}`.", t),
                &t,
            )),
        }
    }

    fn include_statement(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `include`

        let mut requires_var = false;

        let module_path = match &self.cur_token.token_type {
            Identifier(_) => self.consume_token(),

            String(_) => {
                requires_var = true;
                self.consume_token()
            }

            tok => {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!("Expected the name of the module. Found `{}`.", tok),
                    &self.cur_token,
                ))
            }
        };

        let mut var_opt = None;
        if self.match_next(&[As]).is_some() {
            // if cur_token is not an identifier
            if !matches!(self.cur_token.token_type, Identifier(_)) {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!(
                        "Expected variable name to store the module. Found `{}`.",
                        self.cur_token
                    ),
                    &self.cur_token,
                ));
            }

            var_opt = match self.consume_token().token_type {
                Identifier(str) => Some(str),

                _ => unreachable!(),
            }
        } else if requires_var {
            return Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!(
                    "A variable name is expected when using String as module. Found `{}`.",
                    self.cur_token
                ),
                &self.cur_token,
            ));
        }

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => Ok(Stmt::Include(module_path, var_opt)),

            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token. Found `{}`.", t),
                &t,
            )),
        }
    }

    fn from_include_statement(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `from`

        let module_path = match &self.cur_token.token_type {
            Identifier(_) | String(_) => self.consume_token(),

            tok => {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!("Expected the name of the module. Found `{}`.", tok),
                    &self.cur_token,
                ))
            }
        };

        if self.match_next(&[Include]).is_none() {
            return Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                format!(
                    "Expected `include` after module name. Found `{}`.",
                    self.cur_token,
                ),
                &self.cur_token,
            ));
        }

        let mut import_items = Vec::new();

        loop {
            let import_item_token = match &self.cur_token.token_type {
                Identifier(_) => self.consume_token(),

                _ => {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        format!(
                            "Expected import item after `include`. Found `{}`.",
                            self.cur_token
                        ),
                        &self.cur_token,
                    ))
                }
            };

            let mut var_opt = None;
            if self.match_next(&[As]).is_some() {
                // if cur_token is not an identifier
                if !matches!(self.cur_token.token_type, Identifier(_)) {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        format!(
                            "Expected variable name to store the module. Found `{}`.",
                            self.cur_token
                        ),
                        &self.cur_token,
                    ));
                }

                var_opt = match self.consume_token().token_type {
                    Identifier(str) => Some(str),

                    _ => unreachable!(),
                }
            }

            import_items.push((import_item_token, var_opt));

            if self.match_next(&[Comma]).is_none() {
                break;
            }
        }

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => {
                Ok(Stmt::FromInclude(module_path, import_items))
            }

            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token. Found `{}`.", t),
                &t,
            )),
        }
    }

    fn try_statement(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the Try or FkAround

        let try_stmt = self.statement()?;
        let mut catch_stmts = Vec::new();

        while self.match_next(&[Catch, FindOut]).is_some() {
            let error = match self.match_next(&[
                TokenType::Error,
                TokenType::TypeError,
                TokenType::NameError,
                TokenType::IndexError,
                TokenType::ValueError,
            ]) {
                Some(tok) => tok,

                None => {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        format!(
                            "Expected Exceptin type after `catch`. Found `{}`.",
                            self.cur_token
                        ),
                        &self.cur_token,
                    ))
                }
            };

            let mut variable_opt = None;

            if self.match_next(&[As]).is_some() {
                let cur_token = self.consume_token();

                variable_opt = match cur_token.token_type {
                    Identifier(name) => Some(name),

                    _ => {
                        return Err(Error::new(
                            ErrorKind::Fatal(SyntaxError),
                            "Expected Identifier to assign the Error to.".to_string(),
                            &cur_token,
                        ))
                    }
                }
            }

            let catch_stmt = self.statement()?;

            catch_stmts.push((error, variable_opt, catch_stmt));
        }

        if catch_stmts.is_empty() {
            return Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!(
                    "Expected `catch` after `try` block. Found `{}`.",
                    self.cur_token
                ),
                &self.cur_token,
            ));
        }

        Ok(Stmt::TryCatch(Box::new(try_stmt), catch_stmts))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the If or AightBet or LowkeyTho

        let cond = self.expression()?;
        let then_branch = self.statement()?;
        let mut else_branch = None;

        // else-if (i.e. fr tho)
        if self.cur_token.token_type == FrTho {
            let else_if_branch = self.if_statement()?;
            else_branch = Some(Box::new(else_if_branch))
        }
        // else or nah dawg
        else if self.match_next(&[Else, NahDawg]).is_some() {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Stmt::If(cond, Box::new(then_branch), else_branch))
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `print`

        let expr = self.expression()?;

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => Ok(Stmt::Print(expr)),
            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token. Found `{}`.", t),
                &t,
            )),
        }
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        let for_token = self.consume_token(); // consume the `for`

        if self.match_next(&[LeftParen]).is_none() {
            return self.for_in_statement(for_token);
        }

        let mut initializer = None;
        match self.cur_token.token_type {
            Let | Lit => initializer = Some(Box::new(self.var_declaration()?)),
            Semicolon | RN => {
                self.consume_token();
            }
            _ => initializer = Some(Box::new(self.expression_statement()?)),
        }

        let mut cond = None;
        match self.cur_token.token_type {
            Semicolon | RN => {
                self.consume_token();
            }
            _ => {
                // using expression_statement() instead of expression() because it will take care
                // of the Semicolon
                let expr_stmt = self.expression_statement()?;
                match expr_stmt {
                    Stmt::Expression(expr) => cond = Some(expr),
                    _ => panic!("ERROR"), // not possible
                }
            }
        }

        let mut increment = None;
        match self.cur_token.token_type {
            RightParen => (), // it is consumed in the next if statement

            // not using expression_statement() statement because there is no Semicolon
            _ => increment = Some(self.expression()?),
        }

        if self.match_next(&[RightParen]).is_none() {
            return Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                "Expected `)` after `for` clauses.".to_string(),
                &self.cur_token,
            ));
        }

        let body = Box::new(self.statement()?);

        Ok(Stmt::For(initializer, cond, increment, body))
    }

    fn for_in_statement(&mut self, for_token: Token) -> Result<Stmt> {
        let cur_token = self.consume_token();
        let var_name = match cur_token.token_type {
            Identifier(name) => name,

            _ => {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!("Expected variable. Found `{}`.", cur_token),
                    &cur_token,
                ));
            }
        };

        if self.match_next(&[In]).is_none() {
            return Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                format!(
                    "Expected `in` after variable name. Found `{}`.",
                    self.cur_token
                ),
                &self.cur_token,
            ));
        }

        let list = self.expression()?;

        let for_stmt = self.statement()?;

        Ok(Stmt::ForIn(for_token, var_name, list, Box::new(for_stmt)))
    }

    fn while_statemtent(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `while`

        let cond = self.expression()?;

        let body = self.statement()?;

        Ok(Stmt::While(cond, Box::new(body)))
    }

    fn throw_statement(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `throw`

        let exception_token = match &self.cur_token.token_type {
            TokenType::Error
            | TokenType::TypeError
            | TokenType::NameError
            | TokenType::IndexError
            | TokenType::ValueError => self.consume_token(),

            tt => {
                return Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::NameError),
                    format!("Value `{}` not found in the current scope.", tt),
                    &self.cur_token,
                ))
            }
        };

        let mut custom_message = None;
        if self.match_next(&[LeftParen]).is_some() {
            let expr = self.expression()?;
            custom_message = Some(expr);

            if self.match_next(&[RightParen]).is_none() {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!(
                        "Expected `)` after error message. Found `{}`",
                        self.cur_token
                    ),
                    &self.cur_token,
                ));
            }
        }

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => {
                Ok(Stmt::Throw(exception_token, custom_message))
            }
            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token. Found `{}`.", t),
                &t,
            )),
        }
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let ret_token = self.consume_token();
        let ret_expr = self.expression()?;

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => Ok(Stmt::Return(ret_token, ret_expr)),
            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token Found `{}`.", t),
                &t,
            )),
        }
    }

    fn continue_statement(&mut self) -> Result<Stmt> {
        let cont_token = self.consume_token();

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => Ok(Stmt::Continue(cont_token)),
            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token Found `{}`.", t),
                &t,
            )),
        }
    }

    fn break_statement(&mut self) -> Result<Stmt> {
        let break_token = self.consume_token();

        match self.consume_token() {
            t if matches!(t.token_type, Semicolon | RN) => Ok(Stmt::Break(break_token)),
            t => Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!("Expected `;` before this Token. Found `{}`.", t),
                &t,
            )),
        }
    }

    fn block_statement(&mut self) -> Result<Stmt> {
        self.consume_token(); // consume the `{`

        let mut statements: Vec<Stmt> = Vec::new();

        while !matches!(self.cur_token.token_type, RightBrace | Eof) {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }

        let cur_token = self.consume_token();
        match cur_token.token_type {
            RightBrace => Ok(Stmt::Block(statements)),
            _ => Err(Error::new(
                ErrorKind::Fatal(UnterminatedBlock),
                "Expected `}` after block.".to_string(),
                &Token::new(RightBrace, (cur_token.pos.0, cur_token.pos.1 - 1)), // go back one char
            )),
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.range()?;

        if let Some(op) = self.match_next(&[
            Equal,
            Be,
            PlusEqual,
            MinusEqual,
            StarEqual,
            SlashEqual,
            ModulusEqual,
        ]) {
            let value = self.assignment()?;

            match expr {
                Expr::Variable(_) | Expr::Index(_, _, _) => {
                    return Ok(Expr::Assign(Box::new(expr), op, Box::new(value)))
                }

                Expr::Get(target, property) => {
                    return Ok(Expr::Set(target, property, Box::new(value)))
                }

                Expr::PathGet(target, property) => {
                    return Ok(Expr::PathSet(target, property, Box::new(value)))
                }

                _ => {
                    return Err(Error::new(
                        ErrorKind::Exception(ExceptionKind::TypeError),
                        "Can't assign to an expression.".to_string(),
                        &op,
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn range(&mut self) -> Result<Expr> {
        let mut expr = self.logical_or()?;

        if let Some(token) = self.match_next(&[DotDot, DotDotEqual]) {
            let right = self.logical_or()?;

            expr = Expr::Range(Box::new(expr), token, Box::new(right));
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr> {
        let mut expr = self.logical_and()?;

        if let Some(token) = self.match_next(&[Or, PipePipe]) {
            let right = self.logical_and()?;

            expr = Expr::Logical(Box::new(expr), token, Box::new(right));
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr> {
        let mut expr = self.ternary()?;

        if let Some(token) = self.match_next(&[And, AndAnd]) {
            let right = self.ternary()?;

            expr = Expr::Logical(Box::new(expr), token, Box::new(right))
        }

        Ok(expr)
    }

    fn ternary(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        if self.match_next(&[Question]).is_some() {
            let if_right = self.equality()?;
            let if_wrong = match self.match_next(&[Colon]) {
                Some(token) if token.token_type == Colon => self.equality()?,
                _ => {
                    return Err(Error::new(
                        ErrorKind::Fatal(UnexpectedToken),
                        "Expected ':' operator!".to_string(),
                        &self.cur_token,
                    ));
                }
            };
            expr = Expr::Ternary(Box::new(expr), Box::new(if_right), Box::new(if_wrong));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.match_next(&[BangEqual, EqualEqual, Is]) {
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while let Some(operator) = self.match_next(&[Greater, GreaterEqual, Less, LessEqual]) {
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while let Some(operator) = self.match_next(&[Plus, Minus]) {
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.power()?;

        while let Some(operator) = self.match_next(&[Modulus, Slash, SlashSlash, Star]) {
            let right = self.power()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }
        Ok(expr)
    }

    fn power(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.match_next(&[StarStar]) {
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some(operator) = self.match_next(&[Minus, Bang]) {
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.increment()
    }

    fn increment(&mut self) -> Result<Expr> {
        // for prefix increment/decrement
        if let Some(operator) = self.match_next(&[PlusPlus, MinusMinus]) {
            let expr = self.primary()?;
            match expr {
                Expr::Variable(_) | Expr::Index(_, _, _) => {
                    return Ok(Expr::Increment(Some(operator), Box::new(expr), None))
                }
                _ => {
                    return Err(Error::new(
                        ErrorKind::Fatal(ParseError),
                        format!(
                            "Can't perform the `{}` operator on anything other than a Variable.",
                            operator
                        ),
                        &operator,
                    ))
                }
            }
        };

        let expr = self.primary()?;

        // for postfix increment/decrement
        if let Some(operator) = self.match_next(&[PlusPlus, MinusMinus]) {
            match expr {
                Expr::Variable(_) | Expr::Index(_, _, _) => {
                    return Ok(Expr::Increment(None, Box::new(expr), Some(operator)));
                }
                _ => {
                    return Err(Error::new(
                        ErrorKind::Fatal(ParseError),
                        format!(
                            "Can't perform the `{}` operator on anything other than a Variable.",
                            operator
                        ),
                        &operator,
                    ))
                }
            }
        };

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr> {
        let mut expr = self.literal()?;

        loop {
            match self.cur_token.token_type {
                // function call
                LeftParen => {
                    self.consume_token();

                    expr = self.finish_call(expr)?;
                }

                // index
                LeftBracket => {
                    self.consume_token();

                    let index = self.expression()?;

                    if let Some(rb) = self.match_next(&[RightBracket]) {
                        expr = Expr::Index(Box::new(expr), Box::new(index), rb);
                    } else {
                        return Err(Error::new(
                            ErrorKind::Fatal(ParseError),
                            format!("Expected `]` after index. Found `{}`.", self.cur_token),
                            &self.cur_token,
                        ));
                    }
                }

                // get
                Dot => {
                    self.consume_token();

                    let property = match self.cur_token.token_type {
                        Identifier(_) => self.consume_token(),
                        _ => {
                            return Err(Error::new(
                                ErrorKind::Fatal(SyntaxError),
                                format!(
                                    "Expected field name after `.`. Found `{}`.",
                                    self.cur_token
                                ),
                                &self.cur_token,
                            ))
                        }
                    };

                    expr = Expr::Get(Box::new(expr), property);
                }

                // class get
                ColonColon => {
                    self.consume_token();

                    let property = match self.cur_token.token_type {
                        Identifier(_) => self.consume_token(),
                        _ => {
                            return Err(Error::new(
                                ErrorKind::Fatal(SyntaxError),
                                format!(
                                    "Expected field name after `.`. Found `{}`.",
                                    self.cur_token
                                ),
                                &self.cur_token,
                            ))
                        }
                    };

                    expr = Expr::PathGet(Box::new(expr), property);
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn literal(&mut self) -> Result<Expr> {
        let cur_token = self.consume_token();
        match cur_token.token_type {
            // Literal
            Boolean(val) => Ok(Expr::Literal(Value::Boolean(val))),
            NoCap => Ok(Expr::Literal(Value::Boolean(true))),
            Cap => Ok(Expr::Literal(Value::Boolean(false))),
            Number(num) => Ok(Expr::Literal(Value::Number(num))),
            String(str) => Ok(Expr::Literal(Value::String(str))),
            TypeLiteral(type_name) => Ok(Expr::Literal(Value::Type(type_name))),
            Nil => Ok(Expr::Literal(Value::Nil)),

            // Group
            LeftParen => {
                let expr = self.expression()?;
                let next_token = self.consume_token();
                match next_token.token_type {
                    RightParen => Ok(Expr::Group(Box::new(expr))),
                    _ => Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        "Expected ')'.".to_string(),
                        &next_token,
                    )),
                }
            }

            // List
            LeftBracket => {
                let mut list = Vec::new();
                if self.match_next(&[RightBracket]).is_some() {
                    return Ok(Expr::List(list));
                }
                loop {
                    if self.cur_token.token_type == Eof {
                        return Err(Error::new(
                            ErrorKind::Fatal(UnterminatedList),
                            "Expected `]` after List.".to_string(),
                            &self.cur_token,
                        ));
                    }

                    let item = self.expression()?;

                    if self.match_next(&[RightBracket]).is_some() {
                        list.push(item);
                        break;
                    }

                    if self.match_next(&[Comma]).is_none() {
                        return Err(Error::new(
                            ErrorKind::Fatal(UnexpectedToken),
                            "Expected `,` after List element.".to_string(),
                            &self.cur_token,
                        ));
                    }

                    list.push(item);
                }
                Ok(Expr::List(list))
            }

            // Variable
            Identifier(_) => Ok(Expr::Variable(cur_token)),

            // Closure
            Fun => {
                let (params, default_params, has_varargs, stmt) = self.finish_fun(true)?;
                Ok(Expr::Closure(
                    params,
                    default_params,
                    has_varargs,
                    Box::new(stmt),
                ))
            }

            ref tt if self.check_binary(tt) => Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                "Operand missing!".to_string(),
                &cur_token,
            )),

            ref tt if matches!(tt, Catch | FindOut) => Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                format!("Can't use `{}` without an associated `try` block.", tt),
                &cur_token,
            )),

            _ => Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                "Expression expected!".to_string(),
                &cur_token,
            )),
        }
    }

    // HELPER FUNCTIONS
    fn match_next(&mut self, matches: &[TokenType]) -> Option<Token> {
        for token_type in matches {
            if token_type == &self.cur_token.token_type {
                return Some(self.consume_token());
            }
        }
        None
    }

    fn synchronize_parser(&mut self) {
        while self.cur_token.token_type != Eof {
            match self.cur_token.token_type {
                Try | FkAround | Catch | FindOut | If | AightBet | Print | Spit | For | While
                | Return | Yeet | Continue | Break => return,
                Semicolon | RN => {
                    self.consume_token();
                    return;
                }
                _ => (),
            };

            self.consume_token();
        }
    }

    pub fn report_error(&mut self, err: &Error) {
        let line = self.scanner.get_line(err.pos.0).unwrap();
        err.display(line);
        self.synchronize_parser();
    }

    fn check_binary(&self, token: &TokenType) -> bool {
        matches!(
            token,
            BangEqual
                | EqualEqual
                | Is
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
    /*
       1. normal function:
           fun name(a, b, c = 1) { /* */ } --> c has a default param

       2. normal closure:
           fun (a, b, c ...) { /* */ } --> c is the vararg list

       3. shorthand closure:
           fun (a, b, c): a + b + c --> returns a + b + c
                       AND
           fun a: a * 2 --> parenthesis for single param not required
    */

    fn finish_fun(
        &mut self,
        is_closure: bool,
    ) -> Result<(Vec<String>, Vec<(String, Expr)>, bool, Stmt)> {
        let mut single_param = false;

        if self.match_next(&[LeftParen]).is_none() {
            // if the function is not a closure
            if !is_closure {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    "Expected `(` after function name.".to_string(),
                    &self.cur_token,
                ));
            }
            // if the function is a closure
            else {
                single_param = true;
            }
        }

        let mut params = Vec::new();
        let mut default_params = Vec::new();
        let mut has_varargs = false;

        if self.match_next(&[RightParen]).is_some() && !single_param {
            let stmt = self.statement()?;
            return Ok((params, default_params, has_varargs, stmt));
        }

        if single_param {
            let var_token = self.consume_token();

            let Identifier(var_name) = var_token.token_type else {
                return Err(Error::new(
                    ErrorKind::Fatal(ParseError),
                    format!("Expected parameter name. Found `{}`.", var_token),
                    &var_token,
                ));
            };

            params.push(var_name);
        }

        #[allow(clippy::while_immutable_condition)]
        while !single_param {
            let var_token = self.consume_token();

            if !matches!(var_token.token_type, Identifier(_)) {
                return Err(Error::new(
                    ErrorKind::Fatal(ParseError),
                    format!("Expected parameter name. Found `{}`.", var_token),
                    &var_token,
                ));
            }

            // handle default parameter
            // if there is already a default parameter declared, and the next parameter is a regular one
            if !default_params.is_empty() && !matches!(self.cur_token.token_type, Equal | Be) {
                // normal error message
                let mut err_message =
                    "Parameters with a default should follow parameters without a default.";
                let mut err_token = var_token;

                // error message when the next token is Varargs
                if let Some(varargs) = self.match_next(&[Varargs]) {
                    err_message = "Can't use varargs when you have a default parameter.";
                    err_token = varargs;
                }

                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    err_message.to_string(),
                    &err_token,
                ));
            }

            let Identifier(var_name) = var_token.token_type else {
                unreachable!();
            };

            if self.match_next(&[Equal, Be]).is_some() {
                let expr = self.expression()?;

                default_params.push((var_name, expr));
            } else {
                // handle regular parameter
                params.push(var_name);
            }

            // handle varargs
            if let Some(varargs) = self.match_next(&[Varargs]) {
                if !default_params.is_empty() {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        "Can't use varargs when you have a default parameter.".to_string(),
                        &varargs,
                    ));
                }

                has_varargs = true;
            }

            if let Some(comma) = self.match_next(&[Comma]) {
                if has_varargs {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        "Varargs parameter should be positioned last.".to_string(),
                        &comma,
                    ));
                }
            } else {
                break;
            }
        }

        if self.match_next(&[RightParen]).is_none() && !single_param {
            return Err(Error::new(
                ErrorKind::Fatal(SyntaxError),
                format!(
                    "Expected `)` after function parameters. Found `{}`.",
                    self.cur_token
                ),
                &self.cur_token,
            ));
        }

        match &self.cur_token {
            // skip if leftbrace
            tok if matches!(tok.token_type, LeftBrace) => (),

            // if colon
            tok if matches!(tok.token_type, Colon) => {
                self.consume_token(); // consume the colon

                // token after colon can't be leftbrace
                if matches!(self.cur_token.token_type, LeftBrace) {
                    return Err(Error::new(
                        ErrorKind::Fatal(SyntaxError),
                        "Block statement is not allowed in shorthand `:` syntax.".to_string(),
                        &self.cur_token,
                    ));
                }
            }

            // if anything else
            tok => {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    format!(
                        "Expected `{{` or `:` after parameter list. Found `{}`.",
                        tok
                    ),
                    tok,
                ))
            }
        }

        let stmt = if matches!(self.cur_token.token_type, LeftBrace) {
            if single_param {
                return Err(Error::new(
                    ErrorKind::Fatal(SyntaxError),
                    "Block statement is not allowed. Please use parenthesis around the parameter name.".to_string(),
                    &self.cur_token
                ));
            }

            self.statement()?
        } else {
            // if NOT leftbrace, then an expression will be expected
            Stmt::Expression(self.expression()?)
        };

        Ok((params, default_params, has_varargs, stmt))
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = Vec::new();

        // if the function is called directly like `print()`
        // or `like some_instance.print()`, get the `print` token
        let mut callee_token_opt = match &callee {
            Expr::Variable(token) => Some(token.clone()),
            Expr::Get(_, token) => Some(token.clone()),

            _ => None,
        };

        if self.cur_token.token_type != RightParen {
            arguments.push(self.expression()?);
            while self.match_next(&[Comma]).is_some() {
                if arguments.len() >= 255 {
                    return Err(Error::new(
                        ErrorKind::Fatal(ParseError),
                        "Can't have more than 255 arguments in a function!".to_string(),
                        &self.cur_token,
                    ));
                }
                arguments.push(self.expression()?);
            }
        }

        if let Some(rp) = self.match_next(&[RightParen]) {
            // if the function is NOT called like `print()`, get the RightParen
            if callee_token_opt.is_none() {
                callee_token_opt = Some(rp)
            }
            Ok(Expr::Call(
                Box::new(callee),
                arguments,
                callee_token_opt.unwrap(),
            ))
        } else {
            Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!(
                    "Expected `)` after function arguments. Found `{}`.",
                    self.cur_token
                ),
                &self.cur_token,
            ))
        }
    }
}
