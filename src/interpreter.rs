#![allow(clippy::new_without_default)]
#![allow(unused_must_use)]

use std::io::Write;

use crate::environment::*;
use crate::error::{Error, ErrorType};
use crate::parser::*;
use crate::token::{Token, TokenType::*};

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new(environment: Environment) -> Self {
        Self { environment }
    }
    pub fn interpret_statement(&mut self, stmt: Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Print(expr_vec) => self.interpret_print_stmt(expr_vec),
            Stmt::Expression(expr) => self.interpret_expr_stmt(*expr),
            Stmt::VarDecl(name, val_opt) => self.interpret_var_decl_stmt(name, val_opt),

            Stmt::Null => Ok(()),
        }
    }
    fn interpret_print_stmt(&mut self, expr_vec: Vec<Expr>) -> Result<(), Error> {
        for expr in expr_vec {
            let val = self.interpret_expr(expr)?;
            print!("{} ", val);
            std::io::stdout().flush();
        }

        println!("");
        Ok(())
    }
    fn interpret_expr_stmt(&mut self, expr: Expr) -> Result<(), Error> {
        let val = self.interpret_expr(expr)?;
        println!("Result: {}", val);

        Ok(())
    }
    fn interpret_var_decl_stmt(
        &mut self,
        name: std::string::String, // `String` is also a Token
        expr_opt: Option<Box<Expr>>,
    ) -> Result<(), Error> {
        let mut val_opt = None;

        if let Some(expr) = expr_opt {
            val_opt = Some(self.interpret_expr(*expr)?);
        }

        self.environment.define_var(name.to_owned(), val_opt);

        Ok(())
    }
    fn interpret_expr(&mut self, expr: Expr) -> Result<Value, Error> {
        match expr {
            Expr::Literal(val) => Ok(val.to_owned()),
            Expr::Group(expr) => self.interpret_expr(*expr),
            Expr::Unary(op, expr) => self.interpret_unary_expr(op, *expr),
            Expr::Binary(left, op, right) => self.interpret_binary_expr(*left, op, *right),
            Expr::Ternary(cond, if_right, if_wrong) => {
                self.interpret_ternary_expr(*cond, *if_right, *if_wrong)
            }
            Expr::Variable(name) => self.interpret_var_expr(name),
            Expr::Assign(name, expr) => self.interpret_assign_expr(name, *expr),
        }
    }
    fn interpret_unary_expr(&mut self, op: Token, right: Expr) -> Result<Value, Error> {
        let right_val = self.interpret_expr(right)?;

        match (right_val, op.token_type) {
            (Value::Number(num), Minus) => Ok(Value::Number(-num)),
            (Value::Boolean(bool), Bang) => Ok(Value::Boolean(!bool)),

            (val, operator) => Err(Error::new(
                ErrorType::UnsupportedOperation,
                format!(
                    "Unary operation, `{}` is not supported on `{}`.",
                    operator, val
                ),
                op.linenum,
            )),
        }
    }
    fn interpret_binary_expr(
        &mut self,
        left: Expr,
        op: Token,
        right: Expr,
    ) -> Result<Value, Error> {
        let left_val = self.interpret_expr(left)?;
        let right_val = self.interpret_expr(right)?;

        match (left_val, right_val) {
            (Value::Number(l), Value::Number(r)) => match op.token_type {
                Plus => Ok(Value::Number(l + r)),
                Minus => Ok(Value::Number(l - r)),
                Star => Ok(Value::Number(l * r)),
                Slash => Ok(Value::Number(l / r)),
                SlashSlash => Ok(Value::Number((l / r).floor())),
                BangEqual => Ok(Value::Boolean(l != r)),
                EqualEqual => Ok(Value::Boolean(l == r)),
                Greater => Ok(Value::Boolean(l > r)),
                GreaterEqual => Ok(Value::Boolean(l >= r)),
                Less => Ok(Value::Boolean(l < r)),
                LessEqual => Ok(Value::Boolean(l <= r)),
                t => Err(Error::new(
                    ErrorType::UnsupportedOperation,
                    format!("Binary operation, `{}` is not supported on Numbers.", t),
                    op.linenum,
                )),
            },
            (l_val, r_val) => match op.token_type {
                Plus => Ok(Value::String(format!("{}{}", l_val, r_val))),
                t => Err(Error::new(
                    ErrorType::UnsupportedOperation,
                    format!(
                        "Binary operation, `{}` is not supported b/w `{}` and `{}`.",
                        t, l_val, r_val
                    ),
                    op.linenum,
                )),
            },
        }
    }
    fn interpret_ternary_expr(
        &mut self,
        cond: Expr,
        if_right: Expr,
        if_wrong: Expr,
    ) -> Result<Value, Error> {
        let cond_val = self.interpret_expr(cond)?;
        let if_right_val = self.interpret_expr(if_right)?;
        let if_wrong_val = self.interpret_expr(if_wrong)?;

        match cond_val {
            Value::Boolean(true) => Ok(if_right_val),
            Value::Boolean(false) => Ok(if_wrong_val),

            t => Err(Error::new(
                ErrorType::TypeError,
                format!("Boolean expected. Found `{:?}`", t),
                0,
            )),
        }
    }
    fn interpret_var_expr(&mut self, var: Token) -> Result<Value, Error> {
        if let Ok(val_opt) = self.environment.get_var(&var.lexeme) {
            // if variable is found
            match val_opt {
                Some(val) => Ok(val),
                None => Ok(Value::Nil),
            }
        } else {
            // if not found
            Err(Error::new(
                ErrorType::VariableNotFound,
                format!("Variable `{}` not found in the current scope.", var),
                var.linenum,
            ))
        }
    }
    fn interpret_assign_expr(&mut self, var: Token, expr: Expr) -> Result<Value, Error> {
        if self.environment.get_var(&var.lexeme).is_err() {
            // if variable is not found
            return Err(Error::new(
                ErrorType::VariableNotFound,
                format!("Variable `{}` not found in the current scope.", var),
                var.linenum,
            ));
        }
        let value = self.interpret_expr(expr)?;

        self.environment.define_var(var.lexeme, Some(value.clone()));
        Ok(value)
    }
}
