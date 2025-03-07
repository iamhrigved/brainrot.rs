use crate::error::{Error, ErrorKind, FatalKind::*};
use crate::parser::{Expr, Stmt};
use crate::token::{Token, TokenType::*};
use std::collections::HashMap;

type Result<T> = std::result::Result<T, Error>;

pub struct Resolver {
    scopes: Vec<HashMap<std::string::String, bool>>,
    locals: HashMap<Expr, u64>,
}

impl Resolver {
    pub fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Block(stmts) => self.resolve_block_stmt(stmts),

            _ => Ok(()),
        }
    }

    fn resolve_block_stmt(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        self.begin_scope();

        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        self.end_scope();
        Ok(())
    }

    fn resolve_var_decl_stmt(
        &mut self,
        name: &std::string::String,
        expr_opt: Option<&Expr>,
    ) -> Result<()> {
        // declare first (the variable is not ready to use)
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), false);

        if let Some(expr) = expr_opt {
            self.resolve_expr(expr)?
        }

        // define (the variable is now ready to use)
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), true);

        Ok(())
    }

    pub fn resolve_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Variable(var_token) => self.resolve_var_expr(var_token),

            _ => Ok(()),
        }
    }
    fn resolve_var_expr(&mut self, var_token: &Token) -> Result<()> {
        let Identifier(var_name) = &var_token.token_type else {
            unreachable!()
        };

        if *self.scopes.last().unwrap().get(var_name).unwrap_or(&false) {
            return Err(Error::new(
                ErrorKind::Fatal(ParseError),
                format!(
                    "Can't read local variable `{}` in its own initializer.",
                    var_name
                ),
                var_token,
            ));
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
    fn resolve_local(&mut self, expr: &Expr, var_token: &Token) {
        let Identifier(var_name) = &var_token.token_type else {
            unreachable!()
        };

        for scope in &self.scopes {
            if scope.contains_key(var_name) {
                //self.locals.insert(expr.clone(), var_token.clone());
            }
        }
    }
}
