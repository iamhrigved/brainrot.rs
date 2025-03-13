#![allow(clippy::new_without_default)]
#![allow(unused_must_use)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::string::String;

use crate::environment::Environment;
use crate::error::{Error, ErrorKind, ExceptionKind, FatalKind::*};
use crate::libs::{Library, NativeLib};
use crate::parser::{Expr, Stmt};
use crate::sigma::Sigma;
use crate::token::{Token, TokenType, TokenType::*};
use crate::value::native_fun::NativeFun;
use crate::value::{
    module::Module, native_class::NativeClass, sigma_class::SigmaClass, sigma_fun::SigmaFun, Value,
};

type Result<T> = std::result::Result<T, Error>;

pub enum ControlFlow {
    Break,
    Continue,
    Return(Value),
}

pub struct Interpreter {
    // pub for sigma_fun
    pub environment: Rc<RefCell<Environment>>,

    native_libs: Vec<Library>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
            native_libs: Library::init_libs(),
        }
    }

    pub fn from(environment: Rc<RefCell<Environment>>) -> Self {
        Self {
            environment: Environment::new_from(environment),
            native_libs: Library::init_libs(),
        }
    }

    // INTERPRETING STATEMENTS
    pub fn interpret_statement(&mut self, stmt: &Stmt) -> Result<Option<ControlFlow>> {
        match stmt {
            Stmt::Print(expr) => self.interpret_print_stmt(expr),
            Stmt::Expression(expr) => self.interpret_expr_stmt(expr),
            Stmt::ClassDecl(name, declarations) => self.interpret_class_decl(name, declarations),
            Stmt::FunDecl(_, _, _, _, _) => self.interpret_fun_decl(stmt),
            Stmt::VarDecl(name, expr_opt) => self.interpret_var_decl(name, expr_opt.as_ref()),
            Stmt::TryCatch(try_stmt, catch_stmts) => {
                self.interpret_try_stmt(try_stmt.as_ref(), catch_stmts)
            }
            Stmt::Include(path_tok, var_opt) => {
                self.interpret_include_stmt(path_tok, var_opt.as_ref())
            }
            Stmt::FromInclude(path_tok, import_items) => {
                self.interpret_from_include_stmt(path_tok, import_items)
            }
            Stmt::If(cond, then_branch, else_branch_opt) => self.interpret_if_stmt(
                cond,
                then_branch.as_ref(),
                else_branch_opt.as_ref().map(|m| m.as_ref()),
            ),
            Stmt::For(initializer_opt, cond_opt, increment_opt, body) => self.interpret_for_stmt(
                initializer_opt.as_ref().map(|m| m.as_ref()),
                cond_opt.as_ref(),
                increment_opt.as_ref(),
                body.as_ref(),
            ),
            Stmt::ForIn(for_token, var_token, expr, body) => {
                self.interpret_for_in_stmt(for_token, var_token, expr, body.as_ref())
            }
            Stmt::While(cond, body) => self.interpret_while_stmt(cond, body.as_ref()),
            Stmt::Throw(exception_token, expr_opt) => {
                self.interpret_throw_stmt(exception_token, expr_opt.as_ref())
            }
            Stmt::Return(token, expr) => self.interpret_return_stmt(token, expr),
            Stmt::Continue(token) => self.interpret_continue_stmt(token),
            Stmt::Break(token) => self.interpret_break_stmt(token),
            Stmt::Block(stmt_vec) => self.interpret_block_stmt(stmt_vec),

            Stmt::Null => Ok(None),
        }
    }
    fn interpret_print_stmt(&mut self, expr: &Expr) -> Result<Option<ControlFlow>> {
        println!("{} ", self.interpret_expression(expr)?);

        Ok(None)
    }
    fn interpret_expr_stmt(&mut self, expr: &Expr) -> Result<Option<ControlFlow>> {
        self.interpret_expression(expr)?;

        Ok(None)
    }
    fn interpret_class_decl(
        &mut self,
        name: &String,
        declarations: &Vec<Stmt>,
    ) -> Result<Option<ControlFlow>> {
        let mut properties = HashMap::with_capacity(4);

        // new environment for methods
        let new_env = Environment::new();

        for decl in declarations {
            match decl {
                Stmt::VarDecl(name, expr_opt) => {
                    let mut val = Value::Nil;

                    if let Some(expr) = expr_opt {
                        val = self.interpret_expression(expr)?
                    };

                    properties.insert(name.to_owned(), val);
                }

                Stmt::FunDecl(Some(name), _, _, _, _) => {
                    let fun = SigmaFun::new(decl.clone(), Rc::clone(&new_env), self)?;

                    let fun_rc = Rc::new(fun);

                    properties.insert(name.to_owned(), Value::Function(fun_rc));
                }

                _ => unreachable!(),
            }
        }

        let class = SigmaClass::new(name.to_owned(), properties);
        let class_rc = Rc::new(class);

        // define `Me` type in the method environment (we have the RefCell mutable access)
        let mut env_mut = new_env.borrow_mut();
        env_mut.define_var("Me".to_string(), Some(Value::Class(Rc::clone(&class_rc))));
        env_mut.define_var(name.to_string(), Some(Value::Class(Rc::clone(&class_rc))));

        self.environment
            .borrow_mut()
            .define_var(name.to_owned(), Some(Value::Class(class_rc)));

        Ok(None)
    }
    fn interpret_fun_decl(&mut self, declaration: &Stmt) -> Result<Option<ControlFlow>> {
        let name = match declaration {
            Stmt::FunDecl(Some(name), _, _, _, _) => name.clone(),

            _ => panic!("ERROR"), // not possible
        };

        let fun = SigmaFun::new(declaration.to_owned(), Rc::clone(&self.environment), self)?;

        self.environment
            .borrow_mut()
            .define_var(name, Some(Value::Function(Rc::new(fun))));

        Ok(None)
    }
    fn interpret_var_decl(
        &mut self,
        name: &String, // because `String` is also a Token
        expr_opt: Option<&Expr>,
    ) -> Result<Option<ControlFlow>> {
        let mut val_opt = None;

        if let Some(expr) = expr_opt {
            val_opt = Some(self.interpret_expression(expr)?);
        }

        // update name only if the instance is owned (and not just a reference)
        match &val_opt {
            Some(Value::Instance(instance_rc)) if Rc::strong_count(instance_rc) == 1 => {
                instance_rc.borrow_mut().update_name(name.to_owned())
            }
            _ => (),
        }

        self.environment = Environment::new_block(Rc::clone(&self.environment));

        self.environment
            .borrow_mut()
            .define_var(name.to_owned(), val_opt);

        Ok(None)
    }
    fn interpret_try_stmt(
        &mut self,
        try_stmt: &Stmt,
        catch_stmts: &Vec<(Token, Option<String>, Stmt)>,
    ) -> Result<Option<ControlFlow>> {
        let err = match self.interpret_statement(try_stmt) {
            Ok(cf) => return Ok(cf),

            Err(err) if matches!(err.err_kind, ErrorKind::Exception(_)) => err,

            Err(err) => return Err(err),
        };

        for (exception_type, var_opt, stmt) in catch_stmts {
            match (&exception_type.token_type, &err.err_kind) {
                (TokenType::Error, ErrorKind::Exception(_))
                | (TokenType::TypeError, ErrorKind::Exception(ExceptionKind::TypeError))
                | (TokenType::NameError, ErrorKind::Exception(ExceptionKind::NameError))
                | (TokenType::IndexError, ErrorKind::Exception(ExceptionKind::IndexError))
                | (TokenType::ValueError, ErrorKind::Exception(ExceptionKind::ValueError))
                | (TokenType::PropertyError, ErrorKind::Exception(ExceptionKind::PropertyError)) => {
                    let old_env = Rc::clone(&self.environment);

                    self.environment = Environment::new_block(Rc::clone(&old_env));

                    // define variable if possible
                    if let Some(var_name) = var_opt {
                        self.environment
                            .borrow_mut()
                            .define_var(var_name.to_string(), Some(Value::Exception(err.print())));
                    }

                    let result = self.interpret_statement(stmt);

                    // restore the old environment
                    self.environment = old_env;

                    return result;
                }

                _ => (),
            }
        }

        Ok(None)
    }
    fn interpret_include_stmt(
        &mut self,
        path_token: &Token,
        var_name: Option<&String>,
    ) -> Result<Option<ControlFlow>> {
        let (mod_path, mod_name) = match &path_token.token_type {
            // if string: mod_path is the string and the var_name is already a Some
            String(str) => (str.to_owned(), var_name.unwrap().to_owned()),

            // if identifier: mod_path is `identifier.rot` and var_name may be present
            Identifier(str) => (
                format!("{}.rot", str),
                var_name.cloned().unwrap_or(str.to_owned()),
            ),

            _ => unreachable!(),
        };

        let mod_val = self.run_module(&mod_path, &mod_name, None, path_token)?;

        self.environment
            .borrow_mut()
            .define_var(mod_name.to_owned(), Some(mod_val));

        Ok(None)
    }
    fn interpret_from_include_stmt(
        &mut self,
        path_token: &Token,
        import_items: &Vec<(Token, Option<String>)>,
    ) -> Result<Option<ControlFlow>> {
        let (mod_path, mod_name) = match &path_token.token_type {
            String(str) => (str.to_owned(), str.to_owned()),

            Identifier(name) => (format!("{}.rot", name), name.to_owned()),

            _ => unreachable!(),
        };

        for (item_tok, var_opt) in import_items {
            let item_name = match &item_tok.token_type {
                Identifier(str) => str.to_owned(),
                _ => unreachable!(),
            };

            let item_val = self.run_module(&mod_path, &mod_name, Some(item_tok), path_token)?;

            // define the variable with the name item_name or var_opt (if given)
            self.environment
                .borrow_mut()
                .define_var(var_opt.clone().unwrap_or(item_name), Some(item_val));
        }

        Ok(None)
    }
    fn interpret_if_stmt(
        &mut self,
        cond: &Expr,
        then_branch: &Stmt,
        else_branch_opt: Option<&Stmt>,
    ) -> Result<Option<ControlFlow>> {
        let cond_val = self.interpret_expression(cond)?;

        if self.is_truthy(&cond_val) {
            return self.interpret_statement(then_branch);
        } else if let Some(else_branch) = else_branch_opt {
            return self.interpret_statement(else_branch);
        }

        Ok(None)
    }
    fn interpret_for_stmt(
        &mut self,
        initializer_opt: Option<&Stmt>,
        cond_opt: Option<&Expr>,
        increment_opt: Option<&Expr>,
        body: &Stmt,
    ) -> Result<Option<ControlFlow>> {
        let old_env = Rc::clone(&self.environment);
        self.environment = Environment::new_loop(Rc::clone(&old_env));

        if let Some(initializer) = initializer_opt {
            if let Some(cf) = self.interpret_statement(initializer)? {
                self.environment = old_env;

                return Ok(Some(cf));
            }
        }

        loop {
            if let Some(cond) = cond_opt {
                let cond_val = self.interpret_expression(cond)?;
                if !self.is_truthy(&cond_val) {
                    break;
                }
            }
            match self.interpret_statement(body)? {
                Some(ControlFlow::Continue) => (),
                Some(ControlFlow::Break) => break,

                Some(cf) => return Ok(Some(cf)),
                _ => (),
            }
            if let Some(increment) = increment_opt {
                self.interpret_expression(increment)?;
            }
        }

        self.environment = old_env;
        Ok(None)
    }
    fn interpret_for_in_stmt(
        &mut self,
        for_token: &Token,
        var_name: &str,
        expr: &Expr,
        body: &Stmt,
    ) -> Result<Option<ControlFlow>> {
        let list = match self.interpret_expression(expr)? {
            Value::List(list_val) => list_val.borrow().clone().into_iter(),

            Value::Range(num1, num2) => {
                if num1 <= num2 {
                    (num1..num2)
                        .map(|num| Value::Number(num as f64))
                        .collect::<Vec<Value>>()
                        .into_iter()
                } else {
                    (num2..num1)
                        .rev()
                        .map(|num| Value::Number(num as f64))
                        .collect::<Vec<Value>>()
                        .into_iter()
                }
            }

            v => {
                return Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    format!("Iterator should be a List. Found {:?}.", v),
                    for_token,
                ))
            }
        };

        let old_env = Rc::clone(&self.environment);
        self.environment = Environment::new_loop(Rc::clone(&old_env));

        for item in list {
            // since the variable name is the same, we can redefine it in the same environment
            self.environment
                .borrow_mut()
                .define_var(var_name.to_owned(), Some(item));

            match self.interpret_statement(body)? {
                // consume the break or the continue control statement
                Some(ControlFlow::Break) => break,
                Some(ControlFlow::Continue) => (),

                Some(cf) => return Ok(Some(cf)),
                _ => (),
            }
        }
        self.environment = old_env;

        Ok(None)
    }
    fn interpret_while_stmt(&mut self, cond: &Expr, body: &Stmt) -> Result<Option<ControlFlow>> {
        let mut cond_val = self.interpret_expression(cond)?;

        let old_env = Rc::clone(&self.environment);
        self.environment = Environment::new_loop(Rc::clone(&old_env));

        while self.is_truthy(&cond_val) {
            match self.interpret_statement(body)? {
                Some(ControlFlow::Break) => break,
                Some(ControlFlow::Continue) => (), // consume the Continue command
                Some(cf) => return Ok(Some(cf)),
                None => (),
            }
            cond_val = self.interpret_expression(cond)?;
        }

        self.environment = old_env;

        Ok(None)
    }
    fn interpret_block_stmt(&mut self, stmts: &Vec<Stmt>) -> Result<Option<ControlFlow>> {
        let old_env = Rc::clone(&self.environment);
        self.environment = Environment::new_block(Rc::clone(&old_env));

        for stmt in stmts {
            if let Some(cf) = self.interpret_statement(stmt)? {
                self.environment = old_env;
                return Ok(Some(cf)); // in case of break, continue or return:
                                     // we automatically stop interpreting as we return!
            }
        }

        self.environment = old_env;

        Ok(None)
    }
    fn interpret_throw_stmt(
        &mut self,
        exception_token: &Token,
        expr_opt: Option<&Expr>,
    ) -> Result<Option<ControlFlow>> {
        let exception_kind = match exception_token.token_type {
            Error => ExceptionKind::Exception,
            NameError => ExceptionKind::NameError,
            TypeError => ExceptionKind::TypeError,
            IndexError => ExceptionKind::IndexError,
            ValueError => ExceptionKind::ValueError,

            _ => panic!("ERROR"),
        };

        let message_opt = match expr_opt {
            Some(expr) => Some(self.interpret_expression(expr)?.to_string()),
            None => None,
        };

        let err = match message_opt {
            Some(msg) => Error::new(ErrorKind::Exception(exception_kind), msg, exception_token),
            None => {
                Error::new_without_message(ErrorKind::Exception(exception_kind), exception_token)
            }
        };

        Err(err)
    }
    fn interpret_return_stmt(&mut self, token: &Token, expr: &Expr) -> Result<Option<ControlFlow>> {
        let return_val = self.interpret_expression(expr)?;

        if !self.environment.borrow().in_fun {
            Err(Error::new(
                ErrorKind::Fatal(UnexpectedToken),
                "Return outside function!".to_string(),
                token,
            ))
        } else {
            Ok(Some(ControlFlow::Return(return_val)))
        }
    }
    fn interpret_continue_stmt(&mut self, token: &Token) -> Result<Option<ControlFlow>> {
        if !self.environment.borrow().in_loop {
            Err(Error::new(
                ErrorKind::Fatal(UnexpectedToken),
                "Continue outside loop!".to_string(),
                token,
            ))
        } else {
            Ok(Some(ControlFlow::Continue))
        }
    }
    fn interpret_break_stmt(&mut self, token: &Token) -> Result<Option<ControlFlow>> {
        if !self.environment.borrow().in_loop {
            Err(Error::new(
                ErrorKind::Fatal(UnexpectedToken),
                "Break outside loop!".to_string(),
                token,
            ))
        } else {
            Ok(Some(ControlFlow::Break))
        }
    }

    // INTERPRETING EXPRESSIONS
    pub fn interpret_expression(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Increment(op_l_opt, expr, op_r_opt) => {
                self.interpret_increment_expr(op_l_opt.as_ref(), expr, op_r_opt.as_ref())
            }
            Expr::Unary(op, expr) => self.interpret_unary_expr(op, expr.as_ref()),
            Expr::Binary(left, op, right) => {
                self.interpret_binary_expr(left.as_ref(), op, right.as_ref())
            }
            Expr::Logical(left, token, right) => {
                self.interpret_logical_expr(left.as_ref(), token, right.as_ref())
            }
            Expr::Ternary(cond, if_right, if_wrong) => {
                self.interpret_ternary_expr(cond.as_ref(), if_right.as_ref(), if_wrong.as_ref())
            }
            Expr::Literal(val) => Ok(val.to_owned()),
            Expr::Group(expr) => self.interpret_expression(expr.as_ref()),
            Expr::Variable(name) => self.interpret_var_expr(name),
            Expr::Index(expr, index_expr, err_token) => {
                self.interpret_index_expr(expr.as_ref(), index_expr.as_ref(), err_token)
            }
            Expr::List(expr_vec) => self.interpret_list_expr(expr_vec),
            Expr::Assign(name, op, expr) => {
                self.interpret_assign_expr(name.as_ref(), op, expr.as_ref())
            }
            Expr::Range(left, op, right) => {
                self.interpret_range_expr(left.as_ref(), op, right.as_ref())
            }
            Expr::Call(callee, args, err_token) => {
                self.interpret_call_expr(callee.as_ref(), args, err_token)
            }
            Expr::Closure(_, _, _, _) => self.interpret_closure_expr(expr),
            Expr::Get(target, property) => self.interpret_get_expr(target.as_ref(), property),
            Expr::PathGet(target, property) => {
                self.interpret_path_get_expr(target.as_ref(), property)
            }
            Expr::PathSet(target, property, expr) => {
                self.interpret_path_set_expr(target.as_ref(), property, expr.as_ref())
            }
            Expr::Set(target, property, expr) => {
                self.interpret_set_expr(target.as_ref(), property, expr.as_ref())
            }
        }
    }
    fn interpret_increment_expr(
        &mut self,
        op_l_opt: Option<&Token>,
        var: &Expr,
        op_r_opt: Option<&Token>,
    ) -> Result<Value> {
        let (var_token, indices_opt, indices_err_tokens) = self.get_token_and_indices_val(var)?;

        let Identifier(var_name) = var_token.token_type else {
            unreachable!();
        };

        let op = match (op_l_opt, op_r_opt) {
            (Some(op), _) => op,
            (_, Some(op)) => op,
            (_, _) => panic!("ERROR"), // case covered in the parser
        };

        let var_val = self.interpret_expression(var)?;
        match (op_l_opt, var_val, op_r_opt) {
            (Some(_), Value::Number(num), None) => {
                let final_num = match op.token_type {
                    PlusPlus => num + 1.0,
                    MinusMinus => num - 1.0,

                    _ => panic!("ERROR"),
                };
                self.environment.borrow_mut().assign_var(
                    &var_name,
                    indices_opt,
                    Value::Number(final_num),
                    &indices_err_tokens,
                );
                Ok(Value::Number(final_num))
            }
            (None, Value::Number(original_num), Some(_)) => {
                let final_num = match op.token_type {
                    PlusPlus => original_num + 1.0,
                    MinusMinus => original_num - 1.0,

                    _ => panic!("ERROR"),
                };
                self.environment.borrow_mut().assign_var(
                    &var_name,
                    indices_opt,
                    Value::Number(final_num),
                    &indices_err_tokens,
                );
                Ok(Value::Number(original_num))
            }

            (_, val, _) => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                format!("Unary operator `{}` is not supported on {:?}.", op, val),
                op,
            )),
        }
    }
    fn interpret_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<Value> {
        let right_val = self.interpret_expression(right)?;

        match (right_val, &op.token_type) {
            (Value::Number(num), Minus) => Ok(Value::Number(-num)),
            (Value::Boolean(bool), Bang) => Ok(Value::Boolean(!bool)),

            (val, operator) => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::ValueError),
                format!(
                    "Unary operation, `{}` is not supported on `{:?}`.",
                    operator, val
                ),
                op,
            )),
        }
    }
    fn interpret_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value> {
        let left_val = self.interpret_expression(left)?;
        let right_val = self.interpret_expression(right)?;
        let default_err = Error::new(
            ErrorKind::Exception(ExceptionKind::ValueError),
            format!(
                "Binary operation `{}` is not supported b/w `{:?}` and `{:?}`.",
                op, left_val, right_val
            ),
            op,
        );

        match op.token_type {
            Plus | PlusEqual => match (left_val, right_val) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                (Value::String(str), val)
                    if !matches!(val, Value::List(_) | Value::Function(_) | Value::Type(_)) =>
                {
                    Ok(Value::String(format!("{}{}", str, val)))
                }
                (val, Value::String(str))
                    if !matches!(val, Value::List(_) | Value::Function(_) | Value::Type(_)) =>
                {
                    Ok(Value::String(format!("{}{}", val, str)))
                }

                // List + any Value = [..., Value]
                (Value::List(list), val) | (val, Value::List(list))
                    if !matches!(val, Value::List(_)) =>
                {
                    list.borrow_mut().push(val);
                    Ok(Value::List(list))
                }

                // List + List
                (Value::List(l), Value::List(r)) => {
                    l.borrow_mut().append(&mut r.borrow_mut());
                    Ok(Value::List(l))
                }

                _ => Err(default_err),
            },
            Minus | MinusEqual => match (left_val, right_val) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                _ => Err(default_err),
            },
            Modulus | ModulusEqual => match (left_val, right_val) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l % r)),
                _ => Err(default_err),
            },
            Star | StarEqual => match (left_val, right_val) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),

                // String * Number
                (Value::Number(num), Value::String(str))
                | (Value::String(str), Value::Number(num)) => {
                    let mut output_string = "".to_string();

                    if num < 0.0 {
                        return Err(Error::new(
                            ErrorKind::Exception(ExceptionKind::TypeError),
                            "Cannot multiply String with a negative Number.".to_string(),
                            op,
                        ));
                    }

                    if num.fract() != 0.0 {
                        return Err(Error::new(
                            ErrorKind::Exception(ExceptionKind::TypeError),
                            "Cannot multiply String with a non-integer.".to_string(),
                            op,
                        ));
                    }

                    for _ in 0..num as i32 {
                        output_string.push_str(&str)
                    }
                    Ok(Value::String(output_string))
                }

                // List * Number
                (Value::Number(num), Value::List(val_list))
                | (Value::List(val_list), Value::Number(num)) => {
                    if num < 0.0 {
                        return Err(Error::new(
                            ErrorKind::Exception(ExceptionKind::TypeError),
                            "Cannot multiply List with a negative Number.".to_string(),
                            op,
                        ));
                    }

                    if num.fract() != 0.0 {
                        return Err(Error::new(
                            ErrorKind::Exception(ExceptionKind::TypeError),
                            "Cannot multiply List with a non-integer.".to_string(),
                            op,
                        ));
                    }

                    if num == 0.0 {
                        // List * 0 = []
                        return Ok(Value::List(Rc::new(RefCell::new(Vec::new()))));
                    }

                    let val_list_clone = val_list.borrow().clone();

                    // loop one less time
                    for _ in 1..num as usize {
                        val_list.borrow_mut().extend(val_list_clone.clone());
                    }

                    Ok(Value::List(val_list))
                }
                _ => Err(default_err),
            },
            StarStar => match (left_val, right_val) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l.powf(r))),
                _ => Err(default_err),
            },
            Slash | SlashEqual => match (left_val, right_val) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                _ => Err(default_err),
            },
            SlashSlash => match (left_val, right_val) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number((l / r).floor())),
                _ => Err(default_err),
            },

            // Comparison
            EqualEqual | Is => match (left_val, right_val) {
                // don't allow function comparison
                (Value::Function(_), _) | (_, Value::Function(_)) => Err(default_err),
                (l, r) => Ok(Value::Boolean(l == r)),
            },
            BangEqual => match (left_val, right_val) {
                // don't allow function comparison
                (Value::Function(_), _) | (_, Value::Function(_)) => Err(default_err),
                (l, r) => Ok(Value::Boolean(l != r)),
            },
            Greater => match (left_val, right_val) {
                // only allow numbers
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l > r)),
                _ => Err(default_err),
            },
            GreaterEqual => match (left_val, right_val) {
                // only allow numbers
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l >= r)),
                _ => Err(default_err),
            },
            Less => match (left_val, right_val) {
                // only allow numbers
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l < r)),
                _ => Err(default_err),
            },
            LessEqual => match (left_val, right_val) {
                // only allow numbers
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l <= r)),
                _ => Err(default_err),
            },

            _ => Err(default_err),
        }
    }
    fn interpret_ternary_expr(
        &mut self,
        cond: &Expr,
        if_right: &Expr,
        if_wrong: &Expr,
    ) -> Result<Value> {
        let cond_val = self.interpret_expression(cond)?;
        let if_right_val = self.interpret_expression(if_right)?;
        let if_wrong_val = self.interpret_expression(if_wrong)?;

        if self.is_truthy(&cond_val) {
            Ok(if_right_val)
        } else {
            Ok(if_wrong_val)
        }
    }
    fn interpret_logical_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value> {
        let left_val = self.interpret_expression(left)?;

        if matches!(op.token_type, Or | PipePipe) {
            // if Or, and left is true: we return true
            if self.is_truthy(&left_val) {
                return Ok(Value::Boolean(true));
            }
        } else {
            // if And, and left is false: we return false
            if !self.is_truthy(&left_val) {
                return Ok(Value::Boolean(false));
            }
        }

        // if none of that: we return the result of the right expr
        let right_val = self.interpret_expression(right)?;

        Ok(Value::Boolean(self.is_truthy(&right_val)))
    }
    fn interpret_list_expr(&mut self, list: &Vec<Expr>) -> Result<Value> {
        let mut list_val = Vec::with_capacity(4);

        for item in list {
            let item_val = self.interpret_expression(item)?;

            list_val.push(item_val);
        }

        Ok(Value::List(Rc::new(RefCell::new(list_val))))
    }
    fn interpret_var_expr(&mut self, var: &Token) -> Result<Value> {
        let var_name = match &var.token_type {
            Identifier(name) => name,
            _ => panic!("ERROR"), // not possible
        };

        // error handling will be done by the `environment.get_var()` function
        let value_result = self.environment.borrow().get_var(var_name, var);

        match value_result {
            // if a value is found
            Ok(val) => Ok(val),

            // if no entry is found, get the native function with the same name
            Err(val_not_found_err) => self
                .resolve_native_val(None, var_name)
                .ok_or(val_not_found_err),
        }
    }
    fn interpret_index_expr(
        &mut self,
        object: &Expr,
        index_expr: &Expr,
        err_token: &Token,
    ) -> Result<Value> {
        let list_rc = match self.interpret_expression(object)? {
            Value::List(val_list) => val_list,

            v => {
                return Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    format!("Can't index value `{}` which is of type {:?}.", object, v),
                    err_token,
                ))
            }
        };

        let list = list_rc.borrow();

        let index = self.get_index(index_expr, err_token)?;

        if index >= list.len() {
            return Err(Error::new(
                ErrorKind::Exception(ExceptionKind::IndexError),
                format!(
                    "The length of the List `{}` is {} but the index supplied is {}.",
                    object,
                    list.len(),
                    index,
                ),
                err_token,
            ));
        }

        Ok(list[index].to_owned())
    }
    fn interpret_assign_expr(&mut self, target: &Expr, op: &Token, expr: &Expr) -> Result<Value> {
        let (target_token, indices_opt, err_tokens) = self.get_token_and_indices_val(target)?;

        let var_name = match target_token.token_type {
            Identifier(name) => name,
            _ => panic!("ERROR"), // not possible
        };

        let new_val = match op.token_type {
            Equal | Be => self.interpret_expression(expr)?,

            PlusEqual | MinusEqual | StarEqual | SlashEqual | ModulusEqual => {
                // this function will apply the necessary operation
                self.interpret_binary_expr(target, op, expr)?
            }

            _ => panic!("ERROR"),
        };

        // update name only if the instance is owned (and not just a reference)
        if matches!(target, Expr::Variable(_)) {
            match &new_val {
                Value::Instance(instance_rc) if Rc::strong_count(instance_rc) == 1 => {
                    instance_rc.borrow_mut().update_name(var_name.to_owned())
                }
                _ => (),
            }
        }

        self.environment.borrow_mut().assign_var(
            &var_name,
            indices_opt,
            new_val.clone(),
            &err_tokens,
        )?;

        Ok(new_val)
    }
    fn interpret_range_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value> {
        let left_val = self.interpret_expression(left)?;
        let right_val = self.interpret_expression(right)?;

        let is_int = |num: f64| num.fract() == 0.0;

        match (left_val, right_val) {
            (Value::Number(num1), Value::Number(num2)) => {
                if is_int(num1) && is_int(num2) {
                    // if bad range
                    if num2 < num1 {
                        return Ok(Value::Range(0, 0));
                    }

                    match op.token_type {
                        DotDot => return Ok(Value::Range(num1 as i64, num2 as i64)),
                        DotDotEqual => return Ok(Value::Range(num1 as i64, num2 as i64 + 1)),

                        _ => panic!("ERROR"),
                    }
                }

                let problematic_num = if is_int(num1) { num2 } else { num1 };

                Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    format!(
                        "Both ends of Range should be integers. Found `{}`.",
                        problematic_num
                    ),
                    op,
                ))
            }

            (l, r) => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                format!("Can't have a Range between a {:?} and {:?}.", l, r),
                op,
            )),
        }
    }
    fn interpret_call_expr(
        &mut self,
        callee: &Expr,
        args: &Vec<Expr>,
        err_token: &Token,
    ) -> Result<Value> {
        // get the function object
        match self.interpret_expression(callee)? {
            Value::Function(fun_rc) => self.interpret_fun_call_expr(fun_rc, args, err_token),
            Value::NativeFunction(self_opt, native_fun_rc) => self.interpret_native_fun_call_expr(
                native_fun_rc,
                self_opt.map(|val| *val),
                args,
                err_token,
            ),
            Value::Class(class_rc) => self.interpret_class_call_expr(class_rc, args, err_token),
            Value::NativeClass(class_rc) => {
                self.interpret_native_class_call_expr(class_rc, args, err_token)
            }

            v => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                format!("Expected Function. Found {:?}", v),
                err_token,
            )),
        }
    }
    fn interpret_fun_call_expr(
        &mut self,
        fun_rc: Rc<SigmaFun>,
        args: &Vec<Expr>,
        err_token: &Token,
    ) -> Result<Value> {
        self.check_arity(false, fun_rc.arity, args.len(), err_token)?;

        // get arguments
        let mut args_val = Vec::with_capacity(4);

        for arg in args {
            args_val.push(self.interpret_expression(arg)?);
        }

        // call the function
        fun_rc.call(args_val)
    }
    fn interpret_native_fun_call_expr(
        &mut self,
        native_fun_rc: Rc<NativeFun>,
        self_opt: Option<Value>,
        args: &Vec<Expr>,
        err_token: &Token,
    ) -> Result<Value> {
        self.check_arity(false, native_fun_rc.arity, args.len(), err_token)?;

        // get arguments
        let mut args_val = Vec::with_capacity(4);

        for arg in args {
            args_val.push(self.interpret_expression(arg)?);
        }

        // call the function
        native_fun_rc.call(self_opt, args_val, err_token)
    }
    fn interpret_class_call_expr(
        &mut self,
        class_rc: Rc<SigmaClass>,
        args: &Vec<Expr>,
        err_token: &Token,
    ) -> Result<Value> {
        let instance_rc = class_rc.new_instance();

        let fun_rc = match instance_rc.borrow().get_property(&"__new".to_string()) {
            Some(Value::Function(fun_rc)) => fun_rc,

            _ => {
                // if no __new function defined, check if the arity is 0 and return an instance
                self.check_arity(true, (0, Some(0)), args.len(), err_token)?;

                return Ok(Value::Instance(Rc::clone(&instance_rc)));
            }
        };

        self.check_arity(true, fun_rc.arity, args.len(), err_token)?;

        // get arguments
        let mut args_val = Vec::with_capacity(4);

        for arg in args {
            args_val.push(self.interpret_expression(arg)?);
        }

        // call the __new function
        match fun_rc.call(args_val)? {
            Value::Nil => Ok(Value::Instance(instance_rc)),

            // the function should return Nil
            v => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                format!("The function `__new` should return Nil. Found {:?}.", v),
                err_token,
            )),
        }
    }
    fn interpret_native_class_call_expr(
        &mut self,
        class_rc: Rc<NativeClass>,
        args: &Vec<Expr>,
        err_token: &Token,
    ) -> Result<Value> {
        let instance_rc = class_rc.new_instance();

        let (me_opt, fun_rc) = match instance_rc.borrow().get_property(&"__new".to_string()) {
            Some(Value::NativeFunction(me_opt, fun_rc)) => (me_opt.map(|b| *b), fun_rc),

            _ => {
                // if no __new function defined, check if the arity is 0 and return an instance
                self.check_arity(true, (0, Some(0)), args.len(), err_token)?;

                return Ok(Value::Instance(Rc::clone(&instance_rc)));
            }
        };

        self.check_arity(true, fun_rc.arity, args.len(), err_token)?;

        // get arguments
        let mut args_val = Vec::with_capacity(4);

        for arg in args {
            args_val.push(self.interpret_expression(arg)?);
        }

        fun_rc.call(me_opt, args_val, err_token);

        Ok(Value::Instance(instance_rc))
    }
    fn interpret_closure_expr(&mut self, expr: &Expr) -> Result<Value> {
        let fun_decl = match expr {
            Expr::Closure(params, default_params, has_varargs, stmt) => Stmt::FunDecl(
                None,
                params.to_vec(),
                default_params.to_vec(),
                *has_varargs,
                stmt.to_owned(),
            ),
            _ => panic!("ERROR"),
        };

        let fun = SigmaFun::new(fun_decl, Rc::clone(&self.environment), self)?;

        Ok(Value::Function(Rc::new(fun)))
    }
    fn interpret_get_expr(&mut self, target: &Expr, property: &Token) -> Result<Value> {
        let target_val = self.interpret_expression(target)?;

        let property_name = match &property.token_type {
            Identifier(name) => name,
            _ => unreachable!(),
        };

        let instance_rc = match target_val {
            Value::Instance(instance_rc) => instance_rc,

            val => {
                let error = Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    format!(
                        "Can't get property `{}` of value of type {:?}.",
                        property, val
                    ),
                    property,
                );

                // if target is not an instance, try to get native value
                return self
                    .resolve_native_val(Some(val), property_name)
                    .ok_or(error);
            }
        };

        let instance = instance_rc.borrow();

        match instance.get_property(property_name) {
            Some(val) => Ok(val),

            None => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::PropertyError),
                format!(
                    "No property named `{}` found on the Instance `{}` of Class {}.",
                    property_name,
                    instance.get_instance_name(),
                    instance.get_class_name(),
                ),
                property,
            )),
        }
    }
    fn interpret_path_get_expr(&mut self, target: &Expr, property_tok: &Token) -> Result<Value> {
        let target_val = self.interpret_expression(target)?;

        let property_name = match &property_tok.token_type {
            Identifier(name) => name,
            _ => unreachable!(),
        };

        let mod_rc = match target_val {
            Value::Module(mod_rc) => mod_rc,

            val => {
                return Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    format!(
                        "The `::` operator can only be used on Modules. Found {:?}.",
                        val
                    ),
                    property_tok,
                ));
            }
        };

        let mod_name = { mod_rc.borrow().name.to_string() };
        let mut mod_mut = mod_rc.borrow_mut();

        match mod_mut.get_property(property_name) {
            Some(val) => Ok(val),

            None => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::PropertyError),
                format!(
                    "No property named `{}` found on Module `{}`.",
                    property_name, mod_name
                ),
                property_tok,
            )),
        }
    }
    fn interpret_path_set_expr(
        &mut self,
        target: &Expr,
        property_tok: &Token,
        expr: &Expr,
    ) -> Result<Value> {
        let target_val = self.interpret_expression(target)?;

        let property_name = match &property_tok.token_type {
            Identifier(name) => name.to_owned(),
            _ => unreachable!(),
        };

        let val = self.interpret_expression(expr)?;

        match target_val {
            Value::Module(mod_rc) => {
                mod_rc.borrow_mut().set_property(property_name, val.clone());
                Ok(val)
            }

            val => Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                format!(
                    "The `::` operator can only be used on Modules. Found {:?}.",
                    val
                ),
                property_tok,
            )),
        }
    }
    fn interpret_set_expr(
        &mut self,
        target: &Expr,
        property: &Token,
        expr: &Expr,
    ) -> Result<Value> {
        let target_val = self.interpret_expression(target)?;

        let property_name = match &property.token_type {
            Identifier(name) => name,
            _ => unreachable!(),
        };

        let instance_rc = match target_val {
            Value::Instance(instance_rc) => instance_rc,

            val => {
                return Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    format!(
                        "Can't set property `{}` of value of type {:?}.",
                        property, val
                    ),
                    property,
                ));
            }
        };

        let new_val = self.interpret_expression(expr)?;

        instance_rc
            .borrow_mut()
            .set_property(property_name.to_owned(), new_val.clone());

        Ok(new_val)
    }

    // HELPER FUNCTIONS
    fn is_truthy(&self, val: &Value) -> bool {
        match val {
            // false, nil and 0 are falsey and everything else is truthy
            Value::Nil => false,
            Value::Boolean(bool) => *bool,
            Value::Number(0.0) => false,
            _ => true,
        }
    }
    fn get_token_and_indices_val(
        &mut self,
        target: &Expr,
    ) -> Result<(Token, Option<Vec<usize>>, Vec<Token>)> {
        fn search_token(
            target: &Expr,
            indices: &mut Vec<Expr>,
            indices_err_tokens: &mut Vec<Token>,
        ) -> Result<Token> {
            match target {
                Expr::Variable(token) => Ok(token.clone()),

                Expr::Index(target, index, err_token) => {
                    indices.push(*index.to_owned());
                    indices_err_tokens.push(err_token.to_owned());
                    search_token(target, indices, indices_err_tokens)
                }

                _ => Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    "Can't assign to an expression.".to_string(),
                    indices_err_tokens.last().unwrap(),
                )),
            }
        }

        let mut indices = Vec::new();
        let mut indices_err_tokens = Vec::new();

        let token = search_token(target, &mut indices, &mut indices_err_tokens)?;

        indices_err_tokens.push(token.clone());

        let mut indices_val = Vec::new();

        for (i, index_expr) in indices.into_iter().enumerate() {
            indices_val.push(self.get_index(&index_expr, &indices_err_tokens[i])?)
        }

        indices_val.reverse();
        indices_err_tokens.reverse();

        let indices_val_opt = if indices_val.is_empty() {
            None
        } else {
            Some(indices_val)
        };

        Ok((token, indices_val_opt, indices_err_tokens))
    }
    fn get_index(&mut self, index: &Expr, err_token: &Token) -> Result<usize> {
        let index_float = match self.interpret_expression(index)? {
            Value::Number(num) => num,
            v => {
                return Err(Error::new(
                    ErrorKind::Exception(ExceptionKind::TypeError),
                    format!("List index should be a Number. Found {:?}", v),
                    err_token,
                ))
            }
        };

        if index_float < 0.0 || index_float.fract() != 0.0 {
            return Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                format!(
                    "List index should be a non-negative Integer. Found {}.",
                    index_float
                ),
                err_token,
            ));
        }

        let index = index_float as usize;

        Ok(index)
    }
    fn check_arity(
        &self,
        is_class: bool,
        arity: (usize, Option<usize>),
        args_len: usize,
        err_token: &Token,
    ) -> Result<()> {
        let callee = if is_class {
            format!("`__new` of class `{}`", err_token)
        } else {
            format!("`{}`", err_token)
        };

        // if the number of args passed is less than the min arity
        if args_len < arity.0 {
            // if the function does not have a fixed arity
            let mut err_message = format!(
                "The function {} accepts at least {} arguments but {} arguments were supplied.",
                callee, arity.0, args_len
            );

            // if the function has a fixed arity (min arity = max arity)
            if arity.1.is_some() && arity.0 == arity.1.unwrap() {
                err_message = format!(
                    "The function {} accepts {} arguments but {} arguments were supplied.",
                    callee, arity.0, args_len
                );
            }

            return Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                err_message,
                err_token,
            ));
        }
        // if the number of arguments passed is more than the max arity
        else if arity.1.is_some() && args_len > arity.1.unwrap() {
            let mut err_message = format!(
                "The function {} accepts at most {} arguments but {} arguments were supplied.",
                callee,
                arity.1.unwrap(),
                args_len,
            );

            // if the function has a fixed arity (min arity = max arity)
            if arity.0 == arity.1.unwrap() {
                err_message = format!(
                    "The function {} accepts {} arguments but {} arguments were supplied.",
                    callee, arity.0, args_len
                );
            }

            return Err(Error::new(
                ErrorKind::Exception(ExceptionKind::TypeError),
                err_message,
                err_token,
            ));
        }

        Ok(())
    }
    fn resolve_native_val(
        &mut self,
        target: Option<Value>,
        property_name: &String,
    ) -> Option<Value> {
        let lib = self.native_libs.iter_mut().find(|lib| {
            matches!(
                (lib, &target),
                (Library::Number(_), Some(Value::Number(_)))
                    | (Library::String(_), Some(Value::String(_)))
                    | (Library::List(_), Some(Value::List(_)))
                    | (Library::Range(_), Some(Value::Range(_, _)))
                    | (Library::Prelude(_), None)
            )
        })?;

        let fun_val = match lib {
            Library::Number(num_lib) => num_lib.get_item(property_name)?,

            Library::String(string_lib) => string_lib.get_item(property_name)?,

            Library::List(list_lib) => list_lib.get_item(property_name)?,

            Library::Range(range_lib) => range_lib.get_item(property_name)?,

            Library::Prelude(prlelude_lib) => prlelude_lib.get_item(property_name)?,

            _ => return None,
        };

        match fun_val {
            Value::NativeFunction(_, native_fun) => {
                Some(Value::NativeFunction(target.map(Box::new), native_fun))
            }

            _ => None,
        }
    }
    // returns Err(()) when no module
    fn resolve_native_module(
        &mut self,
        mod_name: &String,
        item_name: Option<&String>,
    ) -> std::result::Result<Option<Value>, ()> {
        let lib_opt = self
            .native_libs
            .iter_mut()
            .find(|lib| matches!((lib, &**mod_name), (Library::Standard(_), "std")));

        // if module not found
        if lib_opt.is_none() {
            return Err(());
        }

        match (lib_opt.unwrap(), item_name) {
            (Library::Standard(std_lib), Some(name)) => Ok(std_lib.get_item(name)),

            (Library::Standard(std_lib), None) => {
                let module = Module::new_native(mod_name.to_owned(), Box::new(std_lib.to_owned()));

                Ok(Some(Value::Module(Rc::new(RefCell::new(module)))))
            }

            _ => Err(()),
        }
    }
    fn run_module(
        &mut self,
        mod_path: &str,
        mod_name: &String,
        item_tok: Option<&Token>,
        path_token: &Token,
    ) -> Result<Value> {
        let mut sigma = Sigma::new();

        let item_name = match &item_tok {
            Some(tok) => match &tok.token_type {
                Identifier(name) => Some(name),
                _ => unreachable!(),
            },

            None => None,
        };

        match sigma.run_file(mod_path) {
            Ok(_) => {
                let exports = sigma.interpreter.environment.borrow().deconstruct();

                let module = Module::new(mod_name.clone(), exports);

                Ok(Value::Module(Rc::new(RefCell::new(module))))
            }

            // if there were errors executing the file
            Err(_) if sigma.had_error => Err(Error::new(
                ErrorKind::Fatal(ImportError),
                "Aborting due to previous errors.".to_string(),
                path_token,
            )),

            // if file not found
            Err(_) => match self.resolve_native_module(mod_name, item_name) {
                Ok(Some(mod_val)) => Ok(mod_val),

                // unwrap() is okay because the functions can't return Ok(_) if item_name is none
                Ok(None) => Err(Error::new(
                    ErrorKind::Fatal(ImportError),
                    format!(
                        "No item `{}` found in module `{}`.",
                        item_name.unwrap(),
                        mod_name
                    ),
                    item_tok.unwrap(),
                )),

                Err(_) => Err(Error::new(
                    ErrorKind::Fatal(ImportError),
                    format!("No module named `{}` found.", mod_name),
                    path_token,
                )),
            },
        }
    }
}
