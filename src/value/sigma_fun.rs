use super::*;
use crate::error::Error;
use crate::interpreter::{ControlFlow, Interpreter};
use crate::parser::{Expr, Stmt};

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub struct SigmaFun {
    pub name: Option<String>,
    pub env_id: Option<u64>,
    params: Vec<String>,
    default_params: Vec<(String, Expr)>,
    fun_body: Option<Stmt>,
    pub arity: (usize, Option<usize>), // minimum and maximum arity
}

impl PartialEq for SigmaFun {
    fn eq(&self, other: &Self) -> bool {
        if self.name != other.name {
            return false;
        }

        if self.env_id != other.env_id {
            return false;
        }

        if self.arity != other.arity {
            return false;
        }

        true
    }
}

impl std::fmt::Display for SigmaFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fun_name = match &self.name {
            Some(name) => name,
            _ => "",
        };

        let mut args_string = "(".to_string();

        if self.params.is_empty() {
            return write!(f, "<Fun {}()>", fun_name);
        }

        for param in &self.params {
            args_string.push_str(&format!("{}, ", param))
        }
        for (def_param, def_expr) in &self.default_params {
            args_string.push_str(&format!("{} = {}, ", def_param, def_expr))
        }

        args_string.pop();
        args_string.pop();
        if self.arity.1.is_none() {
            args_string.push_str(" ...")
        }
        args_string.push(')');

        write!(f, "<Fun {}{}>", fun_name, args_string)
    }
}

impl SigmaFun {
    pub fn new_user_defined(declaration: Box<Stmt>, env_id: u64) -> Self {
        let (
            fun_name,
            mut minimum_arity,
            mut maximum_arity,
            params,
            default_params,
            has_varargs,
            fun_body,
        ) = match *declaration {
            Stmt::FunDecl(name_opt, params, default_params, has_varargs, body) => (
                name_opt,
                params.len(),
                Some(params.len() + default_params.len()),
                params,
                default_params,
                has_varargs,
                *body,
            ),
            _ => panic!("ERROR"),
        };

        // if there are varargs:
        //   1. don't count the last param in the arity because it is optional
        //   2. the maximum arity will be None
        if has_varargs {
            minimum_arity -= 1;
            maximum_arity = None;
        }
        Self {
            name: fun_name,
            env_id: Some(env_id),
            params,
            default_params,
            fun_body: Some(fun_body),
            arity: (minimum_arity, maximum_arity),
        }
    }
    pub fn new_method(declaration: Box<Stmt>) -> Self {
        let (
            name_opt,
            mut minimum_arity,
            mut maximum_arity,
            params,
            default_params,
            has_varargs,
            fun_body,
        ) = match *declaration {
            Stmt::FunDecl(name_opt, params, default_params, has_varargs, body) => (
                name_opt,
                params.len(),
                Some(params.len() + default_params.len()),
                params,
                default_params,
                has_varargs,
                *body,
            ),
            _ => panic!("ERROR"),
        };

        // don't count the last param in the arity because it is optional
        // if there are varargs, the maximum arity will be None
        if has_varargs {
            minimum_arity -= 1;
            maximum_arity = None;
        }
        Self {
            name: name_opt,
            env_id: None,
            params,
            default_params,
            fun_body: Some(fun_body),
            arity: (minimum_arity, maximum_arity),
        }
    }

    pub fn update_name(&mut self, name: String) {
        self.name = Some(name)
    }

    pub fn call(&mut self, args_val: Vec<Value>, interpreter: &mut Interpreter) -> Result<Value> {
        let old_id = interpreter.environments.set_cur_id(self.env_id.unwrap());

        // create a new local environment for the function body inside the captured environment
        interpreter.environments.push_fun();

        let mut cur_arg_index = 0;

        // fill out the mandatory params (the number of mandatory params = mandatory arity)
        for param in self.params.iter().take(self.arity.0) {
            // define the var
            interpreter
                .environments
                .define_var(param.clone(), Some(args_val[cur_arg_index].clone()));

            cur_arg_index += 1;
        }

        // manage default parameters
        for (param, expr) in &self.default_params {
            let final_val: Value;

            // if there is a passed argument, take it or else evaluate the expr
            if let Some(val) = args_val.get(cur_arg_index) {
                final_val = val.clone();
            } else {
                final_val = interpreter.interpret_expression(expr)?;
            }

            interpreter
                .environments
                .define_var(param.clone(), Some(final_val));

            cur_arg_index += 1;
        }

        // if there are varargs
        if self.arity.1.is_none() {
            let name = self.params.last().unwrap();

            let val_vec = args_val[self.arity.0..].to_vec();

            interpreter.environments.define_var(
                name.clone(),
                Some(Value::List(Rc::new(RefCell::new(val_vec)))),
            );
        }

        let mut fun_ret = Value::Nil;

        // if the function returns
        if let Some(ControlFlow::Return(val)) =
            interpreter.interpret_statement(self.fun_body.as_ref().unwrap())?
        {
            fun_ret = val;
        }

        interpreter.environments.pop_env();
        interpreter.environments.set_cur_id(old_id);

        Ok(fun_ret)
    }
}
