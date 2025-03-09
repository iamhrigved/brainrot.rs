use super::*;
use crate::environment::Environment;
use crate::error::Error;
use crate::interpreter::{ControlFlow, Interpreter};
use crate::parser::Stmt;

type Result<T> = std::result::Result<T, Error>;

pub struct SigmaFun {
    pub name: Option<String>,
    pub captured_environment: Rc<RefCell<Environment>>,
    params: Vec<String>,
    default_params: Vec<(String, Value)>,
    fun_body: Option<Stmt>,
    pub arity: (usize, Option<usize>), // minimum and maximum arity
}

impl std::fmt::Display for SigmaFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fun_name = match &self.name {
            Some(name) => name,
            _ => "",
        };

        let mut args_string = "(".to_string();

        if self.params.is_empty() && self.default_params.is_empty() {
            return write!(f, "Fun {}()", fun_name);
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

        write!(f, "Fun {}{}", fun_name, args_string)
    }
}

// for Value::deep_clone(&self)
impl Clone for SigmaFun {
    fn clone(&self) -> Self {
        let env = self.captured_environment.borrow();
        let env_clone_rc = Rc::new(RefCell::new(env.clone()));

        Self {
            name: self.name.clone(),
            captured_environment: env_clone_rc,
            params: self.params.clone(),
            default_params: self.default_params.clone(),
            fun_body: self.fun_body.clone(),
            arity: self.arity,
        }
    }
}

impl SigmaFun {
    // takes the interpreter to interpret the default Expr inside the function declaration
    pub fn new(
        declaration: Stmt,
        captured_environment: Rc<RefCell<Environment>>,
        interpreter: &mut Interpreter,
    ) -> Result<Self> {
        let (
            name,
            mut minimum_arity,
            mut maximum_arity,
            params,
            default_params_expr,
            has_varargs,
            fun_body,
        ) = match declaration {
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

        let mut default_params = Vec::with_capacity(default_params_expr.len());
        for (name, expr) in default_params_expr {
            default_params.push((name, interpreter.interpret_expression(&expr)?))
        }

        Ok(Self {
            name,
            captured_environment,
            params,
            default_params,
            fun_body: Some(fun_body),
            arity: (minimum_arity, maximum_arity),
        })
    }

    pub fn call(&self, args_val: Vec<Value>) -> Result<Value> {
        let mut interpreter = Interpreter::from(Rc::clone(&self.captured_environment));

        // create a new local environment for the function body inside the captured environment
        let old_env = Rc::clone(&interpreter.environment);

        interpreter.environment = Environment::new_fun(Rc::clone(&old_env));

        let mut cur_arg_index = 0;

        // fill out the mandatory params (the number of mandatory params = mandatory arity)
        for param in self.params.iter().take(self.arity.0) {
            // define the var
            interpreter
                .environment
                .borrow_mut()
                .define_var(param.clone(), Some(args_val[cur_arg_index].clone()));

            cur_arg_index += 1;
        }

        // manage default parameters
        for (param, default_val) in &self.default_params {
            let final_val: Value;

            // if there is a passed argument, take it or else evaluate the expr
            if let Some(val) = args_val.get(cur_arg_index) {
                final_val = val.clone();
            } else {
                final_val = default_val.clone();
            }

            interpreter
                .environment
                .borrow_mut()
                .define_var(param.clone(), Some(final_val));

            cur_arg_index += 1;
        }

        // if there are varargs
        if self.arity.1.is_none() {
            let name = self.params.last().unwrap();

            let val_vec = args_val[self.arity.0..].to_vec();

            interpreter.environment.borrow_mut().define_var(
                name.clone(),
                Some(Value::List(Rc::new(RefCell::new(val_vec)))),
            );
        }

        let mut fun_ret = Value::Nil;

        // if the function returns
        match self.fun_body.as_ref().unwrap() {
            // if the statement is an expression, return the value
            Stmt::Expression(expr) => fun_ret = interpreter.interpret_expression(expr)?,

            // if any other stmt, normal function execution
            stmt => {
                if let Some(ControlFlow::Return(val)) = interpreter.interpret_statement(stmt)? {
                    fun_ret = val;
                }
            }
        }

        Ok(fun_ret)
    }
}
