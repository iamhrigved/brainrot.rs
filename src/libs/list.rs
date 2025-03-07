use std::cell::RefCell;
use std::rc::Rc;

use crate::value::{native_fun::NativeFun, Value};

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct ListLib {
    loaded_functions: Vec<Value>,
}

impl ListLib {
    pub fn new() -> Self {
        Self {
            loaded_functions: Vec::with_capacity(8),
        }
    }
    pub fn get_function(&mut self, fun_name: &String) -> Option<Value> {
        let loaded_fun_pos = self.loaded_functions.iter().position(|fun_val| {
            let Value::LibFunction(fun_rc) = &fun_val else {
                unreachable!()
            };

            &fun_rc.borrow().name == fun_name
        });

        if let Some(index) = loaded_fun_pos {
            return self.loaded_functions.get(index).cloned();
        }

        self.match_function(fun_name).map(|fun| self.load_fun(fun))
    }

    #[rustfmt::skip]
    fn match_function(&self, fun_name: &String) -> Option<NativeFun> {
        let fun = match &**fun_name {
            "len" => NativeFun::new("len".to_string(), (0, Some(0)), Self::len),
            "clone" => NativeFun::new("clone".to_string(), (0, Some(0)), Self::clone),
            "is_empty" => NativeFun::new("is_emtpy".to_string(), (0, Some(0)), Self::is_empty),
            "any" => NativeFun::new("any".to_string(), (1, Some(1)), Self::any),

            _ => return None,
        };

        Some(fun)
    }

    fn load_fun(&mut self, sigma_fun: NativeFun) -> Value {
        let fun_rc = Rc::new(RefCell::new(sigma_fun));
        let fun_val = Value::LibFunction(fun_rc);

        self.loaded_functions.push(fun_val.clone());

        fun_val
    }

    // FUNCTIONS
    fn len(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::List(list_rc) => Ok(Value::Number(list_rc.borrow().len() as f64)),

            _ => unreachable!(),
        }
    }
    fn clone(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        let Value::List(list_rc) = &args_val[0] else {
            unreachable!()
        };

        let list_clone = list_rc.borrow().clone();

        let list_clone_rc = Rc::new(RefCell::new(list_clone));

        let list_clone_val = Value::List(list_clone_rc);

        Ok(list_clone_val)
    }
    fn is_empty(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::List(list_rc) => Ok(Value::Boolean(list_rc.borrow().is_empty())),

            _ => unreachable!(),
        }
    }
    fn any(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let (list_rc, fun_rc) = match (&args_val[0], &args_val[1]) {
            (Value::List(list_rc), Value::Function(fun_rc)) => {
                (Rc::clone(list_rc), Rc::clone(fun_rc))
            }

            (_, val) => {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `any` expects the argument to be a Function. Found {}",
                        val
                    ),
                    err_token,
                ));
            }
        };

        let fun_name = {
            // borrow begins
            let fun = fun_rc.borrow();

            let mut fun_name = "The function".to_string();

            if let Some(name) = &fun.name {
                fun_name = format!("The function `{}`", name);
            }

            if fun.arity != (1, Some(1)) {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "{} passed into `any` should take exactly 1 argument. Found {}.",
                        fun_name, fun.arity.0
                    ),
                    err_token,
                ));
            }

            fun_name
            // borrow ends
        };

        let list_borrow = list_rc.borrow();
        let list = list_borrow.iter();

        for item in list {
            match fun_rc.borrow_mut().call(vec![item.clone()])? {
                Value::Boolean(bool) => {
                    if bool {
                        return Ok(Value::Boolean(true));
                    }
                }

                val => {
                    return Err(Error::new(
                        ErrorKind::Exception(TypeError),
                        format!(
                            "{} passed into `any`, should return a Boolean. Found {:?}.",
                            fun_name, val
                        ),
                        err_token,
                    ))
                }
            }
        }

        Ok(Value::Boolean(false))
    }
}
