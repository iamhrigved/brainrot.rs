use std::cell::RefCell;
use std::rc::Rc;

use crate::value::{native_fun::NativeFun, Value};

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct StringLib {
    loaded_functions: Vec<Value>,
}

impl StringLib {
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
            "is_empty" => NativeFun::new("is_emtpy".to_string(), (0, Some(0)), Self::is_empty),
            "trim" => NativeFun::new("trim".to_string(), (0, Some(0)), Self::trim),
            "chars" => NativeFun::new("chars".to_string(), (0, Some(0)), Self::chars),

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
            Value::String(string) => Ok(Value::Number(string.len() as f64)),

            _ => unreachable!(),
        }
    }
    fn is_empty(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::String(string) => Ok(Value::Boolean(string.is_empty())),

            _ => unreachable!(),
        }
    }
    fn trim(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::String(string) => Ok(Value::String(string.trim().to_string())),

            _ => unreachable!(),
        }
    }
    fn chars(mut args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        let string = match args_val.pop().unwrap() {
            Value::String(str) => str,
            _ => unreachable!(),
        };

        let list = string
            .chars()
            .map(|ch| Value::String(ch.to_string()))
            .collect::<Vec<_>>();
        let list_rc = Rc::new(RefCell::new(list));

        Ok(Value::List(list_rc))
    }
}
