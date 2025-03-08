use std::cell::RefCell;
use std::rc::Rc;

use crate::value::{native_fun::NativeFun, Value};

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct RangeLib {
    loaded_functions: Vec<Value>,
}

impl RangeLib {
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
            "rev" => NativeFun::new("rev", (0, Some(0)), Self::rev),
            "collect" => NativeFun::new("collect", (0, Some(0)), Self::colect),

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
    fn rev(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::Range(num1, num2) => Ok(Value::Range(*num2, *num1)),

            _ => unreachable!(),
        }
    }
    fn colect(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::Range(num1, num2) => {
                let list = if num1 <= num2 {
                    (*num1..*num2).map(|num| Value::Number(num as f64)).collect::<Vec<_>>()
                }
                else {
                    (*num2..*num1).rev().map(|num| Value::Number(num as f64)).collect::<Vec<_>>()
                };

                let list_rc = Rc::new(RefCell::new(list));

                Ok(Value::List(list_rc))
            },

            _ => unreachable!(),
        }
    }
}
