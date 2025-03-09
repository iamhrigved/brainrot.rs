use std::cell::RefCell;
use std::rc::Rc;

use super::NativeLib;

use crate::value::{native_fun::NativeFun, Value};

use crate::error::Error;
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct RangeLib {
    loaded_functions: Vec<Rc<NativeFun>>,
}

impl NativeLib for RangeLib {
    fn get_loaded(&self) -> &Vec<Rc<NativeFun>> {
        &self.loaded_functions
    }

    fn get_loaded_mut(&mut self) -> &mut Vec<Rc<NativeFun>> {
        &mut self.loaded_functions
    }

    #[rustfmt::skip]
    fn match_function(&self, fun_name: &str) -> Option<NativeFun> {
        let fun = match fun_name {
            "rev" => NativeFun::new("rev", (0, Some(0)), Self::rev),
            "collect" => NativeFun::new("collect", (0, Some(0)), Self::collect),

            _ => return None,
        }; 

        Some(fun)
    }
}

impl RangeLib {
    pub fn new() -> Self {
        Self {
            loaded_functions: Vec::with_capacity(8),
        }
    }

    // FUNCTIONS
    fn rev(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::Range(num1, num2) => Ok(Value::Range(*num2, *num1)),

            _ => unreachable!(),
        }
    }
    fn collect(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
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
