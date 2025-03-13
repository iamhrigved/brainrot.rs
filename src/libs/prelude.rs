#![allow(clippy::new_without_default)]

use super::NativeLib;

use std::cell::RefCell;
use std::rc::Rc;

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;
use crate::value::{native_fun::NativeFun, Value};

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub struct Prelude {
    loaded_functions: Vec<Value>,
}

impl NativeLib for Prelude {
    fn get_loaded(&self) -> &Vec<Value> {
        &self.loaded_functions
    }

    fn get_loaded_mut(&mut self) -> &mut Vec<Value> {
        &mut self.loaded_functions
    }

    #[rustfmt::skip]
    fn match_item(&self, fun_name: &str) -> Option<Value> {
        let fun = match fun_name {
            "type" =>NativeFun::new("type", (1, None), Self::r#type),
            "format" => NativeFun::new("format", (1, None), Self::format),
            "assert" => NativeFun::new("assert", (1, Some(1)), Self::assert),
            "assert_eq" => NativeFun::new("assert_eq", (2, Some(2)), Self::assert_eq),

            _ => return None,
        };

        Some(Value::NativeFunction(None, Rc::new(fun)))
    }
}

impl Prelude {
    pub fn new() -> Self {
        Self {
            loaded_functions: Vec::with_capacity(4),
        }
    }

    // FUNCTIONS
    fn r#type(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        if args_val.len() == 1 {
            return Ok(Value::Type(format!("{:?}", args_val[0])));
        }

        let mut type_list = Vec::new();

        for arg in args_val {
            type_list.push(Value::Type(format!("{:?}", arg)));
        }

        Ok(Value::List(Rc::new(RefCell::new(type_list))))
    }
    fn format(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let format_string = match args_val[0].clone() {
            Value::String(str) => str,
            v => return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!("Native function `format` expects the first argument to be a String. Found {:?}.", v),
                    err_token
                )),
        };

        let mut ret_string = String::new();
        let mut index = 1;

        format_string.split("{}").for_each(|split_string| {
            ret_string.push_str(split_string);
            if index >= args_val.len() {
                return;
            }
            ret_string.push_str(&args_val[index].to_string());
            index += 1;
        });

        Ok(Value::String(ret_string))
    }
    fn assert(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let is_assert_true = match &args_val[0] {
            Value::Boolean(bool) => *bool,

            val => {
                return Err(Error::new(
                    ErrorKind::Exception(ValueError),
                    format!(
                    "Native Function `{}` expects the argument to be of type Boolean. Found {:?}",
                    err_token, val
                ),
                    err_token,
                ))
            }
        };

        if !is_assert_true {
            return Err(Error::new(
                ErrorKind::Exception(AssertionError),
                "Assertion failed.".to_string(),
                err_token,
            ));
        }

        Ok(Value::Nil)
    }
    fn assert_eq(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        if args_val[0] != args_val[1] {
            return Err(Error::new(
                ErrorKind::Exception(AssertionError),
                format!(
                    "Value `{}` is not equal to Value `{}`.",
                    args_val[0], args_val[1]
                ),
                err_token,
            ));
        }
        Ok(Value::Nil)
    }
}
