use std::cell::RefCell;
use std::rc::Rc;

use super::NativeFunLib;

use crate::value::{native_fun::NativeFun, Value};

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct StringLib {
    loaded_functions: Vec<Rc<NativeFun>>,
}

impl NativeFunLib for StringLib {
    fn get_loaded(&self) -> &Vec<Rc<NativeFun>> {
        &self.loaded_functions
    }

    fn get_loaded_mut(&mut self) -> &mut Vec<Rc<NativeFun>> {
        &mut self.loaded_functions
    }

    #[rustfmt::skip]
    fn match_function(&self, fun_name: &str) -> Option<NativeFun> {
        let fun = match fun_name {
            "len" => NativeFun::new("len", (0, Some(0)), Self::len),
            "is_empty" => NativeFun::new("is_emtpy", (0, Some(0)), Self::is_empty),
            "to_lowercase" => NativeFun::new("to_lowercase", (0, Some(0)), Self::to_lowercase),
            "to_uppercase" => NativeFun::new("to_uppercase", (0, Some(0)), Self::to_uppercase),
            "to_num" => NativeFun::new("to_num", (0, Some(0)), Self::to_num),
            "trim" => NativeFun::new("trim", (0, Some(0)), Self::trim),
            "contains" => NativeFun::new("contains", (0, Some(0)), Self::contains),
            "starts_with" => NativeFun::new("starts_with", (1, Some(1)), Self::starts_with),
            "ends_with" => NativeFun::new("ends_with", (1, Some(1)), Self::ends_with),
            "chars" => NativeFun::new("chars", (0, Some(0)), Self::chars),
            "chat_at" => NativeFun::new("chat_at", (1, Some(1)), Self::char_at),
            "reverse" => NativeFun::new("reverse", (0, Some(0)), Self::reverse),
            "concat" => NativeFun::new("concat", (1, Some(1)), Self::concat),
            "replace" => NativeFun::new("replace", (2, Some(2)), Self::replace),
            "repeat" => NativeFun::new("repeat", (1, Some(1)), Self::repeat),

            _ => return None,
        };
        Some(fun)
    }
}

impl StringLib {
    pub fn new() -> Self {
        Self {
            loaded_functions: Vec::with_capacity(4),
        }
    }

    // HELPER FUNCTIONS
    fn check_index(num: f64, err_token: &Token) -> Result<usize> {
        if num < 0.0 {
            return Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!("List index cannot be negative. Found `{}`", num),
                err_token,
            ));
        }

        if num.fract() != 0.0 {
            return Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!("List index cannot be a float value. Found `{}`", num),
                err_token,
            ));
        }

        Ok(num as usize)
    }
    //fn check_list_len(index: usize, list_len: usize, err_token: &Token) -> Result<()> {
    //    if index >= list_len {
    //        return Err(Error::new(
    //            ErrorKind::Exception(TypeError),
    //            format!(
    //                "The length of the List is {} but the index supplied is {}.",
    //                list_len, index
    //            ),
    //            err_token,
    //        ));
    //    }
    //    Ok(())
    //}

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
    fn to_lowercase(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::String(string) => Ok(Value::String(string.to_lowercase())),

            _ => unreachable!(),
        }
    }
    fn to_uppercase(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::String(string) => Ok(Value::String(string.to_uppercase())),

            _ => unreachable!(),
        }
    }
    fn to_num(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::String(string) => {
                let num_val = string
                    .parse::<f64>()
                    .map(Value::Number)
                    .unwrap_or(Value::Nil);

                Ok(num_val)
            }

            _ => unreachable!(),
        }
    }
    fn trim(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::String(string) => Ok(Value::String(string.trim().to_string())),

            _ => unreachable!(),
        }
    }
    fn contains(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::String(str1), Value::String(str2)) => Ok(Value::Boolean(str1.contains(str2))),

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `contains` expects its argument to be a String. Found {:?}",
                    val
                ),
                err_token,
            )),
        }
    }
    fn starts_with(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::String(str1), Value::String(str2)) => {
                Ok(Value::Boolean(str1.starts_with(str2)))
            }

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `starts_with` expects its argument to be a String. Found {:?}",
                    val
                ),
                err_token,
            )),
        }
    }
    fn ends_with(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::String(str1), Value::String(str2)) => Ok(Value::Boolean(str1.ends_with(str2))),

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `ends_with` expects its argument to be a String. Found {:?}",
                    val
                ),
                err_token,
            )),
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
    fn char_at(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::String(string), Value::Number(num)) => {
                let index = Self::check_index(*num, err_token)?;

                let char_opt = string.chars().nth(index);

                // returns Nil if index not found
                let ret_val = char_opt
                    .map(|ch| Value::String(ch.to_string()))
                    .unwrap_or(Value::Nil);

                Ok(ret_val)
            }

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `char_at` expects its argument to be a Number. Found {:?}",
                    val
                ),
                err_token,
            )),
        }
    }
    fn reverse(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::String(string) => {
                let rev_string = string.chars().rev().collect();

                Ok(Value::String(rev_string))
            }

            _ => unreachable!(),
        }
    }
    fn concat(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::String(str1), Value::String(str2)) => {
                Ok(Value::String(format!("{}{}", str1, str2)))
            }

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `concat` expects its argument to be a String. Found {:?}",
                    val
                ),
                err_token,
            )),
        }
    }
    fn replace(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1], &args_val[2]) {
            (Value::String(string), Value::String(from), Value::String(to)) => {
                let ret_string = string.replace(from, to);

                Ok(Value::String(ret_string))
            }

            (_, val1, val2) => {
                let problematic_val = if matches!(val1, Value::String(_)) {
                    val2
                } else {
                    val1
                };

                Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `replace` expects both of its arguments to be String. Found {:?}",
                        problematic_val
                    ),
                    err_token,
                ))
            }
        }
    }
    fn repeat(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let num = match args_val.pop().unwrap() {
            Value::Number(num) => num,

            val => {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `repeat` expects its argument to be a Number. Found {:?}",
                        val
                    ),
                    err_token,
                ))
            }
        };

        if num < 0.0 || num.fract() != 0.0 {
            return Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "The argument to the native function `repeat` must be a positive-integer. Found `{}`",
                    num
                ),
                err_token,
            ));
        }

        match &args_val[0] {
            Value::String(str1) => Ok(Value::String(str1.repeat(num as usize))),

            _ => unreachable!(),
        }
    }
}
