use std::cell::RefCell;
use std::rc::Rc;

use crate::value::{native_fun::NativeFun, Value};

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct NumberLib {
    loaded_functions: Vec<Value>,
}

impl NumberLib {
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
            "round" => NativeFun::new("round", (0, Some(0)), Self::round),
            "floor" => NativeFun::new("floor", (0, Some(0)), Self::floor),
            "ceil" => NativeFun::new("ceil", (0, Some(0)), Self::ceil),
            "trunc" => NativeFun::new("trunc", (0, Some(0)), Self::trunc),
            "abs" => NativeFun::new("abs", (0, Some(0)), Self::abs),
            "sqrt" => NativeFun::new("sqrt", (0, Some(0)), Self::sqrt),
            "pow" => NativeFun::new("pow", (1, Some(1)), Self::pow),
            "to_string" => NativeFun::new("to_string", (0, Some(0)), Self::to_string),

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
    fn round(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match args_val[0] {
            Value::Number(num) => Ok(Value::Number(num.round())),

            _ => unreachable!(),
        }
    }
    fn floor(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match args_val[0] {
            Value::Number(num) => Ok(Value::Number(num.floor())),

            _ => unreachable!(),
        }
    }
    fn ceil(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match args_val[0] {
            Value::Number(num) => Ok(Value::Number(num.ceil())),

            _ => unreachable!(),
        }
    }
    fn trunc(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match args_val[0] {
            Value::Number(num) => Ok(Value::Number(num.trunc())),

            _ => unreachable!(),
        }
    }
    fn abs(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match args_val[0] {
            Value::Number(num) => Ok(Value::Number(num.abs())),

            _ => unreachable!(),
        }
    }
    fn sqrt(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match args_val[0] {
            Value::Number(num) => Ok(Value::Number(num.sqrt())),

            _ => unreachable!(),
        }
    }
    fn pow(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::Number(num), Value::Number(power)) => Ok(Value::Number(num.powf(*power))),

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `{}` axpects the argument to be of type Number. Found {:?}.",
                    err_token, val
                ),
                err_token,
            )),
        }
    }
    //fn trunc_to(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
    //    match (&args_val[0], &args_val[1]) {
    //        (Value::Number(num), Value::Number(power)) => Ok(Value::Number(num.powf(*power))),
    //
    //        (_, val) => Err(Error::new(
    //            ErrorKind::Exception(TypeError),
    //            format!(
    //                "Native function `{}` axpects the argument to be of type Number. Found {:?}.",
    //                err_token, val
    //            ),
    //            err_token,
    //        )),
    //    }
    //}
    fn to_string(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match args_val[0] {
            Value::Number(num) => Ok(Value::String(num.to_string())),

            _ => unreachable!(),
        }
    }

}
