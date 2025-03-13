use std::cell::RefCell;
use std::rc::Rc;

use super::NativeLib;

use crate::value::{native_fun::NativeFun, sigma_fun::SigmaFun, Value};

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub struct ListLib {
    loaded_functions: Vec<Value>,
}

impl NativeLib for ListLib {
    fn get_loaded(&self) -> &Vec<Value> {
        &self.loaded_functions
    }

    fn get_loaded_mut(&mut self) -> &mut Vec<Value> {
        &mut self.loaded_functions
    }

    #[rustfmt::skip]
    fn match_item(&self, fun_name: &str) -> Option<Value> {
        let fun = match fun_name {
            "len" => NativeFun::new("len", (0, Some(0)), Self::len),
            "is_empty" => NativeFun::new("is_emtpy", (0, Some(0)), Self::is_empty),
            "contains" => NativeFun::new("contains", (1, Some(1)), Self::contains),
            "get_at" => NativeFun::new("get_at", (1, Some(1)), Self::get_at),
            "index_of" => NativeFun::new("index_of", (1, Some(1)), Self::index_of),
            "push" => NativeFun::new("push", (1, Some(1)), Self::push),
            "insert" => NativeFun::new("insert", (2, Some(2)), Self::insert),
            "remove" => NativeFun::new("remove", (1, Some(1)), Self::remove),
            "reverse" => NativeFun::new("reverse", (0, Some(0)), Self::reverse),
            "pop" => NativeFun::new("pop", (0, Some(0)), Self::pop),
            "clone" => NativeFun::new("clone", (0, Some(0)), Self::clone),
            "any" => NativeFun::new("any", (1, Some(1)), Self::any),
            "map" => NativeFun::new("map", (1, Some(1)), Self::map),
            "filter" => NativeFun::new("filter", (1, Some(1)), Self::filter),
            "extend" => NativeFun::new("extend", (1, Some(1)), Self::extend),
            "take" => NativeFun::new("take", (1, Some(1)), Self::take),
            "truncate" => NativeFun::new("truncate", (1, Some(1)), Self::truncate),

            _ => return None,
        };

        Some(Value::NativeFunction(None, Rc::new(fun)))
    }
}

impl ListLib {
    pub fn new() -> Self {
        Self {
            loaded_functions: Vec::with_capacity(4),
        }
    }

    // HELPER FUNCTIONS
    fn check_arity(
        fun: &SigmaFun,
        expected_arity: usize,
        native_fun_name: &str,
        err_token: &Token,
    ) -> Result<String> {
        let fun_name = match &fun.name {
            Some(str) => format!(" {}", str),
            None => "".to_string(),
        };

        if fun.arity != (expected_arity, Some(expected_arity)) {
            return Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "The function{} passed into `{}` should take exactly {} arguments. Found {}.",
                    fun_name, native_fun_name, expected_arity, fun.arity.0
                ),
                err_token,
            ));
        }

        Ok(fun_name)
    }
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
    fn check_list_len(index: usize, list_len: usize, err_token: &Token) -> Result<()> {
        if index >= list_len {
            return Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "The length of the List is {} but the index supplied is {}.",
                    list_len, index
                ),
                err_token,
            ));
        }
        Ok(())
    }

    // FUNCTIONS
    fn len(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::List(list_rc) => Ok(Value::Number(list_rc.borrow().len() as f64)),

            _ => unreachable!(),
        }
    }
    fn is_empty(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::List(list_rc) => Ok(Value::Boolean(list_rc.borrow().is_empty())),

            _ => unreachable!(),
        }
    }
    fn contains(mut args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        let val = args_val.pop().unwrap();

        match &args_val[0] {
            Value::List(list_rc) => Ok(Value::Boolean(list_rc.borrow().contains(&val))),

            _ => unreachable!(),
        }
    }
    fn get_at(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::List(list_rc), Value::Number(num)) => {
                let list = list_rc.borrow();

                let index = Self::check_index(*num, err_token)?;

                // return Nil if index not found
                Ok(list.get(index).cloned().unwrap_or(Value::Nil))
            }

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `get_at` expects its argument to be a Number. Found {:?}",
                    val
                ),
                err_token,
            )),
        }
    }
    fn index_of(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::List(list_rc), val) => {
                let list = list_rc.borrow();

                let index_opt = list.iter().position(|item| item == val);

                Ok(index_opt
                    .map(|index| Value::Number(index as f64))
                    .unwrap_or(Value::Nil))
            }

            _ => unreachable!(),
        }
    }
    fn pop(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::List(list_rc) => Ok(list_rc.borrow_mut().pop().unwrap_or(Value::Nil)),

            _ => unreachable!(),
        }
    }
    fn push(mut args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        let val = args_val.pop().unwrap();

        match &args_val[0] {
            Value::List(list_rc) => {
                list_rc.borrow_mut().push(val);

                Ok(Value::Nil)
            }

            _ => unreachable!(),
        }
    }
    fn insert(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let val = args_val.pop().unwrap();

        match (&args_val[0], &args_val[1]) {
            (Value::List(list_rc), Value::Number(num)) => {
                let mut list_mut = list_rc.borrow_mut();

                let index = Self::check_index(*num, err_token)?;
                Self::check_list_len(index, list_mut.len(), err_token)?;

                list_mut.insert(index, val);

                Ok(Value::Nil)
            }

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `insert` expects its 1st argument to be a Number. Found {:?}",
                    val
                ),
                err_token,
            )),
        }
    }
    fn remove(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        match (&args_val[0], &args_val[1]) {
            (Value::List(list_rc), Value::Number(num)) => {
                let mut list_mut = list_rc.borrow_mut();

                let index = Self::check_index(*num, err_token)?;
                Self::check_list_len(index, list_mut.len(), err_token)?;

                Ok(list_mut.remove(index))
            }

            (_, val) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Native function `remove` expects its argument to be a Number. Found {:?}",
                    val
                ),
                err_token,
            )),
        }
    }
    fn reverse(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
        match &args_val[0] {
            Value::List(list_rc) => {
                list_rc.borrow_mut().reverse();

                Ok(Value::Nil)
            }

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

        let fun_name = Self::check_arity(&fun_rc, 1, "any", err_token)?;

        let list = list_rc.borrow();

        for item in list.iter() {
            match fun_rc.call(vec![item.clone()])? {
                Value::Boolean(bool) => {
                    if bool {
                        return Ok(Value::Boolean(true));
                    }
                }

                val => {
                    return Err(Error::new(
                        ErrorKind::Exception(TypeError),
                        format!(
                        "The function{} passed into `any`, should return a Boolean. Found {:?}.",
                        fun_name, val
                    ),
                        err_token,
                    ))
                }
            }
        }

        Ok(Value::Boolean(false))
    }
    fn map(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let (list_rc, fun_rc) = match (&args_val[0], &args_val[1]) {
            (Value::List(list_rc), Value::Function(fun_rc)) => {
                (Rc::clone(list_rc), Rc::clone(fun_rc))
            }

            (_, val) => {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `map` expects the argument to be a Function. Found {}",
                        val
                    ),
                    err_token,
                ));
            }
        };

        let list = list_rc.borrow();

        Self::check_arity(&fun_rc, 1, "map", err_token)?;

        let mut ret = Vec::with_capacity(list.len());

        for item in list.iter() {
            ret.push(fun_rc.call(vec![item.clone()])?)
        }

        let ret_rc = Rc::new(RefCell::new(ret));
        let ret_val = Value::List(ret_rc);

        Ok(ret_val)
    }
    fn filter(args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let (list_rc, fun_rc) = match (&args_val[0], &args_val[1]) {
            (Value::List(list_rc), Value::Function(fun_rc)) => {
                (Rc::clone(list_rc), Rc::clone(fun_rc))
            }

            (_, val) => {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `filter` expects the argument to be a Function. Found {}",
                        val
                    ),
                    err_token,
                ));
            }
        };

        let fun_name = Self::check_arity(&fun_rc, 1, "filter", err_token)?;

        let list = list_rc.borrow();
        let mut ret = Vec::with_capacity(list.len());

        for item in list.iter() {
            match fun_rc.call(vec![item.clone()])? {
                Value::Boolean(bool) => {
                    if bool {
                        ret.push(item.clone())
                    }
                }

                val => {
                    return Err(Error::new(
                        ErrorKind::Exception(TypeError),
                        format!(
                        "The function{} passed into `filter`, should return a Boolean. Found {:?}.",
                        fun_name, val
                    ),
                        err_token,
                    ))
                }
            }
        }

        let ret_rc = Rc::new(RefCell::new(ret));
        let ret_val = Value::List(ret_rc);

        Ok(ret_val)
    }
    fn extend(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let add_list = match args_val.pop().unwrap() {
            Value::List(list_rc) => list_rc.borrow().clone(),

            val => {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `extend` expects the argument to be a List. Found {}",
                        val
                    ),
                    err_token,
                ))
            }
        };

        match &args_val[0] {
            Value::List(list_rc) => {
                list_rc.borrow_mut().extend(add_list);

                Ok(Value::Nil)
            }

            _ => unreachable!(),
        }
    }
    fn take(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let num = match args_val.pop().unwrap() {
            Value::Number(num) => num,

            val => {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `take` expects the argument to be a Number. Found {}",
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
                    "The argument to the native function `take` must be a positive-integer. Found `{}`",
                    num
                ),
                err_token,
            ));
        }

        let take_num = num as usize;

        match &args_val[0] {
            Value::List(list_rc) => {
                let mut list = list_rc.borrow().clone();

                list.truncate(take_num);
                let ret_list_rc = Rc::new(RefCell::new(list));

                Ok(Value::List(ret_list_rc))
            }

            _ => unreachable!(),
        }
    }
    fn truncate(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        let num = match args_val.pop().unwrap() {
            Value::Number(num) => num,

            val => {
                return Err(Error::new(
                    ErrorKind::Exception(TypeError),
                    format!(
                        "Native function `truncate` expects the argument to be a Number. Found {}",
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
                    "The argument to the native function `truncate` must be a positive-integer. Found `{}`",
                    num
                ),
                err_token,
            ));
        }

        let truncate_len = num as usize;

        match &args_val[0] {
            Value::List(list_rc) => {
                list_rc.borrow_mut().truncate(truncate_len);
                Ok(Value::Nil)
            }

            _ => unreachable!(),
        }
    }
}
