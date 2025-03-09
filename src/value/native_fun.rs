use super::Value;
use crate::error::Error;
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct NativeFun {
    pub name: String,
    pub arity: (usize, Option<usize>),
    call_fun: fn(Vec<Value>, &Token) -> Result<Value>,
}

impl std::fmt::Display for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFun {}(## private ##)>", self.name)
    }
}

impl NativeFun {
    pub fn new(
        name: &str,
        arity: (usize, Option<usize>),
        call_fun: fn(Vec<Value>, &Token) -> Result<Value>,
    ) -> Self {
        Self {
            name: name.to_owned(),
            arity,
            call_fun,
        }
    }

    pub fn call(
        &self,
        self_opt: Option<Value>,
        mut args_val: Vec<Value>,
        err_token: &Token,
    ) -> Result<Value> {
        // add the self value to the args if there is a self
        if let Some(self_val) = self_opt {
            args_val.insert(0, self_val)
        }

        (self.call_fun)(args_val, err_token)
    }
}
