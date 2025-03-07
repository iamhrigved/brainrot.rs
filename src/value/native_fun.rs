use super::Value;
use crate::error::Error;
use crate::token::Token;

type Result<T> = std::result::Result<T, Error>;

pub struct NativeFun {
    pub name: String,
    self_val: Option<Value>,
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
        name: String,
        arity: (usize, Option<usize>),
        call_fun: fn(Vec<Value>, &Token) -> Result<Value>,
    ) -> Self {
        Self {
            name,
            self_val: None,
            arity,
            call_fun,
        }
    }

    pub fn take_self(&mut self, self_val: Value) {
        self.self_val = Some(self_val);
    }

    pub fn call(&self, mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
        // add the self value to the args if there is a self
        if let Some(val) = &self.self_val {
            args_val.insert(0, val.clone());
        };

        (self.call_fun)(args_val, err_token)
    }
}
