use crate::error::{Error, ErrorType};
use crate::parser::Value;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    var_map: HashMap<String, Option<Value>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            var_map: HashMap::new(),
        }
    }
    pub fn define_var(&mut self, name: String, val: Option<Value>) {
        self.var_map.insert(name, val);
    }

    // returns Err(()) when variable is not found
    pub fn get_var(&self, name: &String) -> Result<Option<Value>, ()> {
        self.var_map.get(name).ok_or(()).cloned()
    }
}
