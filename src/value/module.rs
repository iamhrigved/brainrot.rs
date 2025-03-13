use std::collections::HashMap;

use super::Value;

use crate::libs::NativeLib;

pub struct Module {
    pub name: String,
    exports: HashMap<String, Value>,
    native_module: Option<Box<dyn NativeLib>>,
}

impl Module {
    pub fn new(mod_name: String, exports: HashMap<String, Value>) -> Self {
        Self {
            name: mod_name,
            exports,
            native_module: None,
        }
    }

    pub fn new_native(mod_name: String, native_module: Box<dyn NativeLib>) -> Self {
        Self {
            name: mod_name,
            exports: HashMap::with_capacity(0),
            native_module: Some(native_module),
        }
    }

    pub fn get_property(&mut self, name: &String) -> Option<Value> {
        if let Some(val) = self.exports.get(name) {
            return Some(val.clone());
        }

        if let Some(native_module) = &mut self.native_module {
            return native_module.get_item(name);
        }

        None
    }

    pub fn set_property(&mut self, name: String, val: Value) {
        self.exports.insert(name, val);
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Module: <{}>", self.name)
    }
}
