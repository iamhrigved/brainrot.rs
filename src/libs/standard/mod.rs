use std::rc::Rc;

mod map;

use super::NativeLib;

use crate::value::Value;

#[derive(Clone)]
pub struct StdLib {
    loaded_classes: Vec<Value>,
}

impl NativeLib for StdLib {
    fn get_loaded(&self) -> &Vec<Value> {
        &self.loaded_classes
    }

    fn get_loaded_mut(&mut self) -> &mut Vec<Value> {
        &mut self.loaded_classes
    }

    fn match_item(&self, item_name: &str) -> Option<Value> {
        let class = match item_name {
            "Map" => map::init_class(),

            _ => return None,
        };

        Some(Value::NativeClass(Rc::new(class)))
    }
}

impl StdLib {
    pub fn new() -> Self {
        Self {
            loaded_classes: Vec::with_capacity(1),
        }
    }
}
