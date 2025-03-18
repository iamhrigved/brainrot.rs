use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{instance::Instance, NativeInstanceData, Value};

#[derive(Clone)]
pub struct NativeClass {
    pub name: String,
    pub properties: HashMap<String, Value>,
    pub data_creator: Option<fn() -> Box<dyn NativeInstanceData>>,
}

impl NativeClass {
    pub fn new(
        name: String,
        properties: HashMap<String, Value>,
        data_creator: Option<fn() -> Box<dyn NativeInstanceData>>,
    ) -> Self {
        Self {
            name,
            properties,
            data_creator,
        }
    }

    pub fn get_property(&self, name: &String) -> Option<Value> {
        self.properties.get(name).cloned()
    }

    pub fn new_instance(&self) -> Rc<RefCell<Instance>> {
        let data_opt = self.data_creator.as_ref().map(|fun| fun());

        let instance = Instance::new_native(None, self.clone(), data_opt);

        let instance_rc = Rc::new(RefCell::new(instance));

        instance_rc.borrow_mut().define_me(Rc::clone(&instance_rc));

        instance_rc
    }
}

impl std::fmt::Display for NativeClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeClass: {}", self.name)
    }
}
