use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    instance::{ClassType, Instance},
    native_class::NativeClass,
    Value,
};

pub struct SigmaClass {
    pub name: String,
    superclass: Option<Box<ClassType>>,
    pub properties: HashMap<String, Value>,
}

impl std::fmt::Display for SigmaClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class: {}", self.name)
    }
}

impl SigmaClass {
    pub fn new(name: String, properties: HashMap<String, Value>) -> Self {
        Self {
            name,
            superclass: None,
            properties,
        }
    }
    pub fn new_subclass(
        name: String,
        sigma_class: SigmaClass,
        properties: HashMap<String, Value>,
    ) -> Self {
        Self {
            name,
            superclass: Some(Box::new(ClassType::Sigma(sigma_class))),
            properties,
        }
    }
    pub fn new_subclass_native(
        name: String,
        native_class: NativeClass,
        properties: HashMap<String, Value>,
    ) -> Self {
        Self {
            name,
            superclass: Some(Box::new(ClassType::Native(native_class))),
            properties,
        }
    }
    pub fn get_property(&self, name: &String) -> Option<Value> {
        self.properties.get(name).cloned()
    }
    pub fn new_instance(&self) -> Rc<RefCell<Instance>> {
        let instance = Instance::new(None, self.clone());

        let instance_rc = Rc::new(RefCell::new(instance));

        instance_rc.borrow_mut().define_me(Rc::clone(&instance_rc));

        instance_rc
    }
}

// in case of regular functions, we don't EVER need to deep-copy them (Rc::clone() is enough).
// but when we are actually defining a method, since it is bound to a unique `me` (its instance),
// we actually have to clone the function object itself to avoid collisions with other instances of
// the same class
impl Clone for SigmaClass {
    fn clone(&self) -> Self {
        let name = self.name.clone();
        let mut properties = HashMap::with_capacity(self.properties.len());

        self.properties.iter().for_each(|(name, val)| {
            properties.insert(name.clone(), val.deep_clone());
        });

        let superclass = self.superclass.clone();

        Self {
            name,
            superclass,
            properties,
        }
    }
}
