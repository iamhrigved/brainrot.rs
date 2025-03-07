use std::cell::RefCell;
use std::rc::Rc;

use super::{sigma_instance::SigmaInstance, Value};
use std::collections::HashMap;

pub struct SigmaClass {
    pub name: String,
    pub properties: HashMap<String, Value>,
}

impl std::fmt::Display for SigmaClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class: {}", self.name)
    }
}

impl SigmaClass {
    pub fn new(name: String, properties: HashMap<String, Value>) -> Self {
        Self { name, properties }
    }
    pub fn new_instance(&mut self, name: Option<String>) -> Rc<RefCell<SigmaInstance>> {
        let instance = SigmaInstance::new(name, self.clone());
        let instance_rc = Rc::new(RefCell::new(instance));

        instance_rc.borrow().define_me(Rc::clone(&instance_rc));

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
        let mut properties = HashMap::with_capacity(8);

        for (var_name, val) in &self.properties {
            let val_clone = match val {
                // deep-clone only in case of a function
                Value::Function(fun_rc) => {
                    let fun_clone = fun_rc.borrow().clone();
                    let fun_clone_rc = Rc::new(RefCell::new(fun_clone));

                    Value::Function(fun_clone_rc)
                }

                // regular clone for other values
                val => val.clone(),
            };

            properties.insert(var_name.clone(), val_clone);
        }

        Self { name, properties }
    }
}
