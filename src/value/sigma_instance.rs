use super::sigma_class::SigmaClass;
use super::*;
use crate::environment::EnvironmentStack;

#[derive(Clone)]
pub struct SigmaInstance {
    name: Option<String>,
    class: SigmaClass,
}

impl SigmaInstance {
    pub fn new(name: Option<String>, class: SigmaClass) -> Self {
        Self { name, class }
    }
    pub fn get_property(&self, name: &String) -> Option<Value> {
        self.class.properties.get(name).cloned()
    }
    pub fn set_property(&mut self, name: String, val: Value) {
        self.class.properties.insert(name, val);
    }
    pub fn get_instance_name(&self) -> &str {
        match &self.name {
            Some(name) => name,
            None => "",
        }
    }
    pub fn get_class_name(&self) -> &str {
        &self.class.name
    }
    pub fn update_name(&mut self, new_name: String) {
        self.name = Some(new_name);
    }

    pub fn define_me(&self, me_rc: Rc<RefCell<SigmaInstance>>, env_stack: &mut EnvironmentStack) {
        let env_id = env_stack.add_meth_env(me_rc);

        for val in self.class.properties.values() {
            if let Value::Function(fun_rc) = val {
                fun_rc.borrow_mut().env_id = Some(env_id);
            }
        }
    }
}

impl std::fmt::Display for SigmaInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Instance: <{}>", self.class)
    }
}
