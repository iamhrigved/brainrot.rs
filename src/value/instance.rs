use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use super::native_class::NativeInstanceData;
use super::{native_class::NativeClass, native_fun::NativeFun, sigma_class::SigmaClass, Value};

#[derive(Clone)]
enum ClassType {
    Sigma(SigmaClass),
    Native(NativeClass),
}

#[derive(Clone)]
pub struct Instance {
    name: Option<String>,
    class: ClassType,
    data: Option<Box<dyn NativeInstanceData>>,
}

impl Instance {
    pub fn new(name: Option<String>, class: SigmaClass) -> Self {
        Self {
            name,
            class: ClassType::Sigma(class),
            data: None,
        }
    }
    pub fn new_native(
        name: Option<String>,
        class: NativeClass,
        data: Option<Box<dyn NativeInstanceData>>,
    ) -> Self {
        Self {
            name,
            class: ClassType::Native(class),
            data,
        }
    }
    pub fn get_data_mut(&mut self) -> Option<&mut dyn Any> {
        self.data.as_mut().map(|data_box| data_box.as_any_mut())
    }
    pub fn get_data_ref(&self) -> Option<&dyn Any> {
        self.data.as_ref().map(|data_box| data_box.as_any_ref())
    }
    pub fn get_property(&self, name: &String) -> Option<Value> {
        match &self.class {
            ClassType::Sigma(sigma_class) => sigma_class.get_property(name),
            ClassType::Native(native_class) => native_class.get_property(name),
        }
    }
    pub fn set_property(&mut self, name: String, val: Value) -> Result<(), ()> {
        if let ClassType::Sigma(class) = &mut self.class {
            class.properties.insert(name, val);
            return Ok(());
        }

        Err(())
    }
    pub fn define_me(&mut self, me_rc: Rc<RefCell<Instance>>) {
        let instance_val = Value::Instance(me_rc);

        match &mut self.class {
            ClassType::Sigma(class) => class.properties.values_mut().for_each(|val| match val {
                // add the "me" variable in the functions
                Value::Function(fun_rc) => fun_rc
                    .captured_environment
                    .borrow_mut()
                    .define_var("me".to_string(), Some(instance_val.clone())),

                _ => (),
            }),

            ClassType::Native(class) => class.properties.values_mut().for_each(|val| match val {
                // add the "me" parameter in native functions
                Value::NativeFunction(self_opt, _) => {
                    *self_opt = Some(Box::new(instance_val.clone()))
                }

                _ => (),
            }),
        }
    }
    pub fn get_instance_name(&self) -> &str {
        match &self.name {
            Some(name) => name,
            None => "",
        }
    }
    pub fn get_class_name(&self) -> &str {
        match &self.class {
            ClassType::Sigma(sigma_class) => &sigma_class.name,
            ClassType::Native(native_class) => &native_class.name,
        }
    }
    pub fn update_name(&mut self, new_name: String) {
        self.name = Some(new_name);
    }
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.class {
            ClassType::Sigma(sigma_class) => write!(f, "Instance: <{}>", sigma_class),
            ClassType::Native(native_class) => write!(f, "Instance: <{}>", native_class),
        }
    }
}
