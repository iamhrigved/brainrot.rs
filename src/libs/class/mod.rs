use std::rc::Rc;

mod map;

use crate::value::native_class::NativeClass;

pub struct ClassLib {
    loaded_classes: Vec<Rc<NativeClass>>,
}

impl ClassLib {
    pub fn new() -> Self {
        Self {
            loaded_classes: Vec::with_capacity(1),
        }
    }

    fn match_class(&self, name: &str) -> Option<NativeClass> {
        match name {
            "Map" => Some(map::init_class()),

            _ => None,
        }
    }

    fn get_class(&mut self, class_name: &String) -> Option<Rc<NativeClass>> {
        let loaded_fun_pos = self
            .loaded_classes
            .iter()
            .position(|class_rc| &class_rc.name == class_name);

        if let Some(index) = loaded_fun_pos {
            return self.loaded_classes.get(index).cloned();
        }

        self.match_class(class_name).map(|fun| self.load_class(fun))
    }

    fn load_class(&mut self, native_class: NativeClass) -> Rc<NativeClass> {
        let class_rc = Rc::new(native_class);

        self.loaded_classes.push(Rc::clone(&class_rc));

        class_rc
    }
}
