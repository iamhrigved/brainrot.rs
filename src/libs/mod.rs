pub mod list;
pub mod number;
pub mod prelude;
pub mod range;
pub mod standard;
pub mod string;

use crate::value::Value;

use list::ListLib;
use number::NumberLib;
use prelude::Prelude;
use range::RangeLib;
use standard::StdLib;
use string::StringLib;

pub trait NativeLib {
    // get a reference to the current loaded functions vector
    fn get_loaded(&self) -> &Vec<Value>;
    fn get_loaded_mut(&mut self) -> &mut Vec<Value>;

    // function which gets the function-objects
    fn match_item(&self, item_name: &str) -> Option<Value>;

    fn get_item(&mut self, item_name: &String) -> Option<Value> {
        let loaded_fun_pos = self.get_loaded().iter().position(|val| match val {
            Value::NativeFunction(_, fun) => &fun.name == item_name,

            Value::NativeClass(class) => &class.name == item_name,

            _ => false,
        });

        if let Some(index) = loaded_fun_pos {
            return self.get_loaded().get(index).cloned();
        }

        self.match_item(item_name).map(|fun| self.load_fun(fun))
    }

    fn load_fun(&mut self, val: Value) -> Value {
        self.get_loaded_mut().push(val.clone());

        val
    }
}

#[derive(Clone)]
pub enum Library {
    Number(NumberLib),
    String(StringLib),
    List(ListLib),
    Range(RangeLib),
    Prelude(Prelude),
    Standard(StdLib),
}

impl Library {
    pub fn init_libs() -> Vec<Self> {
        let prelude = Library::Prelude(Prelude::new());
        let number_lib = Library::Number(NumberLib::new());
        let strign_lib = Library::String(StringLib::new());
        let list_lib = Library::List(ListLib::new());
        let range_lib = Library::Range(RangeLib::new());
        let standard_lib = Library::Standard(StdLib::new());

        vec![
            prelude,
            number_lib,
            strign_lib,
            list_lib,
            range_lib,
            standard_lib,
        ]
    }
}
