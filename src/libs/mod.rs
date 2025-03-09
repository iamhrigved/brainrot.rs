use std::rc::Rc;

pub mod list;
pub mod number;
pub mod prelude;
pub mod range;
pub mod string;

use crate::value::native_fun::NativeFun;

use list::ListLib;
use number::NumberLib;
use prelude::Prelude;
use range::RangeLib;
use string::StringLib;

pub trait NativeLib {
    // get a reference to the current loaded functions vector
    fn get_loaded(&self) -> &Vec<Rc<NativeFun>>;
    fn get_loaded_mut(&mut self) -> &mut Vec<Rc<NativeFun>>;

    // function which gets the function-objects
    fn match_function(&self, fun_name: &str) -> Option<NativeFun>;

    fn get_function(&mut self, fun_name: &String) -> Option<Rc<NativeFun>> {
        let loaded_fun_pos = self
            .get_loaded()
            .iter()
            .position(|fun_rc| &fun_rc.name == fun_name);

        if let Some(index) = loaded_fun_pos {
            return self.get_loaded().get(index).cloned();
        }

        self.match_function(fun_name).map(|fun| self.load_fun(fun))
    }

    fn load_fun(&mut self, sigma_fun: NativeFun) -> Rc<NativeFun> {
        let fun_rc = Rc::new(sigma_fun);

        self.get_loaded_mut().push(Rc::clone(&fun_rc));

        fun_rc
    }
}

pub enum Library {
    Number(NumberLib),
    String(StringLib),
    List(ListLib),
    Range(RangeLib),
    Prelude(Prelude),
}

impl Library {
    pub fn init_libs() -> Vec<Self> {
        let prelude = Library::Prelude(Prelude::new());
        let number_lib = Library::Number(NumberLib::new());
        let strign_lib = Library::String(StringLib::new());
        let list_lib = Library::List(ListLib::new());
        let range_lib = Library::Range(RangeLib::new());

        vec![prelude, number_lib, strign_lib, list_lib, range_lib]
    }
}
