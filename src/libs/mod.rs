pub mod list;
pub mod number;
pub mod prelude;
pub mod string;

use list::ListLib;
use number::NumberLib;
use prelude::Prelude;
use string::StringLib;

pub enum Library {
    Number(NumberLib),
    String(StringLib),
    List(ListLib),
    Prelude(Prelude),
}

impl Library {
    pub fn init_libs() -> Vec<Self> {
        let prelude = Library::Prelude(Prelude::new());
        let number_lib = Library::Number(NumberLib::new());
        let strign_lib = Library::String(StringLib::new());
        let list_lib = Library::List(ListLib::new());

        vec![prelude, number_lib, strign_lib, list_lib]
    }
}
