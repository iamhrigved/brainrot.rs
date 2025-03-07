pub mod list;
pub mod number;
pub mod prelude;

use number::NumberLib;
use prelude::Prelude;

pub enum Library {
    Number(NumberLib),
    Prelude(Prelude),
}
