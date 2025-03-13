//use std::collections::HashMap;
//use std::rc::Rc;
//
//use crate::error::{Error, ErrorKind, ExceptionKind::*};
//use crate::token::Token;
//use crate::value::native_fun::NativeFun;
//use crate::value::{native_class::NativeClass, Value};
//
//type Result<T> = std::result::Result<T, Error>;
//
//pub struct ErrorClass;
//
//impl ErrorClass {
//    pub fn init_class() -> NativeClass {
//        let mut functions = HashMap::new();
//
//        let to_val = |native_fun| Value::NativeFunction(None, Rc::new(native_fun));
//
//        functions.insert(
//            "__new".to_string(),
//            to_val(NativeFun::new("__new", (0, Some(1)), Self::__new)),
//        );
//
//        NativeClass::new("Error".to_string(), HashMap::new())
//    }
//
//    fn __new(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
//        // TODO: ADD DEFAULT PARAMETER
//        let message_val = match args_val.pop().unwrap() {
//            Value::String(str) => Value::String(str),
//
//            val => {
//                return Err(Error::new(
//                    ErrorKind::Exception(TypeError),
//                    format!(
//                        "The class `Error` expects its argument to be a String. Found {}",
//                        val
//                    ),
//                    err_token,
//                ))
//            }
//        };
//
//        match args_val.pop().unwrap() {
//            Value::Instance(instance_rc) => instance_rc
//                .borrow_mut()
//                .set_property("message".to_string(), message_val),
//
//            _ => unreachable!(),
//        }
//
//        Ok(Value::Nil)
//    }
//}
