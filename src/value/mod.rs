use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub mod instance;
pub mod native_class;
pub mod native_fun;
pub mod sigma_class;
pub mod sigma_fun;

use instance::Instance;
use native_class::NativeClass;
use native_fun::NativeFun;
use sigma_class::SigmaClass;
use sigma_fun::SigmaFun;

// only List and Instance require RefCell<T>
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    List(Rc<RefCell<Vec<Value>>>),
    Range(i64, i64),
    Type(String),
    Class(Rc<SigmaClass>),
    NativeClass(Rc<NativeClass>),
    Instance(Rc<RefCell<Instance>>),
    Exception(String),
    Function(Rc<SigmaFun>),
    NativeFunction(Option<Box<Value>>, Rc<NativeFun>), // self and function
    Nil,
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            // if the value is an Rc, return the cloned Rc
            Value::List(list_rc) => Value::List(Rc::clone(list_rc)),
            Value::Instance(instance_rc) => Value::Instance(Rc::clone(instance_rc)),

            Value::Function(fun_rc) => Value::Function(Rc::clone(fun_rc)),
            Value::NativeFunction(self_val, fun_rc) => {
                Value::NativeFunction(self_val.clone(), Rc::clone(fun_rc))
            }
            Value::Class(class_rc) => Value::Class(Rc::clone(class_rc)),
            Value::NativeClass(class_rc) => Value::NativeClass(Rc::clone(class_rc)),

            Value::Number(num) => Value::Number(*num),
            Value::Boolean(bool) => Value::Boolean(*bool),
            Value::String(str) => Value::String(str.clone()),
            Value::Range(num1, num2) => Value::Range(*num1, *num2),
            Value::Exception(str) => Value::Exception(str.clone()),
            Value::Type(str) => Value::Type(str.clone()),

            Value::Nil => Value::Nil,
        }
    }
}

// for SigmaClass::clone(&self)
impl Value {
    pub fn deep_clone(&self) -> Self {
        match self {
            Value::List(list_rc) => Value::List(Rc::new(RefCell::new(list_rc.borrow().clone()))),
            Value::Instance(instance_rc) => {
                Value::Instance(Rc::new(RefCell::new(instance_rc.borrow().clone())))
            }

            Value::Class(class_rc) => Value::Class(Rc::new(class_rc.as_ref().clone())),
            Value::Function(fun_rc) => Value::Function(Rc::new(fun_rc.as_ref().clone())),

            // Never deep-clone native functions and classes
            Value::NativeClass(class_rc) => Value::NativeClass(Rc::clone(class_rc)),
            Value::NativeFunction(self_opt, lib_fun_rc) => Value::NativeFunction(
                // deep clone the self value
                self_opt
                    .as_ref()
                    .map(|self_val| Box::new(self_val.deep_clone())),
                // never deep clone a lib function
                Rc::clone(lib_fun_rc),
            ),

            Value::Number(num) => Value::Number(*num),
            Value::Boolean(bool) => Value::Boolean(*bool),
            Value::String(str) => Value::String(str.clone()),
            Value::Range(num1, num2) => Value::Range(*num1, *num2),
            Value::Exception(str) => Value::Exception(str.clone()),
            Value::Type(str) => Value::Type(str.clone()),

            Value::Nil => Value::Nil,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            Self::Number(num) => num.to_string(),
            Self::String(str) => str.to_string(),
            Self::Range(num1, num2) => format!("{}..{}", num1, num2),
            Self::Boolean(bool) => bool.to_string(),
            Self::List(list_rc) => {
                let list = list_rc.borrow();

                if list.is_empty() {
                    return write!(f, "[]");
                }

                let mut print_string = "[".to_string();

                // leave the last item
                for item in list.iter().take(list.len() - 1) {
                    print_string.push_str(&format!("{}, ", item));
                }

                let last_item = list.last().unwrap();

                print_string += &format!("{}]", last_item);

                print_string
            }
            Self::Type(type_name) => format!("Type <{}>", type_name),
            Self::Instance(instance_rc) => instance_rc.borrow().to_string(),
            Self::Class(class_rc) => class_rc.as_ref().to_string(),
            Self::NativeClass(class_rc) => class_rc.as_ref().to_string(),
            Self::Exception(exception_message) => exception_message.to_string(),
            Self::Function(sigma_fun) => sigma_fun.as_ref().to_string(),
            Self::NativeFunction(_, fun_rc) => fun_rc.as_ref().to_string(),
            Self::Nil => "Nil".to_string(),
        };
        write!(f, "{}", val)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Self::Number(_) => "Number",
            Self::String(_) => "String",
            Self::Range(_, _) => "Range",
            Self::Boolean(_) => "Boolean",
            Self::List(_) => "List",
            Self::Class(class_rc) => &class_rc.as_ref().to_string(),
            Self::NativeClass(class_rc) => &class_rc.as_ref().to_string(),
            Self::Instance(instance_rc) => &instance_rc.borrow().to_string(),
            Self::Function(_) => "Function",
            Self::NativeFunction(_, _) => "NativeFunction",
            Self::Exception(_) => "Exception",
            Self::Type(_) => &self.to_string(), // Type <Number>
            Self::Nil => "Nil",
        };
        write!(f, "{}", str)
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Type(a), Value::Type(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::List(a_rc), Value::List(b_rc)) => {
                let a = a_rc.borrow();
                let b = b_rc.borrow();

                if a.len() != b.len() {
                    return false;
                }
                for i in 0..a.len() {
                    if a[i] != b[i] {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}
