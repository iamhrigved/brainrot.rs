use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;
use crate::value::{native_class::NativeClass, native_fun::NativeFun, NativeData, Value};

type Result<T> = std::result::Result<T, Error>;

// used as key
#[derive(Clone, Eq, Hash, PartialEq)]
enum Hashable {
    Number(i64, u64), // integer and fractional part
    String(String),
    Boolean(bool),
}

// main function
pub fn init_class() -> NativeClass {
    let mut functions = HashMap::new();

    let mut insert = |name: &str,
                      arity: (usize, Option<usize>),
                      fun: fn(Vec<Value>, &Token) -> Result<Value>| {
        let native_fun = NativeFun::new(name, arity, fun);
        functions.insert(
            name.to_owned(),
            Value::NativeFunction(None, Rc::new(native_fun)),
        );
    };

    insert("insert", (2, Some(2)), map_insert);
    insert("get", (1, Some(1)), map_get);
    insert("remove", (1, Some(1)), map_remove);
    insert("contains_key", (1, Some(1)), map_contains_key);
    insert("len", (0, Some(0)), map_len);
    insert("is_empty", (0, Some(0)), map_is_empty);
    insert("clear", (0, Some(0)), map_clear);
    insert("keys", (0, Some(0)), map_keys);
    insert("values", (0, Some(0)), map_values);

    NativeClass::new("Map".to_string(), functions, Some(create_data))
}

// data initializer
fn create_data() -> Box<dyn NativeData> {
    let hashmap: HashMap<Hashable, Value> = HashMap::with_capacity(4);
    Box::new(hashmap)
}

// HELPER FUNCTIONS
fn val_to_key(val: Value, err_token: &Token) -> Result<Hashable> {
    let key = match val {
        Value::String(str) => Hashable::String(str),
        Value::Boolean(bool) => Hashable::Boolean(bool),
        Value::Number(num) => {
            let nums = num
                .to_string()
                .split(".")
                .map(|num| num.parse::<i64>().unwrap_or(0))
                .collect::<Vec<_>>();

            let int = nums[0];
            let fract = *nums.get(1).unwrap_or(&0);

            Hashable::Number(int, fract as u64)
        }

        val => {
            return Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!(
                    "Key can only be of type Number, String or Boolean. Found {:?}.",
                    val
                ),
                err_token,
            ))
        }
    };

    Ok(key)
}
fn key_to_val(key: Hashable) -> Value {
    match key {
        Hashable::Number(int, fract) => {
            let fract_dec = fract as f64 / (fract.ilog10() + 1) as f64;

            Value::Number(int as f64 + fract_dec)
        }

        Hashable::String(str) => Value::String(str),

        Hashable::Boolean(bool) => Value::Boolean(bool),
    }
}

// FUNCTIONS
fn map_insert(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let val = args_val.pop().unwrap();

    let key = val_to_key(args_val.pop().unwrap(), err_token)?;

    let mut instance = instance_rc.borrow_mut();

    let data_mut = instance.get_data_mut().unwrap();
    let hashmap = data_mut.downcast_mut::<HashMap<Hashable, Value>>().unwrap();
    hashmap.insert(key, val);

    Ok(Value::Nil)
}
fn map_get(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let key = val_to_key(args_val.pop().unwrap(), err_token)?;

    let instance = instance_rc.borrow();

    let data_mut = instance.get_data_ref().unwrap();
    let hashmap = data_mut.downcast_ref::<HashMap<Hashable, Value>>().unwrap();

    Ok(hashmap.get(&key).cloned().unwrap_or(Value::Nil))
}
fn map_remove(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let key = val_to_key(args_val.pop().unwrap(), err_token)?;

    let mut instance = instance_rc.borrow_mut();

    let data_mut = instance.get_data_mut().unwrap();
    let hashmap = data_mut.downcast_mut::<HashMap<Hashable, Value>>().unwrap();

    Ok(hashmap.remove(&key).unwrap_or(Value::Nil))
}
fn map_contains_key(mut args_val: Vec<Value>, err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let key = val_to_key(args_val.pop().unwrap(), err_token)?;

    let instance = instance_rc.borrow();

    let data_ref = instance.get_data_ref().unwrap();
    let hashmap = data_ref.downcast_ref::<HashMap<Hashable, Value>>().unwrap();

    Ok(Value::Boolean(hashmap.contains_key(&key)))
}
fn map_len(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let instance = instance_rc.borrow();

    let data_ref = instance.get_data_ref().unwrap();
    let hashmap = data_ref.downcast_ref::<HashMap<Hashable, Value>>().unwrap();

    Ok(Value::Number(hashmap.len() as f64))
}
fn map_is_empty(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let instance = instance_rc.borrow();

    let data_ref = instance.get_data_ref().unwrap();
    let hashmap = data_ref.downcast_ref::<HashMap<Hashable, Value>>().unwrap();

    Ok(Value::Boolean(hashmap.is_empty()))
}
fn map_clear(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let mut instance = instance_rc.borrow_mut();

    let data_mut = instance.get_data_mut().unwrap();
    let hashmap = data_mut.downcast_mut::<HashMap<Hashable, Value>>().unwrap();

    hashmap.clear();

    Ok(Value::Nil)
}
fn map_keys(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let instance = instance_rc.borrow();

    let data_mut = instance.get_data_ref().unwrap();
    let hashmap = data_mut.downcast_ref::<HashMap<Hashable, Value>>().unwrap();

    let keys_list = hashmap.keys().cloned().map(key_to_val).collect();
    let list_rc = Rc::new(RefCell::new(keys_list));

    Ok(Value::List(list_rc))
}
fn map_values(args_val: Vec<Value>, _err_token: &Token) -> Result<Value> {
    let instance_rc = match &args_val[0] {
        Value::Instance(instance_rc) => Rc::clone(instance_rc),
        _ => unreachable!(),
    };

    let instance = instance_rc.borrow();

    let data_mut = instance.get_data_ref().unwrap();
    let hashmap = data_mut.downcast_ref::<HashMap<Hashable, Value>>().unwrap();

    let values_list = hashmap.values().cloned().collect();
    let list_rc = Rc::new(RefCell::new(values_list));

    Ok(Value::List(list_rc))
}
