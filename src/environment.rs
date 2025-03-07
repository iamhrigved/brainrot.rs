#![allow(clippy::new_without_default)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;
use crate::value::Value;

#[derive(Clone, Debug)]
enum EnvType {
    Block,
    Loop,
    Fun,
}

#[derive(Clone, Debug)]
pub struct Environment {
    pub variables: HashMap<String, Option<Value>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
    pub in_loop: bool,
    pub in_fun: bool,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        let global_env = Self {
            variables: HashMap::new(),
            enclosing: None,
            in_loop: false,
            in_fun: false,
        };

        Rc::new(RefCell::new(global_env))
    }
    pub fn new_from(environment: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let global_env = Self {
            variables: HashMap::new(),
            enclosing: Some(environment),
            in_loop: false,
            in_fun: false,
        };

        Rc::new(RefCell::new(global_env))
    }

    fn new_inside(enclosing: Rc<RefCell<Environment>>, env_type: EnvType) -> Rc<RefCell<Self>> {
        let mut in_loop = enclosing.borrow().in_loop;
        let mut in_fun = enclosing.borrow().in_fun;

        match &env_type {
            EnvType::Fun => in_fun = true,
            EnvType::Loop => in_loop = true,
            _ => (),
        }

        let new_env = Self {
            variables: HashMap::new(),
            enclosing: Some(enclosing),
            in_loop,
            in_fun,
        };

        Rc::new(RefCell::new(new_env))
    }

    pub fn new_block(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Self::new_inside(enclosing, EnvType::Block)
    }
    pub fn new_loop(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Self::new_inside(enclosing, EnvType::Loop)
    }
    pub fn new_fun(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Self::new_inside(enclosing, EnvType::Fun)
    }

    pub fn define_var(&mut self, var_name: String, val_opt: Option<Value>) {
        self.variables.insert(var_name, val_opt);
    }

    pub fn get_var(&self, var_name: &String, err_token: &Token) -> Result<Value, Error> {
        match self.variables.get(var_name).ok_or(()) {
            // if variable exists and is already initialized
            Ok(Some(val)) => return Ok(val.clone()),

            // if variable exist but not initialized
            Ok(None) => {
                return Err(Error::new(
                    ErrorKind::Exception(NameError),
                    format!("Binding `{}` is not initilized.", var_name),
                    err_token,
                ))
            }

            _ => (),
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get_var(var_name, err_token);
        }

        // if no variable with the name is found
        Err(Error::new(
            ErrorKind::Exception(NameError),
            format!("Can't find value `{}` in the current scope.", var_name),
            err_token,
        ))
    }

    pub fn assign_var(
        &mut self,
        var_name: &String,
        indices_opt: Option<Vec<usize>>,
        new_val: Value,
        err_tokens: &[Token],
    ) -> Result<(), Error> {
        // for index assign
        if let Some(indices) = indices_opt {
            self.assign_index_var(var_name, indices, new_val, &err_tokens[1..])
        }
        // for regular assign
        else {
            if let Some(val_opt) = self.variables.get_mut(var_name) {
                *val_opt = Some(new_val);
                return Ok(());
            }

            if let Some(enclosing) = &self.enclosing {
                return enclosing.borrow_mut().assign_var(
                    var_name,
                    indices_opt,
                    new_val,
                    err_tokens,
                );
            }

            Err(Error::new(
                ErrorKind::Exception(NameError),
                format!("Can't find value `{}` in the current scope.", var_name),
                &err_tokens[0],
            ))
        }
    }

    fn assign_index_var(
        &mut self,
        var_name: &String,
        indices: Vec<usize>,
        new_val: Value,
        err_tokens: &[Token],
    ) -> Result<(), Error> {
        match self.get_var(var_name, &err_tokens[0])? {
            v if !matches!(v, Value::List(_)) => Err(Error::new(
                ErrorKind::Exception(TypeError),
                format!("Can't index value `{}` which is of type {:?}.", var_name, v),
                &err_tokens[0],
            )),

            Value::List(val_list) => {
                let mut cur_list = val_list;

                // the error print_string which will be called when there is an error
                let get_err_string = |cur_i: usize| {
                    let mut print_string = var_name.to_string();

                    for j in indices.iter().take(cur_i) {
                        print_string.push_str(&format!("[{}]", indices[*j]))
                    }

                    print_string
                };

                // loop till we get the last list to be indexed
                for i in 0..indices.len() - 1 {
                    cur_list = {
                        let cur_list_borrow = cur_list.borrow();
                        // borrow scope starts

                        match cur_list_borrow.get(indices[i]) {
                            None => {
                                return Err(Error::new(
                                    ErrorKind::Exception(IndexError),
                                    format!(
                                        "The length of the List `{}` is {} but index supplied is {}.",
                                        get_err_string(i),
                                        cur_list_borrow.len(),
                                        indices[i]
                                    ),
                                    &err_tokens[i],
                                ));
                            }

                            // go deeper into the list
                            Some(Value::List(list_rc)) => Rc::clone(list_rc),

                            // since we are looping till we reach the final list, all lookups
                            // SHOULD be of type List
                            Some(v) => {
                                return Err(Error::new(
                                    ErrorKind::Exception(TypeError),
                                    format!(
                                        "Can't index value `{}` which is of type {:?}.",
                                        get_err_string(i),
                                        v
                                    ),
                                    &err_tokens[i],
                                ));
                            }
                        }

                        // borrow scope ends and it is dropped
                    };
                }

                let last_index = *indices.last().unwrap();
                let list_len = cur_list.borrow().len();
                match cur_list.borrow_mut().get_mut(last_index) {
                    Some(v) => *v = new_val,

                    None => {
                        return Err(Error::new(
                            ErrorKind::Exception(IndexError),
                            format!(
                                "The length of the List `{}` is {} but the index supplied is {}.",
                                get_err_string(indices.len() - 1),
                                list_len,
                                last_index
                            ),
                            err_tokens.last().unwrap(),
                        ))
                    }
                }

                Ok(())
            }

            _ => panic!("ERROR"), // not possible
        }
    }
}
