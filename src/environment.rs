#![allow(clippy::new_without_default)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::error::{Error, ErrorKind, ExceptionKind::*};
use crate::token::Token;
use crate::value::sigma_instance::SigmaInstance;
use crate::value::Value;

struct EnvMap {
    environments: Vec<Environment>,
    id_to_index: HashMap<u64, usize>,
}

impl EnvMap {
    pub fn new() -> Self {
        Self {
            environments: vec![Environment::new(EnvType::Global, None)],
            id_to_index: HashMap::from([(0, 0)]),
        }
    }

    pub fn pop(&mut self) {
        let popped_env = self.environments.pop().unwrap();
    }

    pub fn push(&mut self, env: Environment, id: u64) {
        let last_index = self.environments.len();

        self.environments.push(env);
        self.id_to_index.insert(id, last_index);
    }

    pub fn last(&self) -> Option<&Environment> {
        self.environments.last()
    }

    pub fn get_at(&self, id: &u64) -> Option<&Environment> {
        let index = *self.id_to_index.get(id)?;

        self.environments.get(index)
    }

    pub fn get_mut_at(&mut self, id: &u64) -> Option<&mut Environment> {
        let index = *self.id_to_index.get(id)?;

        self.environments.get_mut(index)
    }

    pub fn get_ancestory(&self, id: &u64) -> Option<Vec<u64>> {
        let mut chain = Vec::with_capacity(4);
        chain.push(*id);

        let mut cur_env = self.get_at(id)?;

        while let Some(parent_id) = cur_env.parent_id {
            cur_env = self.get_at(&parent_id).unwrap();
            chain.push(parent_id);
        }

        chain.reverse();
        Some(chain)
    }
}

#[derive(Clone, Debug)]
enum EnvType {
    Block,
    Loop,
    Fun,
    Meth,
    Global,
}

#[derive(Clone, Debug)]
pub struct Environment {
    pub variables: HashMap<String, Option<Value>>,
    parent_id: Option<u64>,
    is_captured: bool,
    env_type: EnvType,
}

impl Environment {
    fn new(env_type: EnvType, parent_id: Option<u64>) -> Self {
        Self {
            variables: HashMap::new(),
            parent_id,
            is_captured: false,
            env_type,
        }
    }

    pub fn new_method(instance_rc: Rc<RefCell<SigmaInstance>>) -> Self {
        Self {
            variables: HashMap::from([("me".to_string(), Some(Value::Instance(instance_rc)))]),
            parent_id: None,
            is_captured: true,
            env_type: EnvType::Meth,
        }
    }

    pub fn define_var(&mut self, var_name: String, val: Option<Value>) -> bool {
        self.variables.insert(var_name, val).is_some()
    }

    pub fn get_var(&self, var_name: &String) -> Result<Option<Value>, ()> {
        self.variables.get(var_name).cloned().ok_or(())
    }

    pub fn get_var_mut(&mut self, var_name: &String) -> Result<&mut Option<Value>, ()> {
        self.variables.get_mut(var_name).ok_or(())
    }
}

pub struct EnvironmentStack {
    environments: EnvMap,
    current_ancestry: Vec<u64>,
    largest_id: u64,
}

impl EnvironmentStack {
    pub fn new() -> Self {
        Self {
            environments: EnvMap::new(),
            current_ancestry: vec![0],
            largest_id: 0,
        }
    }
    fn push_env(&mut self, env_type: EnvType) {
        let cur_env_id = *self.current_ancestry.last().unwrap();

        self.environments.push(
            Environment::new(env_type, Some(cur_env_id)),
            self.largest_id + 1,
        );

        self.current_ancestry.push(self.largest_id + 1);

        self.largest_id += 1;
    }

    pub fn add_meth_env(&mut self, instance_rc: Rc<RefCell<SigmaInstance>>) -> u64 {
        let cur_largest_id = self.largest_id;

        self.environments
            .push(Environment::new_method(instance_rc), cur_largest_id + 1);

        self.largest_id += 1;

        cur_largest_id + 1
    }

    pub fn push_block(&mut self) {
        self.push_env(EnvType::Block)
    }

    pub fn push_loop(&mut self) {
        self.push_env(EnvType::Loop)
    }

    pub fn push_fun(&mut self) {
        self.push_env(EnvType::Fun)
    }

    pub fn pop_env(&mut self) {
        let last_env = self.environments.last().unwrap();

        // if the last environment is not captured, pop it
        if !last_env.is_captured {
            self.environments.pop();
        }

        // go back 1 env
        self.current_ancestry.pop();
    }

    pub fn last(&self) -> &Environment {
        self.environments.last().unwrap()
    }

    pub fn set_cur_id(&mut self, id: u64) -> u64 {
        let old_id = *self.current_ancestry.last().unwrap();

        self.current_ancestry = self.environments.get_ancestory(&id).unwrap();

        old_id
    }

    pub fn get_cur_id(&mut self) -> u64 {
        *self.current_ancestry.last().unwrap()
    }

    pub fn mark_envs_uncaptured(&mut self, fun_ids: Vec<u64>) {
        let mut ids = HashSet::with_capacity(6);

        for id in fun_ids {
            let ancestry = self.environments.get_ancestory(&id).unwrap();

            ids.extend(ancestry.into_iter());
        }

        for id in &ids {
            self.environments.get_mut_at(id).unwrap().is_captured = false;
        }
    }

    pub fn mark_cur_env_captured(&mut self) {
        for id in &self.current_ancestry {
            self.environments.get_mut_at(id).unwrap().is_captured = true
        }
    }

    pub fn in_loop(&self) -> bool {
        for id in &self.current_ancestry {
            let env_type = &self.environments.get_at(id).unwrap().env_type;

            if matches!(env_type, EnvType::Loop) {
                return true;
            }
        }

        false
    }
    pub fn in_fun(&self) -> bool {
        for id in self.current_ancestry.iter().rev() {
            let env_type = &self.environments.get_at(id).unwrap().env_type;

            if matches!(env_type, EnvType::Fun) {
                return true;
            }
        }

        false
    }

    pub fn define_var(&mut self, var_name: String, val_opt: Option<Value>) {
        let cur_id = self.current_ancestry.last().unwrap();

        let cur_env = self.environments.get_mut_at(cur_id).unwrap();

        cur_env.define_var(var_name, val_opt);
    }

    pub fn get_var(&mut self, var_name: &String, err_token: &Token) -> Result<Value, Error> {
        for id in self.current_ancestry.iter().rev() {
            let env = self.environments.get_at(id).unwrap();

            match env.get_var(var_name) {
                // if variable exists and is already initialized
                Ok(Some(val)) => return Ok(val),

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
            for id in self.current_ancestry.iter().rev() {
                let env = self.environments.get_mut_at(id).unwrap();

                if let Ok(val_opt) = env.get_var_mut(var_name) {
                    *val_opt = Some(new_val);
                    break;
                }
            }

            Ok(())
        }
    }

    // HELPER FUNCTIONS
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
