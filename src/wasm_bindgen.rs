use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use js_sys::JSON;
use wasm_bindgen::prelude::*;

use crate::{luafn::LuaFn, prog::LuaScope, tokenizing::Tokenizer, value::Value, Prog};
use crate::{LuaError, Result};
static mut SCOPE: Mutex<Option<LuaScope>> = Mutex::new(None);

#[wasm_bindgen]
pub fn run(code: &str) -> std::result::Result<(), String> {
    let mut lock = unsafe { SCOPE.lock().unwrap() };
    let map = if let Some(ref mut v) = *lock {
        v
    } else {
        *lock = Some(LuaScope::default());
        lock.as_mut().unwrap()
    };
    let mut prog = Prog::new(Tokenizer::from(code.chars()), map);
    prog.run().map_err(|e| format!("{e:?}"))
}

#[wasm_bindgen]
pub fn register_print(fun: &js_sys::Function) {
    let mut lock = unsafe { SCOPE.lock().unwrap() };
    let fun = fun.clone();
    let map = if let Some(ref mut v) = *lock {
        v
    } else {
        *lock = Some(LuaScope::default());
        lock.as_mut().unwrap()
    };
    map.0.insert(
        String::from("print"),
        Value::Function(Arc::new(move |e: Vec<Value>, v: &mut LuaScope| {
            let val = JsValue::from(e.get(0).unwrap_or(&Value::Nil).to_string());
            fun.call1(&JsValue::null(), &val)
                .map(|e| Value::Nil)
                .map_err(|e| {
                    crate::LuaError::new_without_span(crate::error::LuaErrorType::Else(format!(
                        "{e:?}"
                    )))
                })
        })),
    );
}

#[wasm_bindgen]
pub fn get_scope_var(name: &str) -> Option<String> {
    let mut lock = unsafe { SCOPE.lock().unwrap() };
    let map = if let Some(ref mut v) = *lock {
        v
    } else {
        *lock = Some(LuaScope::default());
        lock.as_mut().unwrap()
    };

    map.0.get(name).map(|e| e.to_string())
}

#[wasm_bindgen]
pub fn get_scope_var_names() -> Vec<String> {
    let mut lock = unsafe { SCOPE.lock().unwrap() };
    if let Some(ref mut v) = *lock {
        v.0.keys().cloned().collect()
    } else {
        vec![]
    }
}
