use crate::prog::LuaScopePair;
use crate::Result;
use crate::{prog::LuaScope, statement::Statement, value::Value};
use std::fmt::Debug;
use std::sync::Mutex;

pub trait LuaFn {
    fn call(&self, args: Vec<Value>, global_scope: &mut LuaScope) -> Result<Value>;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

impl Debug for dyn LuaFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt(f)
    }
}

#[derive(Debug)]
pub struct LuaCodeFn {
    pub names: Vec<String>,
    pub body: Vec<Statement>,
}

impl LuaFn for LuaCodeFn {
    fn call(&self, args: Vec<Value>, global_scope: &mut LuaScope) -> Result<Value> {
        let mut local_scope = LuaScope::default();
        for (name, value) in self.names.iter().zip(args) {
            local_scope.0.insert(name.to_string(), value);
        }
        let mut scope_pair = LuaScopePair {
            local: &mut local_scope,
            global: global_scope,
        };
        for statement in self.body.iter() {
            if let Some(v) = statement.execute(&mut scope_pair)? {
                return Ok(v);
            }
        }
        Ok(Value::Nil)
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

impl<T> LuaFn for T
where
    T: Fn(Vec<Value>, &mut LuaScope) -> Result<Value>,
{
    fn call(&self, args: Vec<Value>, global_scope: &mut LuaScope) -> Result<Value> {
        self(args, global_scope)
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<external function>")
    }
}

impl<T> LuaFn for Mutex<T>
where
    T: FnMut(Vec<Value>, &mut LuaScope) -> Result<Value>,
{
    fn call(&self, args: Vec<Value>, global_scope: &mut LuaScope) -> Result<Value> {
        self.lock().unwrap()(args, global_scope)
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<external function>")
    }
}
