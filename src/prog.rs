use tracing::debug;
use wasm_bindgen::prelude::*;

use crate::{
    luafn::LuaFn,
    statement::Statement,
    tokenizing::{Token, TokenType, Tokenizer},
    value::Value,
    Result,
};
use std::{collections::HashMap, iter::Peekable};
use std::{fmt::Debug, sync::Arc};

#[derive(Debug, Default)]
pub struct LuaScope(pub HashMap<String, Value>);

impl LuaScope {
    fn new() -> Self {
        Self(HashMap::new())
    }
}

#[derive(Debug)]
pub struct LuaScopePair<'local, 'global> {
    pub local: &'local mut LuaScope,
    pub global: &'global mut LuaScope,
}

impl<'local, 'global> LuaScopePair<'local, 'global> {
    pub fn with_new_local(&'global mut self, local: &'local mut LuaScope) -> Self {
        Self {
            local,
            global: self.global,
        }
    }

    pub fn get(&self, key: &str) -> &Value {
        self.local
            .0
            .get(key)
            .or_else(|| self.global.0.get(key))
            .unwrap_or(&Value::Nil)
    }
}

#[derive(Debug)]
pub struct Prog<'scope, T: Iterator<Item = Token> + Debug> {
    source: Peekable<T>,
    global_scope: &'scope mut LuaScope,
    local_global_scope: LuaScope,
}

impl<T: Iterator<Item = Token> + Debug> Drop for Prog<'_, T> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            debug!("{:?}", self)
        }
    }
}

impl<'scope, T> Prog<'scope, T>
where
    T: Iterator<Item = Token> + Debug,
{
    pub fn register_function<F>(&mut self, name: String, f: F)
    where
        F: LuaFn + 'static,
    {
        self.global_scope
            .0
            .insert(name, Value::Function(Arc::new(f)));
    }
    pub fn register_arc_function(&mut self, name: String, f: Arc<dyn LuaFn>) {
        self.global_scope.0.insert(name, Value::Function(f));
    }

    pub fn set_global_scope(&mut self, scope: &'scope mut LuaScope) {
        self.global_scope = scope
    }

    pub fn run(&mut self) -> Result<()> {
        let mut scope_pair = LuaScopePair {
            global: &mut self.global_scope,
            local: &mut self.local_global_scope,
        };
        loop {
            let Some(peek) = self.source.peek() else {
                break Ok(());
            };

            if peek.tokentype == TokenType::LineBreak {
                self.source.next();
                continue;
            }

            let statement = Statement::parse(&mut self.source)?;
            statement.execute(&mut scope_pair)?;
        }
    }

    pub fn new(iter: T, scope: &'scope mut LuaScope) -> Self {
        Self {
            source: iter.peekable(),
            global_scope: scope,
            local_global_scope: LuaScope::default(),
        }
    }
}
