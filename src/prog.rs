use tracing::debug;

use crate::{
    luafn::LuaFn,
    peekable_n::{NPeekable, PeekableN},
    statement::Statement,
    str_interner::InternedStr,
    tokenizing::{Token, TokenType},
    value::Value,
    Result,
};
use std::fmt::Debug;
use std::{collections::HashMap, sync::Arc};

#[derive(Debug, Default)]
pub struct LuaScope(pub HashMap<InternedStr, Value>);

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
    source: PeekableN<T, 2>,
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
    pub fn register_function<F>(&mut self, name: &str, f: F)
    where
        F: LuaFn + 'static,
    {
        self.global_scope
            .0
            .insert(name.into(), Value::Function(Arc::new(f)));
    }
    pub fn register_arc_function(&mut self, name: &str, f: Arc<dyn LuaFn>) {
        self.global_scope.0.insert(name.into(), Value::Function(f));
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
            source: iter.peekable_n(),
            global_scope: scope,
            local_global_scope: LuaScope::default(),
        }
    }
}
