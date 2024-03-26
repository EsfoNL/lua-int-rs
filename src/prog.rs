use crate::{
    expr::Expr,
    luafn::LuaFn,
    statement::Statement,
    tokenizing::{BraceType, Operator, Span, Token, TokenType},
    value::Value,
    LuaError, Result,
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
pub struct Prog<T: Iterator<Item = Token> + Debug> {
    source: Peekable<T>,
    global_scope: LuaScope,
    local_global_scope: LuaScope,
}

impl<T: Iterator<Item = Token> + Debug> Drop for Prog<T> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!("{:?}", self)
        }
    }
}

impl<T> Prog<T>
where
    T: Iterator<Item = Token> + Debug,
{
    pub fn from(iter: T) -> Self
    where
        T: Iterator<Item = Token>,
    {
        Self {
            source: iter.peekable(),
            global_scope: LuaScope::new(),
            local_global_scope: LuaScope::new(),
        }
    }

    pub fn register_function<F>(&mut self, name: String, f: F)
    where
        F: LuaFn + 'static,
    {
        self.global_scope
            .0
            .insert(name, Value::Function(Arc::new(f)));
    }

    pub fn run(&mut self) -> Result<()> {
        let mut scope_pair = LuaScopePair {
            global: &mut self.global_scope,
            local: &mut self.local_global_scope,
        };
        loop {
            if self.source.peek().is_none() {
                break Ok(());
            }
            let statement = Statement::parse(&mut self.source)?;
            statement.execute(&mut scope_pair)?;
        }
    }
}
