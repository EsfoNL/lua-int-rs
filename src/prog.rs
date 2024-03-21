use clap::error::Result;

use crate::{
    expr::Expr,
    statement::Statement,
    tokenizing::{BraceType, Operator, Span, Token, TokenType},
    LuaError,
};
use std::{collections::HashMap, iter::Peekable};
use std::{fmt::Debug, sync::Arc};

#[derive(Debug)]
pub struct LuaScope(pub HashMap<String, Value>);

impl LuaScope {
    fn new() -> Self {
        Self(HashMap::new())
    }
}

#[derive(Debug, Clone)]
pub enum LuaFn {
    External(fn(Vec<Value>) -> Result<Value, LuaError>),
    Internal(Arc<LuaFnDef>),
}

impl LuaFn {
    fn call(&self, input: Vec<Value>, global_scope: &mut LuaScope) -> Result<Value, LuaError> {
        match self {
            LuaFn::External(f) => f(input),
            LuaFn::Internal(v) => {
                let mut local_scope = LuaScope::new();
                for (index, name) in v.names.iter().enumerate() {
                    if let Some(v) = input.get(index) {
                        local_scope.0.insert(name.clone(), v.clone());
                    } else {
                        local_scope.0.insert(name.clone(), Value::Nil);
                    }
                }
                for i in v.body.iter() {
                    if let Some(v) = i.execute(global_scope, &mut local_scope)? {
                        return Ok(v);
                    }
                }
                Ok(Value::Nil)
            }
        }
    }
}

impl PartialEq for LuaFn {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaFn::External(a), LuaFn::External(b)) => a == b,
            (LuaFn::Internal(a), LuaFn::Internal(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct LuaFnDef {
    names: Vec<String>,
    body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Function(LuaFn),
    Num(f64),
    String(String),
    Nil,
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

    pub fn register_function(
        &mut self,
        name: String,
        f: fn(Vec<Value>) -> Result<Value, LuaError>,
    ) {
        self.global_scope
            .0
            .insert(name, Value::Function(LuaFn::External(f)));
    }

    pub fn run(&mut self) -> Result<(), LuaError> {
        loop {
            if self.source.peek().is_none() {
                break Ok(());
            }
            let statement = Statement::parse(&mut self.source)?;
            statement.execute(&mut self.global_scope, &mut self.local_global_scope)?;
        }
    }
}
