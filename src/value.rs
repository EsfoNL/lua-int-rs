use crate::error::LuaErrorType;
use crate::luafn::LuaFn;
use crate::Result;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Value {
    Function(Arc<dyn LuaFn>),
    Num(f64),
    String(String),
    Nil,
    Boolean(bool),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Function(slf), Self::Function(other)) => Arc::ptr_eq(slf, other),
            (Value::String(slf), Self::String(other)) => slf == other,
            (Value::Num(slf), Self::Num(other)) => slf == other,
            (Value::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}

impl Value {
    pub fn num(&self) -> Option<f64> {
        match self {
            Self::Num(v) => Some(*v),
            Self::String(v) => v.parse().ok(),
            _ => None,
        }
    }
    pub fn string(&self) -> Option<&String> {
        match self {
            Self::String(ref v) => Some(v),
            _ => None,
        }
    }

    pub fn bool(&self) -> Option<bool> {
        match self {
            Self::Boolean(ref v) => Some(*v),
            _ => None,
        }
    }

    pub fn function(&self) -> Option<Arc<dyn LuaFn>> {
        match self {
            Self::Function(v) => Some(v.clone()),
            _ => None,
        }
    }

    pub(crate) fn bool_nil_false(&self) -> Option<bool> {
        match self {
            Self::Boolean(ref v) => Some(*v),
            Self::Nil => Some(false),
            _ => None,
        }
    }

    pub fn lua_eq(&self, other: &Self) -> std::result::Result<bool, LuaErrorType> {
        match (self, other) {
            (Value::Function(slf), Self::Function(other)) => Ok(Arc::ptr_eq(slf, other)),
            (Value::String(slf), Self::String(other)) => Ok(slf == other),
            (Value::Num(slf), Self::Num(other)) => Ok(slf == other),
            (Value::Nil, Self::Nil) => Ok(true),
            _ => Err(LuaErrorType::CompareDifferentTypes),
        }
    }
}
