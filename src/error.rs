use std::backtrace::Backtrace;

use crate::tokenizing::Span;

#[derive(Debug)]
pub struct LuaError {
    pub backtrace: Backtrace,
    pub error_type: LuaErrorType,
    pub span: Option<Span>,
}
impl LuaError {
    pub fn unexpected_eof() -> Self {
        Self::new_without_span(LuaErrorType::UnexpectedEOF)
    }

    pub fn new_with_span(error_type: LuaErrorType, span: Span) -> Self {
        Self {
            span: Some(span),
            error_type,
            backtrace: Backtrace::capture(),
        }
    }
    pub fn new_without_span(error_type: LuaErrorType) -> Self {
        Self {
            span: None,
            error_type,
            backtrace: Backtrace::capture(),
        }
    }
}

#[derive(Debug)]
pub enum LuaErrorType {
    Unit,
    MalFormed,
    UnexpectedEOF,
    UndefinedVar(String),
    Nothing,
    NotAValue,
    ExpectedValue,
    NotANum,
    CompareDifferentTypes,
    WrongArgumentCount,
    NotAFunction,
}

impl From<LuaErrorType> for LuaError {
    fn from(value: LuaErrorType) -> Self {
        Self::new_without_span(value)
    }
}

pub type Result<T> = std::result::Result<T, LuaError>;
