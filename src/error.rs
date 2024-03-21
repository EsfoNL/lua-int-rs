use crate::tokenizing::Span;

#[derive(Debug)]
pub enum LuaError {
    Unit,
    MalFormed(Span),
    UnexpectedEOF,
    UndefinedVar(String),
    Nothing,
}
