use std::iter::Peekable;

use crate::{
    expr::Expr,
    prog::{LuaScope, Value},
    tokenizing::{Token, TokenType},
    LuaError,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(String, Expr),
    Expr(Expr),
    Return(Expr),
}
impl Statement {
    pub fn execute(
        &self,
        global_scope: &mut LuaScope,
        local_scope: &mut LuaScope,
    ) -> Result<Option<Value>, LuaError> {
        match self {
            Statement::Assignment(name, value_expr) => {
                let res = value_expr.eval(global_scope, local_scope)?;
                global_scope.0.insert(name.clone(), res);
                Ok(None)
            }
            Statement::Expr(e) => e.eval(global_scope, local_scope).map(|_| None),
            Statement::Return(e) => e.eval(global_scope, local_scope).map(Some),
        }
    }

    pub fn parse<T>(source: &mut Peekable<T>) -> Result<Statement, LuaError>
    where
        T: Iterator<Item = Token>,
    {
        let val = source.next().ok_or(LuaError::UnexpectedEOF)?;
        match val.tokentype {
            TokenType::Ident(name) => {
                let val = source.next().ok_or(LuaError::UnexpectedEOF)?;
                match val.tokentype {
                    TokenType::EqAssign => {
                        let expr = Expr::parse(source)?;

                        println!("expr: {expr:?}");
                        if source
                            .peek()
                            .is_some_and(|v| v.tokentype == TokenType::LineBreak)
                        {
                            source.next();
                        }
                        Ok(Statement::Assignment(name, expr))
                    }
                    _ => Err(val.into_malformed()),
                }
            }
            _ => todo!(),
        }
    }
}
