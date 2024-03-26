use std::iter::Peekable;

use crate::{
    error::LuaErrorType,
    expr::{Expr, ExprElement},
    prog::{LuaScope, LuaScopePair},
    tokenizing::{BraceType, Token, TokenType},
    value::Value,
    LuaError,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(String, Expr),
    Expr(Expr),
    Return(Expr),
}
impl Statement {
    pub fn execute(&self, scopes: &mut LuaScopePair) -> Result<Option<Value>, LuaError> {
        match self {
            Statement::Assignment(name, value_expr) => {
                let res = value_expr.eval(scopes)?;
                scopes.global.0.insert(name.clone(), res);
                Ok(None)
            }
            Statement::Expr(e) => e.eval(scopes).map(|_| None),
            Statement::Return(e) => e.eval(scopes).map(Some),
        }
    }

    /// does not consume newlines
    pub fn parse<T>(source: &mut Peekable<T>) -> Result<Statement, LuaError>
    where
        T: Iterator<Item = Token>,
    {
        let is_token_type =
            |tokentype: TokenType| move |token: &Token| token.tokentype == tokentype;
        let val = loop {
            let val = source.next().ok_or(LuaError::unexpected_eof())?;
            if !is_token_type(TokenType::LineBreak)(&val) {
                break val;
            }
        };
        match val.tokentype {
            TokenType::Ident(name) => {
                let val = source.next().ok_or(LuaError::unexpected_eof())?;
                println!("name: {name}, next_tokentype: {:?}", val.tokentype);
                match val.tokentype {
                    TokenType::EqAssign => {
                        let expr = Expr::parse(source)?;

                        Ok(Statement::Assignment(name, expr))
                    }
                    TokenType::LBrac(BraceType::Round) => {
                        if source
                            .peek()
                            .is_some_and(is_token_type(TokenType::Rbrac(BraceType::Round)))
                        {
                            let end_val = source.next().unwrap();
                            return Ok(Statement::Expr(Expr::function_call(
                                name,
                                vec![],
                                val.span + &end_val.span,
                            )));
                        }

                        todo!()
                    }
                    TokenType::Value(Value::String(v)) => Ok(Self::Expr(Expr::function_call(
                        name,
                        vec![Expr::value(Value::String(v), val.span.clone())],
                        val.span,
                    ))),
                    _ => Err(val.into_malformed()),
                }
            }
            _ => todo!(),
        }
    }
}
