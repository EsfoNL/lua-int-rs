use tracing::debug;

use crate::luafn::LuaCodeFn;
use crate::peekable_n::PeekableN;
use crate::str_interner::InternedStr;
use crate::Result;
use crate::{
    error::LuaErrorType,
    expr::Expr,
    prog::LuaScopePair,
    tokenizing::{BraceType, Token, TokenType},
    value::Value,
    LuaError,
};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(InternedStr, Expr),
    Expr(Expr),
    Return(Expr),
    If {
        condition: Expr,
        statements: Vec<Statement>,
        elseif: Vec<(Expr, Vec<Statement>)>,
        else_branch: Option<Vec<Statement>>,
    },
}
pub fn is_token_type(tokentype: TokenType) -> impl Fn(&Token) -> bool {
    move |token: &Token| token.tokentype == tokentype
}
impl Statement {
    pub fn execute(&self, scopes: &mut LuaScopePair) -> Result<Option<Value>> {
        match self {
            Statement::Assignment(name, value_expr) => {
                let res = value_expr.eval(scopes)?;
                scopes.global.0.insert(name.clone(), res);
                Ok(None)
            }
            Statement::Expr(e) => e.eval(scopes).map(|_| None),
            Statement::Return(e) => e.eval(scopes).map(Some),
            Statement::If {
                condition,
                statements,
                elseif,
                else_branch,
            } => {
                debug!("scopes: {scopes:?}");
                let mut evalled = false;
                if condition.eval(scopes)?.bool() {
                    for i in statements {
                        if let Some(v) = i.execute(scopes)? {
                            return Ok(Some(v));
                        }
                    }
                    evalled = true;
                } else if !elseif.is_empty() {
                    for (cond, branch) in elseif {
                        if cond.eval(scopes)?.bool() {
                            for i in branch {
                                if let Some(v) = i.execute(scopes)? {
                                    return Ok(Some(v));
                                }
                            }
                            evalled = true;
                            break;
                        }
                    }
                }
                if let (false, Some(branch)) = (evalled, else_branch) {
                    for i in branch {
                        if let Some(v) = i.execute(scopes)? {
                            return Ok(Some(v));
                        }
                    }
                }
                Ok(None)
            }
        }
    }

    pub fn parse_until_end<T>(source: &mut PeekableN<T, 2>) -> Result<Vec<Statement>>
    where
        T: Iterator<Item = Token>,
    {
        let mut statements = Vec::new();
        loop {
            let val = source.peek().ok_or(LuaError::unexpected_eof())?;
            match val.tokentype {
                TokenType::End | TokenType::Else | TokenType::ElseIf => break,
                TokenType::LineBreak => {
                    source.next();
                    continue;
                }
                _ => {
                    statements.push(Statement::parse(source)?);
                }
            }
        }
        Ok(statements)
    }

    /// does not consume newlines
    pub fn parse<T>(source: &mut PeekableN<T, 2>) -> Result<Statement>
    where
        T: Iterator<Item = Token>,
    {
        let val = loop {
            let val = source.next().ok_or(LuaError::unexpected_eof())?;
            if !is_token_type(TokenType::LineBreak)(&val) {
                break val;
            }
        };
        match val.tokentype {
            TokenType::Ident(name) => {
                debug!("ident: {name}");
                let next_val = source.next().ok_or(LuaError::unexpected_eof())?;
                match next_val.tokentype {
                    TokenType::EqAssign => {
                        let expr = Expr::parse(source)?;

                        Ok(Statement::Assignment(name.into(), expr))
                    }
                    TokenType::LBrac(BraceType::Round) => {
                        if source
                            .peek()
                            .is_some_and(is_token_type(TokenType::RBrac(BraceType::Round)))
                        {
                            let end_val = source.next().unwrap();
                            return Ok(Statement::Expr(Expr::function_call(
                                name,
                                vec![],
                                val.span + &end_val.span,
                            )));
                        }
                        // parse args
                        let mut args = Vec::new();
                        let mut span = val.span;
                        loop {
                            let val = Expr::parse(source)?;
                            args.push(val);
                            let next_token = source
                                .next()
                                .ok_or(Into::<LuaError>::into(LuaErrorType::UnexpectedEOF))?;
                            span += &next_token.span;
                            match next_token.tokentype {
                                TokenType::RBrac(BraceType::Round) => {
                                    break;
                                }
                                TokenType::Comma => (),
                                _ => return Err(next_token.into_malformed()),
                            }
                        }

                        Ok(Statement::Expr(Expr::function_call(name, args, span)))
                    }
                    TokenType::Value(Value::String(v)) => Ok(Self::Expr(Expr::function_call(
                        name,
                        vec![Expr::value(Value::String(v), next_val.span.clone())],
                        next_val.span,
                    ))),
                    TokenType::LBrac(BraceType::Square) => todo!(),
                    TokenType::Dot => todo!(),
                    _ => Err(next_val.into_malformed()),
                }
            }
            TokenType::Return => {
                let expr = Expr::parse(source)?;
                Ok(Self::Return(expr))
            }
            TokenType::Fn => {
                // name of fun

                let next_token = source.next().ok_or(LuaError::unexpected_eof())?;
                let TokenType::Ident(name) = next_token.tokentype else {
                    return Err(next_token.into_malformed());
                };

                // opening bracket args
                let should_be_bracket = source.next().ok_or(LuaError::unexpected_eof())?;
                if should_be_bracket.tokentype != TokenType::LBrac(BraceType::Round) {
                    return Err(should_be_bracket.into_malformed());
                }
                // args
                let mut args = Vec::new();
                if source.peek().ok_or(LuaError::unexpected_eof())?.tokentype
                    != TokenType::RBrac(BraceType::Round)
                {
                    loop {
                        debug!("args: {args:?}");
                        let val = source.next().ok_or(LuaError::unexpected_eof())?;
                        let TokenType::Ident(arg) = val.tokentype else {
                            return Err(val.into_malformed());
                        };
                        args.push(arg);
                        debug!("args: {args:?}");
                        let next_val = source.next().ok_or(LuaError::unexpected_eof())?;
                        match next_val.tokentype {
                            TokenType::RBrac(BraceType::Round) => {
                                break;
                            }
                            TokenType::Comma => (),
                            _ => return Err(next_val.into_malformed()),
                        }
                    }
                }

                let body = Self::parse_until_end(source)?;
                let next_token = source.next().ok_or(LuaError::unexpected_eof())?;
                if next_token.tokentype != TokenType::End {
                    return Err(next_token.into_malformed());
                }
                let fun = Arc::new(LuaCodeFn { names: args, body });
                Ok(Statement::Assignment(
                    name,
                    Expr::value(Value::Function(fun), val.span + &next_token.span),
                ))
            }
            TokenType::If => {
                let condition = Expr::parse(source)?;
                let then_token = source.next().ok_or(LuaError::unexpected_eof())?;
                if then_token.tokentype != TokenType::Then {
                    return Err(then_token.into_malformed());
                }
                let statements = Statement::parse_until_end(source)?;
                let mut else_branch = None;
                let mut elseif = Vec::new();
                loop {
                    let else_or_end = source.next().ok_or(LuaError::unexpected_eof())?;
                    if else_or_end.tokentype == TokenType::End {
                        break;
                    } else if else_or_end.tokentype == TokenType::ElseIf {
                        let condition = Expr::parse(source)?;
                        let then_token = source.next().ok_or(LuaError::unexpected_eof())?;
                        if then_token.tokentype != TokenType::Then {
                            return Err(then_token.into_malformed());
                        }
                        let statements = Statement::parse_until_end(source)?;
                        elseif.push((condition, statements));
                        debug!("{elseif:?}");
                    } else if else_or_end.tokentype == TokenType::Else {
                        let statements = Statement::parse_until_end(source)?;
                        else_branch = Some(statements);
                        let end_token = source.next().ok_or(LuaError::unexpected_eof())?;
                        if end_token.tokentype != TokenType::End {
                            return Err(then_token.into_malformed());
                        }
                        break;
                    }
                }
                Ok(Statement::If {
                    condition,
                    statements,
                    elseif,
                    else_branch,
                })
            }
            _ => todo!("token: {val:?}"),
        }
    }
}
