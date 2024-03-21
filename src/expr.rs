use std::iter::Peekable;

use crate::{
    prog::{LuaScope, Value},
    tokenizing::{BraceType, Operator, Span, Token, TokenType},
    LuaError,
};

#[derive(Debug, Clone, PartialEq)]
struct ExprElement {
    span: Span,
    expr_element_type: ExprElementType,
}

#[derive(Debug, Clone, PartialEq)]
enum ExprElementType {
    FnCall(String, Vec<Expr>),
    VarAccess(String, Vec<Expr>),
    Operator(Operator),
    Nested(Expr),
    Value(Value),
}
#[derive(Debug, Clone, PartialEq)]
pub struct Expr(Vec<ExprElement>);
impl Expr {
    pub fn eval(&self, global_scope: &LuaScope, local_scope: &LuaScope) -> Result<Value, LuaError> {
        self.validate()?;
        todo!()
    }

    fn validate(&self) -> Result<(), LuaError> {
        for i in 0..self.0.len() {
            let val = &self.0[i];
            if let ExprElementType::Operator(ref op) = val.expr_element_type {
                match op {
                    Operator::Eq
                    | Operator::Plus
                    | Operator::Star
                    | Operator::Gr
                    | Operator::GrEq
                    | Operator::Sm
                    | Operator::SmEq
                    | Operator::And
                    | Operator::Or
                    | Operator::Power
                    | Operator::Div
                    | Operator::Con => {
                        let Some(neigbour_back) = self.0.get(i - 1) else {
                            return Err(val.span.into_malformed());
                        };
                        let Some(neigbour_front) = self.0.get(i - 1) else {
                            return Err(val.span.into_malformed());
                        };

                        // can have `Plus Not` for example
                        let front_incorrect = match neigbour_front.expr_element_type {
                            ExprElementType::Operator(Operator::Min | Operator::Not) => false,
                            ExprElementType::Operator(_) => true,
                            _ => false,
                        };

                        // cannot have `Not Plus` for example
                        let back_incorrect = matches!(
                            neigbour_back.expr_element_type,
                            ExprElementType::Operator(_)
                        );

                        if front_incorrect | back_incorrect {
                            return Err((neigbour_back.span.clone()
                                + &val.span
                                + &neigbour_front.span)
                                .into_malformed());
                        }
                    }
                    // (possibly) unary operators
                    Operator::Min | Operator::Not => {
                        let Some(neigbour_front) = self.0.get(i - 1) else {
                            return Err(val.span.clone().into_malformed());
                        };
                        if match neigbour_front.expr_element_type {
                            ExprElementType::Operator(Operator::Min | Operator::Not) => false,
                            ExprElementType::Operator(_) => true,
                            _ => false,
                        } {
                            return Err((neigbour_front.span.clone() + &val.span).into_malformed());
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// ends parsing when it finds something that escapes scope without consuming it
    /// as such does not consume [TokenType::LineBreak] at the end of statemtent
    /// this is left to the caller
    pub fn parse<T>(source: &mut Peekable<T>) -> Result<Expr, LuaError>
    where
        T: Iterator<Item = Token>,
    {
        let mut expr_vec = Vec::new();
        loop {
            let Some(val) = source.peek() else {
                if expr_vec.is_empty() {
                    return Err(LuaError::UnexpectedEOF);
                } else {
                    break;
                }
            };

            match val.tokentype {
                TokenType::Ident(_) => {
                    todo!()
                }
                TokenType::Value(_) => {
                    let val = source.next().unwrap();
                    let TokenType::Value(value) = val.tokentype else {
                        unreachable!()
                    };
                    expr_vec.push(ExprElement {
                        span: val.span,
                        expr_element_type: ExprElementType::Value(value),
                    })
                }
                TokenType::LBrac(ref brace) => {
                    if brace == &BraceType::Square {
                        return Err(val.clone().into_malformed());
                    }
                    let inner_expr = Self::parse(source)?;
                    let Some(val) = source.next() else {
                        return Err(LuaError::UnexpectedEOF);
                    };
                    if val.tokentype != TokenType::Rbrac(BraceType::Round) {
                        return Err(val.into_malformed());
                    }
                    expr_vec.push(ExprElement {
                        span: val.span,
                        expr_element_type: ExprElementType::Nested(inner_expr),
                    });
                }
                TokenType::Rbrac(_) => {
                    break;
                }
                TokenType::Operator(_) => {
                    let val = source.next().unwrap();
                    let TokenType::Operator(op) = val.tokentype else {
                        unreachable!()
                    };
                    expr_vec.push(ExprElement {
                        span: val.span,
                        expr_element_type: ExprElementType::Operator(op),
                    })
                }
                TokenType::LineBreak => {
                    if expr_vec.is_empty() {
                        break;
                    } else {
                        source.next();
                    }
                }
                _ => return Err(val.clone().into_malformed()),
            }
        }
        Ok(Expr(expr_vec))
    }
}
