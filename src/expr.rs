use std::iter::Peekable;

use crate::{
    error::LuaErrorType,
    prog::{LuaScope, LuaScopePair},
    tokenizing::{BraceType, Operator, Span, Token, TokenType},
    value::Value,
    LuaError,
};

#[derive(Debug, Clone, PartialEq)]
pub struct ExprElement {
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

impl ExprElement {
    fn function_call(name: String, args: Vec<Expr>, span: Span) -> Self {
        Self {
            span,
            expr_element_type: ExprElementType::FnCall(name, args),
        }
    }

    fn value(value: Value, span: Span) -> Self {
        Self {
            span,
            expr_element_type: ExprElementType::Value(value),
        }
    }

    fn eval_value(&self, scopes: &mut LuaScopePair) -> Result<Value, LuaError> {
        match self.expr_element_type {
            ExprElementType::FnCall(ref name, ref args) => {
                println!("function call: name: {name}");
                let fun = scopes.get(name).function().ok_or(LuaError::new_with_span(
                    LuaErrorType::NotAFunction,
                    self.span.clone(),
                ))?;
                let mut arg_vals = Vec::with_capacity(args.len());
                for i in args {
                    arg_vals.push(i.eval(scopes)?);
                }
                fun.call(arg_vals, scopes.global)
            }
            ExprElementType::VarAccess(ref name, ref path) => {
                if path.is_empty() {
                    Ok(scopes.get(&name).clone())
                } else {
                    todo!()
                }
            }
            ExprElementType::Operator(_) => Err(self.to_not_a_value()),
            ExprElementType::Nested(ref v) => v.eval(scopes),
            ExprElementType::Value(ref v) => Ok(v.clone()),
        }
    }

    fn to_expected_value(&self) -> LuaError {
        LuaError::new_with_span(LuaErrorType::ExpectedValue, self.span.clone())
    }

    fn to_not_a_num(&self) -> LuaError {
        LuaError::new_with_span(LuaErrorType::NotANum, self.span.clone())
    }
    fn to_not_a_value(&self) -> LuaError {
        todo!()
    }

    fn is_value(&self) -> bool {
        match self.expr_element_type {
            ExprElementType::FnCall(_, _)
            | ExprElementType::VarAccess(_, _)
            | ExprElementType::Nested(_)
            | ExprElementType::Value(_) => true,
            ExprElementType::Operator(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr(pub Vec<ExprElement>);
impl Expr {
    pub fn function_call(name: String, args: Vec<Expr>, span: Span) -> Self {
        Self(vec![ExprElement::function_call(name, args, span)])
    }

    pub fn value(value: Value, span: Span) -> Self {
        Self(vec![ExprElement::value(value, span)])
    }

    pub fn eval(&self, scopes: &mut LuaScopePair) -> Result<Value, LuaError> {
        let is_or =
            |e: &ExprElement| e.expr_element_type == ExprElementType::Operator(Operator::Or);
        let is_and =
            |e: &ExprElement| e.expr_element_type == ExprElementType::Operator(Operator::And);

        if self.0.iter().any(is_or) {
            let mut lastval = Value::Nil;
            for i in self.0.split(is_or) {
                match Expr(Vec::from(i)).eval(scopes)? {
                    e @ Value::Boolean(false) => lastval = e,
                    e @ Value::Nil => lastval = e,
                    e => return Ok(e),
                };
            }
            return Ok(lastval);
        }

        if self.0.iter().any(is_and) {
            let mut lastval = Value::Nil;
            for i in self.0.split(is_and) {
                match Expr(Vec::from(i)).eval(scopes)? {
                    e @ Value::Boolean(false) => return Ok(e),
                    e => lastval = e,
                };
            }
            return Ok(lastval);
        }

        if self.0.len() == 1 {
            return self.0[0].eval_value(scopes);
        }

        let mut reducing_array = self.0.clone();

        /*
            precidence:
            ^
            not - (unary)
            * /
            + -
            ..
            < > <= >= ~= ==
        */
        'block: {
            // ^ is right associative
            let mut index = reducing_array.len() - 2;
            loop {
                if index < 1 {
                    break;
                }
                let val = &reducing_array[index];
                if val.expr_element_type == ExprElementType::Operator(Operator::Power) {
                    let val_a = reducing_array
                        .get(index - 1)
                        .ok_or(val.to_expected_value())?
                        .eval_value(scopes)?
                        .num()
                        .ok_or(val.to_not_a_num())?;
                    let val_b = reducing_array
                        .get(index + 1)
                        .ok_or(val.to_expected_value())?
                        .eval_value(scopes)?
                        .num()
                        .ok_or(val.to_not_a_num())?;
                    reducing_array[index - 1] = ExprElement {
                        span: reducing_array[index + 1].span.clone()
                            + &reducing_array[index - 1].span,
                        expr_element_type: ExprElementType::Value(Value::Num(val_a.powf(val_b))),
                    }; // replace val_a with result
                    reducing_array.remove(index + 1); // remove old val_b
                    reducing_array.remove(index); // remove operator
                }
                index -= 1;
            }
            if reducing_array.len() == 1 {
                break 'block;
            }

            // - and not unary operators
            index = reducing_array.len() - 2;
            loop {
                let val = &reducing_array[index];
                if val.expr_element_type == ExprElementType::Operator(Operator::Not) {
                    let val_b = reducing_array
                        .get(index + 1)
                        .ok_or(val.to_expected_value())?
                        .eval_value(scopes)?
                        .bool_nil_false()
                        .unwrap_or(true);
                    reducing_array[index] = ExprElement {
                        span: val.span.clone() + &reducing_array[index + 1].span,
                        expr_element_type: ExprElementType::Value(Value::Boolean(!val_b)),
                    };
                    reducing_array.remove(index + 1);
                } else if val.expr_element_type == ExprElementType::Operator(Operator::Min) {
                    // check if the preceding element doesnt exist or its an operator
                    if index == 0 || (!reducing_array[index - 1].is_value()) {
                        let val_b = reducing_array
                            .get(index + 1)
                            .ok_or(val.to_expected_value())?
                            .eval_value(scopes)?
                            .num()
                            .ok_or(val.to_not_a_num())?;
                        reducing_array[index] = ExprElement {
                            span: val.span.clone() + &reducing_array[index + 1].span,
                            expr_element_type: ExprElementType::Value(Value::Num(-val_b)),
                        };
                        reducing_array.remove(index + 1);
                    }
                }
                if index < 1 {
                    break;
                } else {
                    index -= 1;
                }
            }
            if reducing_array.len() == 1 {
                break 'block;
            }

            // * and / operators
            index = 1;
            loop {
                let val = &reducing_array[index];
                if let ExprElementType::Operator(ref op @ (Operator::Star | Operator::Slash)) =
                    val.expr_element_type
                {
                    let val_a = reducing_array
                        .get(index - 1)
                        .ok_or(val.to_expected_value())?
                        .eval_value(scopes)?
                        .num()
                        .ok_or(val.to_not_a_num())?;
                    let val_b = reducing_array
                        .get(index + 1)
                        .ok_or(val.to_expected_value())?
                        .eval_value(scopes)?
                        .num()
                        .ok_or(val.to_not_a_num())?;
                    let res = if op == &Operator::Star {
                        val_a * val_b
                    } else {
                        val_a / val_b
                    };

                    reducing_array[index - 1] = ExprElement {
                        span: reducing_array[index + 1].span.clone()
                            + &reducing_array[index - 1].span,
                        expr_element_type: ExprElementType::Value(Value::Num(res)),
                    };
                    reducing_array.remove(index + 1);
                    reducing_array.remove(index);
                } else {
                    index += 1;
                }
                //[1 * 2];
                //len == 3
                //index = 1
                //index +
                if index + 1 > reducing_array.len() {
                    break;
                }
            }
            if reducing_array.len() == 1 {
                break 'block;
            }

            // + and - operators
            index = 1;
            loop {
                let val = &reducing_array[index];
                if let ExprElementType::Operator(ref op @ (Operator::Plus | Operator::Min)) =
                    val.expr_element_type
                {
                    let val_a = reducing_array
                        .get(index - 1)
                        .ok_or(val.to_expected_value())?
                        .eval_value(scopes)?
                        .num()
                        .ok_or(val.to_not_a_num())?;
                    let val_b = reducing_array
                        .get(index + 1)
                        .ok_or(val.to_expected_value())?
                        .eval_value(scopes)?
                        .num()
                        .ok_or(val.to_not_a_num())?;
                    let res = if op == &Operator::Min {
                        val_a - val_b
                    } else {
                        val_a + val_b
                    };

                    reducing_array[index - 1] = ExprElement {
                        span: reducing_array[index + 1].span.clone()
                            + &reducing_array[index - 1].span,
                        expr_element_type: ExprElementType::Value(Value::Num(res)),
                    };
                    reducing_array.remove(index + 1);
                    reducing_array.remove(index);
                } else {
                    index += 1;
                }
                //[1 * 2];
                //len == 3
                //index = 1
                //index +
                if index + 1 > reducing_array.len() {
                    break;
                }
            }
            if reducing_array.len() == 1 {
                break 'block;
            }
        }

        if reducing_array.len() != 1 {
            todo!("expression reduction not implemented fully, expr: {reducing_array:?}");
        }
        reducing_array[0].eval_value(scopes)
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
                    return Err(LuaError::new_without_span(LuaErrorType::UnexpectedEOF));
                } else {
                    break;
                }
            };

            match val.tokentype {
                TokenType::Ident(_) => {
                    let val = source.next().unwrap();
                    let TokenType::Ident(value) = val.tokentype else {
                        unreachable!()
                    };

                    let Some(next_val) = source.peek() else {
                        expr_vec.push(ExprElement {
                            span: val.span,
                            expr_element_type: ExprElementType::VarAccess(value, Vec::new()),
                        });
                        continue;
                    };
                    match next_val.tokentype {
                        TokenType::LBrac(BraceType::Round) => {
                            todo!()
                        }
                        TokenType::LBrac(BraceType::Square) => {
                            todo!()
                        }
                        _ => expr_vec.push(ExprElement {
                            span: val.span,
                            expr_element_type: ExprElementType::VarAccess(value, Vec::new()),
                        }),
                    }
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
                TokenType::LBrac(_) => {
                    let val = source.next().unwrap();
                    if let Token {
                        tokentype: TokenType::LBrac(BraceType::Square),
                        ..
                    } = val
                    {
                        return Err(val.into_malformed());
                    }
                    let inner_expr = Self::parse(source)?;
                    let Some(val) = source.next() else {
                        return Err(LuaError::new_with_span(
                            LuaErrorType::UnexpectedEOF,
                            val.span,
                        ));
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
                    break;
                }
                ref e => panic!("{e:?}"), //return Err(val.clone().into_malformed()),
            }
        }
        Ok(Expr(expr_vec))
    }
}
