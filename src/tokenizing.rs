use std::{iter::Peekable, ops::Add};

use crate::{prog::Value, LuaError};

#[derive(Clone, Debug)]
pub struct Token {
    pub tokentype: TokenType,
    pub span: Span,
}

impl Token {
    pub fn into_malformed(self) -> LuaError {
        LuaError::MalFormed(self.span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub chars: std::ops::RangeInclusive<u64>,
    pub line: u64,
}

impl Add<&Span> for Span {
    type Output = Span;

    fn add(self, rhs: &Self) -> Self::Output {
        Span {
            chars: u64::min(*self.chars.start(), *rhs.chars.start())
                ..=u64::max(*self.chars.end(), *rhs.chars.end()),
            line: self.line,
        }
    }
}

impl Span {
    pub fn into_malformed(&self) -> LuaError {
        LuaError::MalFormed(self.clone())
    }
}

impl From<PositionedChar> for Span {
    fn from(i: PositionedChar) -> Self {
        Span {
            chars: i.char_pos..=i.char_pos,
            line: i.line,
        }
    }
}

impl Span {
    fn from_positioned_chars(
        start: impl Into<PositionedChar>,
        finish: impl Into<PositionedChar>,
    ) -> Self {
        let (start, finish) = (start.into(), finish.into());
        Self {
            chars: start.char_pos..=finish.char_pos,
            line: start.line,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Fn,
    Ident(String),
    If,
    End,
    EqAssign,
    Dot,
    LBrac(BraceType),
    Rbrac(BraceType),
    Operator(Operator),
    Else,
    Value(Value),
    LineBreak,
    Local,
    Return,
    For,
    Comma,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BraceType {
    Round,
    Square,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Operator {
    Eq,
    Min,
    Plus,
    Star,
    Gr,
    GrEq,
    Sm,
    SmEq,
    And,
    Or,
    Power,
    Not,
    Div,
    Con,
}

pub struct Tokenizer<T>
where
    T: Iterator<Item = PositionedChar>,
{
    iter: Peekable<T>,
}

impl<T> std::fmt::Debug for Tokenizer<T>
where
    T: Iterator<Item = PositionedChar>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tokenizer").finish()
    }
}

pub fn tokenize<I: IntoIterator<Item = char>>(
    i: I,
) -> Tokenizer<impl Iterator<Item = PositionedChar>>
where {
    let mut line_number = 0;
    let mut char_number = 0;
    let iter = i.into_iter().map(move |c| {
        match c {
            '\n' => {
                line_number += 1;
                char_number = 0;
            }
            _ => {
                char_number += 1;
            }
        }
        PositionedChar {
            line: line_number,
            char_pos: char_number,
            c,
        }
    });

    Tokenizer {
        iter: iter.peekable(),
    }
}

#[derive(Clone)]
pub struct PositionedChar {
    line: u64,
    char_pos: u64,
    c: char,
}

impl<T: Iterator<Item = PositionedChar>> Iterator for Tokenizer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let c = loop {
            let v = self.iter.next()?;
            match v.c {
                ' ' | '\t' => (),
                _ => break v,
            }
        };

        Some(match c.c {
            '(' => Token {
                span: c.into(),
                tokentype: TokenType::LBrac(BraceType::Round),
            },
            ')' => Token {
                span: c.into(),
                tokentype: TokenType::Rbrac(BraceType::Round),
            },
            '=' => {
                if let Some(v) = self.iter.peek() {
                    match v.c {
                        '=' => {
                            let v = self.iter.next().unwrap();
                            Token {
                                tokentype: TokenType::Operator(Operator::Eq),
                                span: Span::from_positioned_chars(c, v.clone()),
                            }
                        }
                        _ => Token {
                            tokentype: TokenType::EqAssign,
                            span: c.into(),
                        },
                    }
                } else {
                    Token {
                        tokentype: TokenType::EqAssign,
                        span: c.into(),
                    }
                }
            }
            '>' => {
                if let Some(PositionedChar { c: '=', .. }) = self.iter.peek() {
                    let v = self.iter.next().unwrap();
                    Token {
                        tokentype: TokenType::Operator(Operator::GrEq),
                        span: Span::from_positioned_chars(c, v),
                    }
                } else {
                    Token {
                        tokentype: TokenType::Operator(Operator::Gr),
                        span: c.into(),
                    }
                }
            }
            '<' => {
                if let Some(PositionedChar { c: '=', .. }) = self.iter.peek() {
                    let v = self.iter.next().unwrap();
                    Token {
                        tokentype: TokenType::Operator(Operator::SmEq),
                        span: Span::from_positioned_chars(c, v),
                    }
                } else {
                    Token {
                        tokentype: TokenType::Operator(Operator::Sm),
                        span: c.into(),
                    }
                }
            }
            '-' => Token {
                tokentype: TokenType::Operator(Operator::Min),
                span: c.into(),
            },
            '+' => Token {
                tokentype: TokenType::Operator(Operator::Plus),
                span: c.into(),
            },
            '*' => Token {
                tokentype: TokenType::Operator(Operator::Star),
                span: c.into(),
            },
            '\n' => Token {
                tokentype: TokenType::LineBreak,
                span: c.into(),
            },
            '"' => {
                let mut data = String::new();
                let endchar = loop {
                    let c = self.iter.next()?;
                    if c.c == '"' {
                        break c;
                    }
                    if c.c == '\\' {
                        data.push(self.iter.next()?.c);
                    } else {
                        data.push(c.c);
                    }
                };
                Token {
                    tokentype: TokenType::Value(Value::String(data)),
                    span: Span::from_positioned_chars(c, endchar),
                }
            }
            v @ ('0'..='9' | '.') => {
                let mut value = 0f64;
                let mut endchar = c.clone();
                if v == '.' {
                    match self.iter.peek() {
                        Some(PositionedChar { c: '0'..='9', .. }) => (),
                        _ => {
                            return Some(Token {
                                tokentype: TokenType::Dot,
                                span: c.into(),
                            })
                        }
                    }
                } else {
                    value = v.to_digit(10).unwrap() as f64;
                    loop {
                        let Some(val) = self.iter.peek() else {
                            return Some(Token {
                                span: Span::from_positioned_chars(c, endchar),
                                tokentype: TokenType::Value(Value::Num(value)),
                            });
                        };
                        if val.c == '.' {
                            self.iter.next();
                            break;
                        }

                        if !val.c.is_ascii_digit() {
                            return Some(Token {
                                tokentype: TokenType::Value(Value::Num(value)),
                                span: Span::from_positioned_chars(c, endchar),
                            });
                        }

                        endchar = self.iter.next().unwrap();
                        value *= 10f64;
                        value += endchar.c.to_digit(10).unwrap() as f64;
                    }
                }

                let mut digit = 1;
                // post point numbers
                loop {
                    let Some(val) = self.iter.peek() else {
                        return Some(Token {
                            span: Span::from_positioned_chars(c, endchar),
                            tokentype: TokenType::Value(Value::Num(value)),
                        });
                    };

                    if !val.c.is_ascii_digit() {
                        return Some(Token {
                            tokentype: TokenType::Value(Value::Num(value)),
                            span: Span::from_positioned_chars(c, endchar),
                        });
                    }

                    endchar = self.iter.next().unwrap();
                    value += endchar.c.to_digit(10).unwrap() as f64 / 10f64.powi(digit);
                    digit += 1;
                }
            }
            cur_c => {
                let mut data = String::new();
                data.push(cur_c);
                let mut endchar = c.clone();
                loop {
                    match self.iter.peek()?.c {
                        ' ' | '\t' | '\n' | '(' | ')' | '>' | '<' | '=' | '*' | '/' | '%' | '^'
                        | '.' | '-' | '+' | '"' => break,
                        _ => {
                            let v = self.iter.next().unwrap();
                            data.push(v.c);
                            endchar = v;
                        }
                    }
                }
                Token {
                    tokentype: match data.as_str() {
                        "function" => TokenType::Fn,
                        "if" => TokenType::If,
                        "end" => TokenType::End,
                        "else" => TokenType::Else,
                        "local" => TokenType::Local,
                        "return" => TokenType::Return,
                        "for" => TokenType::For,
                        _ => TokenType::Ident(data),
                    },
                    span: Span::from_positioned_chars(c, endchar),
                }
            }
        })
    }
}
