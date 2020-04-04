use std::convert::TryInto;
use std::str::CharIndices;
use unicode_xid::UnicodeXID;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    EOF,
    Import,
    Def,
    Extern,
    Return,
    While,
    If,
    Else,
    Elsif,
    End,
    And,
    Or,
    As,
    Equ,
    Neq,
    Add,
    Sub,
    Mul,
    Div,
    Not,
    Asg,
    Mod,
    Adds,
    Subs,
    Muls,
    Divs,
    Mods,
    Lt,
    Gt,
    Lte,
    Gte,
    LeftParen,
    RightParen,
    Comma,
    Dot,
    Colon,
    Newline,
    At,
    AtBracket,
    LeftBracket,
    RightBracket,
    I32 { value: i32 },
    I64 { value: i64 },
    Float { value: u64 },
    Identifier { value: String },
    String { value: String },
}

impl Eq for Tok {}

impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Spanned<T> = (usize, T, usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    UnexpectedCharacter((usize, char)),
    UnexpectedEof,
    DuplicateArguments,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
    last_char: Option<(usize, char)>,
    outputted_eof: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices(),
            last_char: None,
            outputted_eof: false,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        self.last_char.take().or_else(|| self.chars.next())
    }
}

macro_rules! mod_op {
    ($self:expr, $idx0:expr, $x:expr, $next_char:expr, $y:expr) => {{
        let peek = $self.chars.next();
        match peek {
            Some((idx1, $next_char)) => return Some(Ok(($idx0, $y, idx1))),
            _ => {
                $self.last_char = peek;
            }
        }
        return Some(Ok(($idx0, $x, $idx0)));
    }};
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Spanned<Tok>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let curr_char = self.bump();
            match curr_char {
                Some((idx0, ch @ '0'..='9')) => {
                    let mut value = ch.to_digit(10).unwrap() as i64;
                    let mut idx1 = idx0;
                    loop {
                        match self.chars.next() {
                            Some((_, ch @ '0'..='9')) => {
                                value = value * 10 + (ch.to_digit(10).unwrap() as i64);
                                idx1 += 1;
                            }
                            Some((_, 'i')) => {
                                idx1 += 1;
                                match (self.chars.next(), self.chars.next()) {
                                    (Some((_, '3')), Some((_, '2'))) => {
                                        idx1 += 2;
                                        return Some(Ok((
                                            idx0,
                                            Tok::I32 {
                                                value: (value as i32),
                                            },
                                            idx1,
                                        )));
                                    }
                                    (Some((_, '6')), Some((_, '4'))) => {
                                        idx1 += 2;
                                        return Some(Ok((idx0, Tok::I64 { value }, idx1)));
                                    }
                                    _ => {
                                        return Some(Err(Error::UnexpectedCharacter((idx1, 'i'))));
                                    }
                                }
                            }
                            Some((_, '.')) => {
                                idx1 += 1;
                                let mut decimal = 0i64;
                                let mut ndigits = 1.0f64;
                                loop {
                                    match self.chars.next() {
                                        Some((_, ch @ '0'..='9')) => {
                                            decimal =
                                                decimal * 10 + (ch.to_digit(10).unwrap() as i64);
                                            idx1 += 1;
                                            ndigits *= 10.0;
                                        }
                                        ch => {
                                            self.last_char = ch;
                                            break;
                                        }
                                    }
                                }
                                return Some(Ok((
                                    idx0,
                                    Tok::Float {
                                        value: f64::to_bits(
                                            (value as f64) + (decimal as f64 / ndigits),
                                        ),
                                    },
                                    idx1,
                                )));
                            }
                            ch => {
                                self.last_char = ch;
                                break;
                            }
                        }
                    }
                    if let Ok(value) = value.try_into() {
                        return Some(Ok((idx0, Tok::I32 { value }, idx1)));
                    } else {
                        return Some(Ok((idx0, Tok::I64 { value }, idx1)));
                    }
                }

                Some((idx0, '=')) => mod_op!(self, idx0, Tok::Asg, '=', Tok::Equ),
                Some((idx0, '!')) => mod_op!(self, idx0, Tok::Not, '=', Tok::Neq),
                Some((idx0, '+')) => mod_op!(self, idx0, Tok::Add, '=', Tok::Adds),
                Some((idx0, '-')) => mod_op!(self, idx0, Tok::Sub, '=', Tok::Subs),
                Some((idx0, '*')) => mod_op!(self, idx0, Tok::Mul, '=', Tok::Muls),
                Some((idx0, '/')) => mod_op!(self, idx0, Tok::Div, '=', Tok::Divs),
                Some((idx0, '%')) => mod_op!(self, idx0, Tok::Mod, '=', Tok::Mods),
                Some((idx0, '<')) => mod_op!(self, idx0, Tok::Lt, '=', Tok::Lte),
                Some((idx0, '>')) => mod_op!(self, idx0, Tok::Gt, '=', Tok::Gte),
                Some((idx0, '(')) => return Some(Ok((idx0, Tok::LeftParen, idx0))),
                Some((idx0, ')')) => return Some(Ok((idx0, Tok::RightParen, idx0))),
                Some((idx0, ',')) => return Some(Ok((idx0, Tok::Comma, idx0))),
                Some((idx0, '.')) => return Some(Ok((idx0, Tok::Dot, idx0))),
                Some((idx0, ':')) => return Some(Ok((idx0, Tok::Colon, idx0))),
                Some((idx0, '@')) => mod_op!(self, idx0, Tok::At, '[', Tok::AtBracket),
                Some((idx0, '[')) => return Some(Ok((idx0, Tok::LeftBracket, idx0))),
                Some((idx0, ']')) => return Some(Ok((idx0, Tok::RightBracket, idx0))),

                Some((idx0, ch)) if UnicodeXID::is_xid_start(ch) => {
                    let mut idx1 = idx0;
                    let mut string = ch.to_string();
                    loop {
                        match self.chars.next() {
                            Some((m_idx1, ch)) if UnicodeXID::is_xid_continue(ch) => {
                                string.push(ch);
                                idx1 = m_idx1;
                            }
                            ch => {
                                self.last_char = ch;
                                break;
                            }
                        }
                    }
                    let value = match string.as_str() {
                        "import" => Tok::Import,
                        "def" => Tok::Def,
                        "extern" => Tok::Extern,
                        "return" => Tok::Return,
                        "while" => Tok::While,
                        "if" => Tok::If,
                        "else" => Tok::Else,
                        "elsif" => Tok::Elsif,
                        "end" => Tok::End,
                        "and" => Tok::And,
                        "or" => Tok::Or,
                        "as" => Tok::As,
                        _ => Tok::Identifier { value: string },
                    };
                    return Some(Ok((idx0, value, idx1)));
                }

                Some((idx0, '"')) => {
                    let mut idx1 = idx0;
                    let mut string = String::new();
                    // let mut escape = false;
                    loop {
                        match self.chars.next() {
                            Some((_, '"')) => {
                                idx1 += 1;
                                break;
                            }
                            Some((_, ch)) => {
                                idx1 += 1;
                                string.push(ch);
                            }
                            None => return Some(Err(Error::UnexpectedEof)),
                        }
                    }
                    return Some(Ok((idx0, Tok::String { value: string }, idx1)));
                }

                None => {
                    if self.outputted_eof {
                        return None;
                    }
                    self.outputted_eof = true;
                    return Some(Ok((0, Tok::EOF, 0)));
                }
                Some((idx0, '\n')) => {
                    let mut idx1 = idx0;
                    loop {
                        match self.chars.next() {
                            Some((_, '\n')) => idx1 += 1,
                            Some((_, ch)) if ch.is_whitespace() => idx1 += 1,
                            ch => {
                                self.last_char = ch;
                                break;
                            }
                        }
                    }
                    return Some(Ok((idx0, Tok::Newline, idx1)));
                }
                Some((_, ch)) if ch.is_whitespace() => continue,
                Some(other) => return Some(Err(Error::UnexpectedCharacter(other))),
            }
        }
    }
}
