use std::str::CharIndices;
use unicode_xid::UnicodeXID;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok {
    Def,
    Return,
    If,
    Else,
    End,
    And,
    Or,
    Equ,
    Neq,
    Add,
    Sub,
    Mul,
    Div,
    Not,
    Asg,
    Adds,
    Subs,
    Muls,
    Divs,
    LeftParen,
    RightParen,
    Comma,
    Colon,
    Newline,
    Int { value : i64 },
    Identifier { value : String },
    String { value : String },
}

pub type Spanned<T> = (usize, T, usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter((usize, char)),
    UnexpectedEof
}

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
    last_char: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices(),
            last_char: None,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        self.last_char.take().or_else(|| self.chars.next())
    }
}

macro_rules! mod_op {
    ($self:expr, $idx0:expr, $x:expr, $y:expr) => {{
        let peek = $self.chars.next();
        match peek {
            Some((idx1, '=')) => return Some(Ok(($idx0, $y, idx1))),
            _ => {
                $self.last_char = peek;
            }
        }
        return Some(Ok(($idx0, $x, $idx0)))
    }};
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Spanned<Tok>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let curr_char = self.bump();
            println!("char: {:?}", curr_char);
            match curr_char {
                Some((idx0, ch @ '0'..='9')) | Some((idx0, ch @ '-'))  => {
                    let negative = ch == '-';
                    let mut value = if ch != '-' { ch.to_digit(10).unwrap() as i64 }
                                    else { 0 };
                    let mut idx1 = idx0;
                    loop {
                        match self.chars.next() {
                            Some((_, ch @ '0'..='9')) => {
                                value = value * 10 + (ch.to_digit(10).unwrap() as i64);
                                idx1 += 1;
                            }
                            ch => {
                                self.last_char = ch;
                                break
                            },
                        }
                    }
                    if negative {
                        value *= -1;
                    }
                    return Some(Ok((idx0, Tok::Int { value }, idx1)))
                }

                Some((idx0, '=')) => mod_op!(self, idx0, Tok::Asg, Tok::Equ),
                Some((idx0, '!')) => mod_op!(self, idx0, Tok::Not, Tok::Neq),
                Some((idx0, '+')) => mod_op!(self, idx0, Tok::Add, Tok::Adds),
                Some((idx0, '-')) => mod_op!(self, idx0, Tok::Sub, Tok::Subs),
                Some((idx0, '*')) => mod_op!(self, idx0, Tok::Mul, Tok::Muls),
                Some((idx0, '/')) => mod_op!(self, idx0, Tok::Div, Tok::Divs),
                Some((idx0, '(')) => return Some(Ok((idx0, Tok::LeftParen, idx0))),
                Some((idx0, ')')) => return Some(Ok((idx0, Tok::RightParen, idx0))),
                Some((idx0, ',')) => return Some(Ok((idx0, Tok::Comma, idx0))),
                Some((idx0, ':')) => return Some(Ok((idx0, Tok::Colon, idx0))),

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
                        "def" => Tok::Def,
                        "return" => Tok::Return,
                        "if" => Tok::If,
                        "else" => Tok::Else,
                        "end" => Tok::End,
                        "and" => Tok::And,
                        "or" => Tok::Or,
                        _ => Tok::Identifier { value: string },
                    };
                    return Some(Ok((idx0, value, idx1)))
                }

                Some((idx0, '"')) => {
                    let mut idx1 = idx0;
                    let mut string = String::new();
                    let mut escape = false;
                    loop {
                        match self.chars.next() {
                            Some((m_idx1, ch @ '"')) => {
                                idx1 = m_idx1;
                                break;
                            }
                            Some((m_idx1, ch)) => {
                                idx1 = m_idx1;
                                string.push(ch);
                            },
                            None => {
                                return Some(Err(LexError::UnexpectedEof))
                            }
                        }
                    }
                    return Some(Ok((idx0, Tok::String { value: string }, idx1)))
                }

                None => return None,
                Some((idx0, '\n')) => {
                    let mut idx1 = idx0;
                    loop {
                        match self.chars.next() {
                            Some((_, '\n')) => { idx1 += 1 }
                            Some((_, ch)) if ch.is_whitespace() => { idx1 += 1 }
                            ch => {
                                self.last_char = ch;
                                break
                            }
                        }
                    }
                    return Some(Ok((idx0, Tok::Newline, idx1)))
                },
                Some((_, ch)) if ch.is_whitespace() => continue,
                Some(other) => return Some(Err(LexError::UnexpectedCharacter(other))),
            }
        }
    }
}