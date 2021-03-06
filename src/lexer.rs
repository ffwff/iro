use crate::compiler;
use crate::compiler::sources::FileIndex;
use std::collections::VecDeque;
use std::convert::TryInto;
use std::intrinsics::likely;
use std::str::CharIndices;
use unicode_xid::UnicodeXID;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    EOF,
    Use,
    Def,
    Extern,
    Return,
    While,
    If,
    Else,
    Elsif,
    Pass,
    Break,
    Module,
    And,
    Or,
    As,
    Mut,
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
    LeftCurly,
    RightCurly,
    Comma,
    Dot,
    Amp,
    Colon,
    DoubleColon,
    Semicolon,
    Newline,
    At,
    AtBracket,
    LeftBracket,
    RightBracket,
    True,
    False,
    Uninitialized,
    Struct,
    Uni,
    BlockBegin,
    Dedent,
    I32 { value: i32 },
    I64 { value: i64 },
    ISize { value: i64 },
    Float { value: u64 },
    Identifier { value: String },
    CapitalIdentifier { value: String },
    String { value: String },
}

impl Eq for Tok {}

impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::EOF => write!(f, "end of file"),
            Tok::Use => write!(f, "use"),
            Tok::Def => write!(f, "def"),
            Tok::Extern => write!(f, "extern"),
            Tok::Return => write!(f, "return"),
            Tok::While => write!(f, "while"),
            Tok::If => write!(f, "if"),
            Tok::Else => write!(f, "else"),
            Tok::Elsif => write!(f, "elsif"),
            Tok::Pass => write!(f, "pass"),
            Tok::Break => write!(f, "break"),
            Tok::Module => write!(f, "mod"),
            Tok::Struct => write!(f, "struct"),
            Tok::And => write!(f, "and"),
            Tok::Or => write!(f, "or"),
            Tok::As => write!(f, "as"),
            Tok::Mut => write!(f, "mut"),
            Tok::Equ => write!(f, "=="),
            Tok::Neq => write!(f, "!="),
            Tok::Add => write!(f, "+"),
            Tok::Sub => write!(f, "-"),
            Tok::Mul => write!(f, "*"),
            Tok::Div => write!(f, "/"),
            Tok::Not => write!(f, "not"),
            Tok::Asg => write!(f, "="),
            Tok::Mod => write!(f, "%"),
            Tok::Adds => write!(f, "+="),
            Tok::Subs => write!(f, "-="),
            Tok::Muls => write!(f, "*="),
            Tok::Divs => write!(f, "/="),
            Tok::Mods => write!(f, "%="),
            Tok::Lt => write!(f, "<"),
            Tok::Gt => write!(f, ">"),
            Tok::Lte => write!(f, "<="),
            Tok::Gte => write!(f, ">="),
            Tok::LeftParen => write!(f, "("),
            Tok::RightParen => write!(f, ")"),
            Tok::LeftCurly => write!(f, "{{"),
            Tok::RightCurly => write!(f, "}}"),
            Tok::Comma => write!(f, ","),
            Tok::Dot => write!(f, "."),
            Tok::Amp => write!(f, "&"),
            Tok::Colon => write!(f, ":"),
            Tok::DoubleColon => write!(f, "::"),
            Tok::Semicolon => write!(f, "semicolon"),
            Tok::Newline => write!(f, "newline"),
            Tok::At => write!(f, "@"),
            Tok::AtBracket => write!(f, "@["),
            Tok::LeftBracket => write!(f, "["),
            Tok::RightBracket => write!(f, "]"),
            Tok::True => write!(f, "true"),
            Tok::False => write!(f, "false"),
            Tok::Uninitialized => write!(f, "unintialized"),
            Tok::I32 { value } => write!(f, "integer value {:?}", value),
            Tok::I64 { value } => write!(f, "integer value {:?}", value),
            Tok::ISize { value } => write!(f, "integer value {:?}", value),
            Tok::Float { value } => write!(f, "floating point value {:?}", value),
            Tok::Identifier { value } => write!(f, "identifier {:?}", value),
            Tok::CapitalIdentifier { value } => write!(f, "type identifier {:?}", value),
            Tok::String { value } => write!(f, "string {:?}", value),
            other => write!(f, "{:?}", other),
        }
    }
}

pub type Spanned<T> = (usize, T, usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    UnexpectedCharacter(char),
    UnexpectedEof,
    InvalidIndent,
}

impl Error {
    pub fn into_compiler_error(self, file: FileIndex, location: usize) -> compiler::Error {
        use compiler::error::Code;
        use compiler::sources::SourceSpan;
        compiler::Error {
            error: match self {
                Error::UnexpectedCharacter(ch) => Code::UnexpectedCharacter(ch),
                Error::UnexpectedEof => Code::UnrecognizedEOF { expected: vec![] },
                Error::InvalidIndent => Code::InvalidIndent,
            },
            span: Some(SourceSpan {
                file,
                start: location,
                end: location,
            }),
        }
    }
}

#[inline(always)]
fn is_identifier_start(ch: char) -> bool {
    // Cache common identifier start characters
    if likely(('A'..='Z').contains(&ch) || ('a'..='z').contains(&ch) || ch == '$' || ch == '_') {
        true
    } else {
        UnicodeXID::is_xid_start(ch)
    }
}

#[inline(always)]
fn is_identifier_continue(ch: char) -> bool {
    // Cache common identifier continuation characters
    if likely(
        ('A'..='Z').contains(&ch)
            || ('a'..='z').contains(&ch)
            || ('0'..='9').contains(&ch)
            || ch == '$'
            || ch == '_',
    ) {
        true
    } else {
        UnicodeXID::is_xid_continue(ch)
    }
}

pub struct Lexer<'input> {
    size: usize,
    chars: CharIndices<'input>,
    last_char: Option<(usize, char)>,
    outputted_eof: bool,
    indent_stack: Vec<usize>,
    curr_indent: usize,
    last_token: VecDeque<Spanned<Tok>>,
    file: FileIndex,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str, file: FileIndex) -> Self {
        Lexer {
            size: input.len(),
            chars: input.char_indices(),
            last_char: None,
            outputted_eof: false,
            indent_stack: vec![],
            curr_indent: 0,
            last_token: VecDeque::new(),
            file,
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
    type Item = Result<Spanned<Tok>, compiler::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(last_token) = self.last_token.pop_front() {
                return Some(Ok(last_token));
            }
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
                                    (Some((_, 's')), ch) => {
                                        idx1 += 1;
                                        self.last_char = ch;
                                        return Some(Ok((idx0, Tok::ISize { value }, idx1)));
                                    }
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
                                        return Some(Err(Error::UnexpectedCharacter('i')
                                            .into_compiler_error(self.file, idx0)));
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

                Some((idx0, '=')) => {
                    match self.chars.next() {
                        Some((idx1, '=')) => return Some(Ok((idx0, Tok::Equ, idx1))),
                        Some((idx1, '>')) => return Some(Ok((idx0, Tok::BlockBegin, idx1))),
                        peek => {
                            self.last_char = peek;
                        }
                    }
                    return Some(Ok((idx0, Tok::Asg, idx0)));
                }
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
                Some((idx0, '{')) => return Some(Ok((idx0, Tok::LeftCurly, idx0))),
                Some((idx0, '}')) => return Some(Ok((idx0, Tok::RightCurly, idx0))),
                Some((idx0, ',')) => return Some(Ok((idx0, Tok::Comma, idx0))),
                Some((idx0, '.')) => return Some(Ok((idx0, Tok::Dot, idx0))),
                Some((idx0, ':')) => mod_op!(self, idx0, Tok::Colon, ':', Tok::DoubleColon),
                Some((idx0, ';')) => return Some(Ok((idx0, Tok::Semicolon, idx0))),
                Some((idx0, '@')) => mod_op!(self, idx0, Tok::At, '[', Tok::AtBracket),
                Some((idx0, '[')) => return Some(Ok((idx0, Tok::LeftBracket, idx0))),
                Some((idx0, ']')) => return Some(Ok((idx0, Tok::RightBracket, idx0))),
                Some((idx0, '&')) => return Some(Ok((idx0, Tok::Amp, idx0))),

                Some((idx0, ch)) if is_identifier_start(ch) => {
                    let mut idx1 = idx0;
                    let mut string = ch.to_string();
                    loop {
                        match self.chars.next() {
                            Some((m_idx1, ch)) if is_identifier_continue(ch) => {
                                string.push(ch);
                                idx1 = m_idx1;
                            }
                            ch => {
                                self.last_char = ch;
                                break;
                            }
                        }
                    }
                    if string.chars().next().unwrap().is_uppercase() {
                        return Some(Ok((idx0, Tok::CapitalIdentifier { value: string }, idx0)));
                    }
                    let value = match string.as_str() {
                        "use" => Tok::Use,
                        "def" => Tok::Def,
                        "extern" => Tok::Extern,
                        "return" => Tok::Return,
                        "while" => Tok::While,
                        "if" => Tok::If,
                        "else" => Tok::Else,
                        "elsif" => Tok::Elsif,
                        "pass" => Tok::Pass,
                        "break" => Tok::Break,
                        "and" => Tok::And,
                        "or" => Tok::Or,
                        "as" => Tok::As,
                        "mut" => Tok::Mut,
                        "uni" => Tok::Uni,
                        "true" => Tok::True,
                        "false" => Tok::False,
                        "uninitialized" => Tok::Uninitialized,
                        "struct" => Tok::Struct,
                        "mod" => Tok::Module,
                        _ => Tok::Identifier { value: string },
                    };
                    return Some(Ok((idx0, value, idx1)));
                }

                Some((idx0, '"')) => {
                    let mut idx1 = idx0;
                    let mut string = String::new();
                    loop {
                        match self.chars.next() {
                            Some((_, '"')) => {
                                idx1 += 1;
                                break;
                            }
                            Some((_, '\\')) => {
                                idx1 += 1;
                                match self.chars.next() {
                                    Some((_, '\'')) => {
                                        idx1 += 1;
                                        string.push('\'')
                                    }
                                    Some((_, '\"')) => {
                                        idx1 += 1;
                                        string.push('\"')
                                    }
                                    Some((_, '\\')) => {
                                        idx1 += 1;
                                        string.push('\\')
                                    }
                                    Some((_, 'n')) => {
                                        idx1 += 1;
                                        string.push('\n')
                                    }
                                    Some((_, 'r')) => {
                                        idx1 += 1;
                                        string.push('\r')
                                    }
                                    Some((_, 't')) => {
                                        idx1 += 1;
                                        string.push('\t')
                                    }
                                    Some((_, ch)) => {
                                        idx1 += 1;
                                        string.push(ch)
                                    }
                                    None => {
                                        return Some(Err(Error::UnexpectedEof
                                            .into_compiler_error(self.file, self.size)))
                                    }
                                }
                            }
                            Some((_, ch)) => {
                                idx1 += 1;
                                string.push(ch);
                            }
                            None => {
                                return Some(Err(
                                    Error::UnexpectedEof.into_compiler_error(self.file, self.size)
                                ))
                            }
                        }
                    }
                    return Some(Ok((idx0, Tok::String { value: string }, idx1)));
                }

                None => {
                    if self.outputted_eof {
                        return None;
                    }
                    self.outputted_eof = true;
                    // println!("{:#?}", self.indent_stack);
                    if self.indent_stack.len() < 2 {
                        return Some(Ok((0, Tok::EOF, 0)));
                    } else {
                        // println!("{:#?}", self.indent_stack);
                        for _ in self.indent_stack.iter().skip(2) {
                            self.last_token.push_back((0, Tok::Dedent, 0));
                        }
                        self.last_token.push_back((0, Tok::EOF, 0));
                        return Some(Ok((0, Tok::Dedent, 0)));
                    }
                }
                Some((idx0, '\n')) => {
                    let mut idx1 = idx0;
                    let last_indent = std::mem::replace(&mut self.curr_indent, 0);
                    loop {
                        match self.chars.next() {
                            Some((_, '\n')) => {
                                idx1 += 1;
                                self.curr_indent = 0;
                            }
                            Some((_, ch)) if ch.is_whitespace() => {
                                idx1 += 1;
                                self.curr_indent += 1;
                            }
                            ch => {
                                self.last_char = ch;
                                break;
                            }
                        }
                    }
                    if self.curr_indent < last_indent {
                        let mut n_pops = 0;
                        while let Some(last) = self.indent_stack.last() {
                            if *last <= self.curr_indent {
                                break;
                            }
                            self.indent_stack.pop();
                            n_pops += 1;
                        }
                        for _ in 1..n_pops {
                            self.last_token.push_back((idx0, Tok::Dedent, idx1));
                        }
                        return Some(Ok((idx0, Tok::Dedent, idx1)));
                    } else if self.curr_indent > last_indent || self.indent_stack.is_empty() {
                        self.indent_stack.push(self.curr_indent);
                    }
                    return Some(Ok((idx0, Tok::Newline, idx1)));
                }
                Some((_, ch)) if ch.is_whitespace() => continue,
                Some((idx, other)) => {
                    return Some(Err(
                        Error::UnexpectedCharacter(other).into_compiler_error(self.file, idx)
                    ))
                }
            }
        }
    }
}

#[cfg(test)]
mod lexer_test {
    use super::*;

    macro_rules! lex {
        ($str:expr) => {
            Lexer::new($str, 0)
                .filter(|x| x.is_ok())
                .map(|x| x.unwrap())
                .collect::<Vec<_>>()
        };
    }

    #[test]
    fn dedent_before_eof() {
        let tokens = lex!(
            "
while 1 == 2 =>
    1
    if 1 == 2 =>
        2"
        );
        let length = tokens.len();
        assert_ne!(tokens[length - 4].1, Tok::Dedent);
        assert_eq!(tokens[length - 3].1, Tok::Dedent);
        assert_eq!(tokens[length - 2].1, Tok::Dedent);
        assert_eq!(tokens[length - 1].1, Tok::EOF);
    }

    #[test]
    fn dedent() {
        let tokens = lex!(
            "
while 1 == 2 =>
    1
    if 1 == 2 =>
        2
"
        );
        let length = tokens.len();
        assert_ne!(tokens[length - 4].1, Tok::Dedent);
        assert_eq!(tokens[length - 3].1, Tok::Dedent);
        assert_eq!(tokens[length - 2].1, Tok::Dedent);
        assert_eq!(tokens[length - 1].1, Tok::EOF);
    }

    #[test]
    fn dedent_with_start_indent() {
        let tokens = lex!(
            "\
                while 1 == 2 =>
                    1
                    if 1 == 2 =>
                        2
"
        );
        let length = tokens.len();
        assert_ne!(tokens[length - 4].1, Tok::Dedent);
        assert_eq!(tokens[length - 3].1, Tok::Dedent);
        assert_eq!(tokens[length - 2].1, Tok::Dedent);
        assert_eq!(tokens[length - 1].1, Tok::EOF);
    }

    #[test]
    fn dedent_after_nested_eof() {
        let tokens = lex!(
            "
while 1 == 2 =>
    if 1 == 2 =>
        2
    1"
        );
        let length = tokens.len();
        println!("{:#?}", tokens);
        assert_ne!(tokens[length - 3].1, Tok::Dedent);
        assert_eq!(tokens[length - 2].1, Tok::Dedent);
        assert_eq!(tokens[length - 1].1, Tok::EOF);
    }
}
