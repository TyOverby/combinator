use super::{ParseResult, ParseError};
use tendril::StrTendril;

fn after(tendril: StrTendril, n: u32) -> StrTendril {
    tendril.subtendril(n, tendril.len32() - n)
}

pub fn digit(input: StrTendril) -> ParseResult<u8> {
    match input.chars().next() {
        Some('0') => Ok((0, after(input, 1))),
        Some('1') => Ok((1, after(input, 1))),
        Some('2') => Ok((2, after(input, 1))),
        Some('3') => Ok((3, after(input, 1))),
        Some('4') => Ok((4, after(input, 1))),
        Some('5') => Ok((5, after(input, 1))),
        Some('6') => Ok((6, after(input, 1))),
        Some('7') => Ok((7, after(input, 1))),
        Some('8') => Ok((8, after(input, 1))),
        Some('9') => Ok((9, after(input, 1))),

        Some(_) => Err(ParseError::Not("number")),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn alpha(input: StrTendril) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) if c.is_alphabetic() => Ok((c, after(input, c.len_utf8() as u32))),
        Some(_) => Err(ParseError::Not("alphabetic")),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn alphanumeric(input: StrTendril) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) if c.is_alphanumeric() => Ok((c, after(input, c.len_utf8() as u32))),
        Some(_) => Err(ParseError::Not("alphanumeric")),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn identifier(input: StrTendril) -> ParseResult<StrTendril> {
    let mut end = 0;

    for c in input.chars() {
        if end == 0 {
            if c.is_alphabetic() || c == '_' {
                end += c.len_utf8();
            } else {
                break;
            }
        } else {
            if c.is_alphanumeric() || c == '_' {
                end += c.len_utf8();
            } else {
                break;
            }
        }
    }

    if end == 0 {
        Err(ParseError::Not("identifier"))
    } else {
        let left = input.subtendril(0, end as u32);
        let right = after(input, end as u32);
        Ok((left, right))
    }
}

pub fn string_literal(input: StrTendril) -> ParseResult<StrTendril> {
    let mut escaped = false;
    let mut end = 0;
    let mut first = true;
    let mut is_closed = false;

    for c in input.chars() {
        if first {
            if c != '"' {
                return Err(ParseError::Not("opening quote"));
            }
            first = false;
            continue;
        }

        match c {
            '\\' => {
                escaped = !escaped;
            }
            '"' if !escaped => {
                is_closed = true;
                break;
            }
            _ if escaped => {
                escaped = false;
            }
            _ => {}
        }
        end += c.len_utf8();
    }

    if !is_closed {
        return Err(ParseError::Not("closing quote"));
    }

    let quote_size = '"'.len_utf8() as u32;
    let left = input.subtendril(quote_size, end as u32);
    let right = after(input, 2 * quote_size + end as u32);
    Ok((left, right))
}
