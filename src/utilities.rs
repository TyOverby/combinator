use super::{ParseResult, ParseError};

pub fn digit<'a>(input: &'a str) -> ParseResult<'a, u8> {
    match input.chars().next() {
        Some('0') => Ok((0, &input[1..])),
        Some('1') => Ok((1, &input[1..])),
        Some('2') => Ok((2, &input[1..])),
        Some('3') => Ok((3, &input[1..])),
        Some('4') => Ok((4, &input[1..])),
        Some('5') => Ok((5, &input[1..])),
        Some('6') => Ok((6, &input[1..])),
        Some('7') => Ok((7, &input[1..])),
        Some('8') => Ok((8, &input[1..])),
        Some('9') => Ok((9, &input[1..])),

        Some(_) => Err(ParseError::Not("number")),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn alpha<'a>(input: &'a str) -> ParseResult<'a, char> {
    match input.chars().next() {
        Some(c) if c.is_alphabetic() => Ok((c, &input[c.len_utf8()..])),
        Some(_) => Err(ParseError::Not("alphabetic")),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn alphanumeric<'a>(input: &'a str) -> ParseResult<'a, char> {
    match input.chars().next() {
        Some(c) if c.is_alphanumeric() => Ok((c, &input[c.len_utf8()..])),
        Some(_) => Err(ParseError::Not("alphanumeric")),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn identifier<'a>(input: &'a str) -> ParseResult<'a, &'a str> {
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
        let (left, right) = input.split_at(end);
        Ok((left, right))
    }
}

pub fn string_literal<'a>(input: &'a str) -> ParseResult<'a, &'a str> {
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

    let quote_size = '"'.len_utf8();
    let left = &input[quote_size .. end + 1];

    let right = &input[(2 * quote_size + end) ..];
    Ok((left, right))
}
