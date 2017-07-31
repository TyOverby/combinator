use super::{ParseResult, ParseError, Position};

pub fn digit(input: &str, position: Position) -> ParseResult<u8> {
    match input.chars().next() {
        Some('0') => Ok((0, &input[1..], Position)),
        Some('1') => Ok((1, &input[1..], Position)),
        Some('2') => Ok((2, &input[1..], Position)),
        Some('3') => Ok((3, &input[1..], Position)),
        Some('4') => Ok((4, &input[1..], Position)),
        Some('5') => Ok((5, &input[1..], Position)),
        Some('6') => Ok((6, &input[1..], Position)),
        Some('7') => Ok((7, &input[1..], Position)),
        Some('8') => Ok((8, &input[1..], Position)),
        Some('9') => Ok((9, &input[1..], Position)),

        Some(_) => Err(ParseError::ExpectedDetail {
            detail: "number",
            position: position,
        }),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn alpha(input: &str, position: Position) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) if c.is_alphabetic() => Ok((c, &input[c.len_utf8()..], Position)),
        Some(_) => Err(ParseError::ExpectedDetail{ 
            detail: "alphabetic",
            position: position,
        }),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn alphanumeric(input: &str, position: Position) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) if c.is_alphanumeric() => Ok((c, &input[c.len_utf8()..], Position)),
        Some(_) => Err(ParseError::ExpectedDetail { 
            detail: "alphanumeric",
            position: position,
        }),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn identifier(input: &str, position: Position) -> ParseResult<&str> {
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
        Err(ParseError::ExpectedDetail {
            detail: "identifier",
            position: position,
        })
    } else {
        let (left, right) = input.split_at(end);
        Ok((left, right, Position))
    }
}

pub fn string_literal(input: &str, position: Position) -> ParseResult<&str> {
    let mut escaped = false;
    let mut end = 0;
    let mut first = true;
    let mut is_closed = false;

    for c in input.chars() {
        if first {
            if c != '"' {
                return Err(ParseError::ExpectedDetail{
                    detail: "opening quote",
                    position: position
                });
            }
            first = false;
            continue;
        }

        end += c.len_utf8();
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
    }

    if !is_closed {
        return Err(ParseError::ExpectedDetail{
            detail: "closing quote",
            position: position,
        });
    }

    let quote_size = '"'.len_utf8();
    let left = &input[quote_size .. end];
    let right = &input[(quote_size + end) ..];
    Ok((left, right, Position))
}
