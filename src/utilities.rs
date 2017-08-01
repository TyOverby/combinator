use super::{ParseResult, ParseError, Position};

pub fn digit(input: &[u8], position: Position) -> ParseResult<u8> {
    match input.first().map(|&x|x) {
        Some(b'0') => Ok((0, &input[1..], Position)),
        Some(b'1') => Ok((1, &input[1..], Position)),
        Some(b'2') => Ok((2, &input[1..], Position)),
        Some(b'3') => Ok((3, &input[1..], Position)),
        Some(b'4') => Ok((4, &input[1..], Position)),
        Some(b'5') => Ok((5, &input[1..], Position)),
        Some(b'6') => Ok((6, &input[1..], Position)),
        Some(b'7') => Ok((7, &input[1..], Position)),
        Some(b'8') => Ok((8, &input[1..], Position)),
        Some(b'9') => Ok((9, &input[1..], Position)),

        Some(_) => Err(ParseError::ExpectedDetail {
            detail: "number",
            position: position,
        }),
        None => Err(ParseError::ReachedEof),
    }
}

pub fn alpha(input: &[u8], position: Position) -> ParseResult<char> {
    if input.len() == 0 {
        return Err(ParseError::ReachedEof) 
    } 

    match input[0] {
        b'a' ... b'z' => Ok((input[0] as char, &input[1..], Position)),
        b'A' ... b'Z' => Ok((input[0] as char, &input[1..], Position)),
        _ => Err(ParseError::ExpectedDetail {
            detail: "alphabetic character",
            position: position,
        })
    }
}

pub fn alphanumeric(input: &[u8], position: Position) -> ParseResult<char> {
    if input.len() == 0 {
        return Err(ParseError::ReachedEof) 
    } 

    match input[0] {
        b'a' ... b'z' => Ok((input[0] as char, &input[1..], Position)),
        b'A' ... b'Z' => Ok((input[0] as char, &input[1..], Position)),
        b'0' ... b'9' => Ok((input[0] as char, &input[1..], Position)),
        _ => Err(ParseError::ExpectedDetail {
            detail: "alphanumeric character",
            position: position,
        })
    }
}

pub fn identifier(input: &[u8], position: Position) -> ParseResult<&str> {
    let mut end = 0;

    for c in input.iter().cloned() {
        if end == 0 {
            if ((c >= b'a') && (c <= b'z')) || ((c >= b'A') && (c <= b'Z')) || c == b'_' {
                end += 1;
            } else {
                break;
            }
        } else {
            if (c >= b'0') && (c <= b'9') || ((c >= b'a') && (c <= b'z')) || ((c >= b'A') && (c <= b'Z')) || c == b'_' {
                end += 1;
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
        let left = unsafe {  ::std::str::from_utf8_unchecked(left) };
        Ok((left, right, Position))
    }
}

pub fn string_literal(input: &[u8], position: Position) -> ParseResult<&[u8]> {
    let mut escaped = false;
    let mut end = 0;
    let mut first = true;
    let mut is_closed = false;

    for c in input.iter().cloned() {
        if first {
            if c != b'"' {
                return Err(ParseError::ExpectedDetail{
                    detail: "opening quote",
                    position: position
                });
            }
            first = false;
            continue;
        }

        end += 1;
        match c {
            b'\\' => {
                escaped = !escaped;
            }
            b'"' if !escaped => {
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
