#![feature(conservative_impl_trait, specialization)]

use std::rc::{Rc, Weak};
use std::cell::RefCell;

pub mod utilities;
#[cfg(test)]
pub mod test;

#[derive(Debug)]
pub enum ParseError {
    RcKerfluffle,
    ExpectedString{
        expected: String,
        position: Position,
    },
    ExpectedBytes{
        expected: Vec<u8>,
        position: Position,
    },
    ExpectedChar {
        expected: char,
        position: Position,
    },
    ExpectedDetail {
        detail: &'static str,
        position: Position
    },
    ReachedEof,
}

pub type ParseResult<'a, T> = Result<(T, &'a [u8], Position), ParseError>;

#[derive(Clone, Debug)]
pub struct Position;

pub trait Parser<'a> {
    type Out;
    fn parse(&self, input: &'a [u8], position: Position) -> ParseResult<'a, Self::Out>;
}

impl <'c, 'r> Parser<'r> for &'c [u8] {
    type Out = &'r [u8];

    fn parse(&self, input: &'r [u8], position: Position) -> ParseResult<'r, Self::Out> {
        let s = *self;
        if input.starts_with(s) {
            let (before, after) = input.split_at(s.len());
            Ok((before, after, Position))
        } else {
            Err(ParseError::ExpectedBytes {
                expected: s.into(),
                position: position,
            })
        }
    }
}

impl <'c, 'r> Parser<'r> for &'c str {
    type Out = &'r str;

    fn parse(&self, input: &'r [u8], position: Position) -> ParseResult<'r, Self::Out> {
        let s = *self;
        if input.starts_with(s.as_bytes()) {
            let (before, after) = input.split_at(s.len());
            let before = unsafe{ ::std::str::from_utf8_unchecked(before) };
            Ok((before, after, Position))
        } else {
            Err(ParseError::ExpectedBytes {
                expected: s.into(),
                position: position,
            })
        }
    }
}

impl <'r> Parser<'r> for char {
    type Out = char;

    fn parse(&self, input: &'r [u8], position: Position) -> ParseResult<'r, Self::Out> {
        let c = *self;
        let mut stack_char = [0u8; 4];
        let encoded_len = c.encode_utf8(&mut stack_char).len();

        if input.starts_with(&stack_char[..encoded_len]) {
            let (_, after) = input.split_at(c.len_utf8());
            Ok((c, after, Position))
        } else {
            Err(ParseError::ExpectedChar {
                expected: c,
                position: position,
            })
        }
    }
}

impl<'r, R, F> Parser<'r> for F
where F: Fn(&'r [u8], Position) -> ParseResult<R>,
{
    type Out = R;
    fn parse(&self, input: &'r [u8], position: Position) -> ParseResult<'r, R> {
        self(input, position)
    }
}

impl<'r, R> Parser<'r> for Box<Parser<'r, Out=R>>
{
    type Out = R;
    fn parse(&self, input: &'r [u8], position: Position) -> ParseResult<'r, R> {
        (**self).parse(input, position)
    }
}

pub fn and_then<'r, A, B, R, F>(a: A, f: F) -> impl Parser<'r, Out = B::Out> 
where 
    A: Parser<'r>,
    B: Parser<'r>,
    F: Fn(A::Out) -> B 
{
    move |input, position| {
        let (r_a, input, position) = a.parse(input, position)?;
        let b = f(r_a);
        let (r_b, input, position) = b.parse(input, position)?;
        Ok((r_b, input, position))
    }
}

pub fn map<'r, A, F, R>(a: A, f: F) -> impl Parser<'r, Out = R> 
where A: Parser<'r>,
      F: Fn(A::Out) -> R 
{
    move |input, position| {
        match a.parse(input, position) {
            Ok((r, next, pos)) => Ok((f(r), next, pos)),
            Err(e) => Err(e),
        }
    }
}

pub fn and<'r, A, B>(a: A, b: B) -> impl Parser<'r, Out = (A::Out, B::Out)>
where
    A: Parser<'r>,
    B: Parser<'r>,
{
    move |input, position| {
        let (r_a, input, position) = a.parse(input, position)?;
        let (r_b, input, position) = b.parse(input, position)?;
        Ok(((r_a, r_b), input, position))
    }
}

pub fn or<'r, A, B, R>(a: A, b: B) -> impl Parser<'r, Out = R>
where
    A: Parser<'r, Out=R>,
    B: Parser<'r, Out=R>,
{
    move |input, position: Position| {
        match a.parse(input, position.clone()) {
            r@Ok(_) => r,
            Err(_) => b.parse(input, position)
        }
    }
}

pub fn ignore_left<'r, A, B>(a: A, b: B) -> impl Parser<'r, Out = B::Out> 
where
    A: Parser<'r>,
    B: Parser<'r>,
{
    map(and(a, b), |(_, br)| br)
}

pub fn ignore_right<'r, A, B>(a: A, b: B) -> impl Parser<'r, Out = A::Out> 
where
    A: Parser<'r>,
    B: Parser<'r>,
{
    map(and(a, b), |(ar, _)| ar)
}

pub fn surrounded_by<'r, L, R, A>(left: L, a: A, right: R) -> impl Parser<'r, Out = A::Out>
where
    A: Parser<'r>,
    L: Parser<'r>,
    R: Parser<'r>,
{
    ignore_right(ignore_left(left, a), right)
}

pub fn as_ref<'r, 'x, P>(p: &'x P) -> impl Parser<'r, Out= P::Out>  
where P: Parser<'r> {
    move |input, position| p.parse(input, position)
}

pub fn optional<'r, P>(p: P) -> impl Parser<'r, Out = Option<P::Out>> 
where P: Parser<'r> {
    move |input, position: Position| {
        match p.parse(input, position.clone())  {
            Ok((r, next, pos)) => Ok((Some(r), next, pos)),
            Err(_) => Ok((None, input, position)),
        }
    }
}

pub fn self_reference<'r, O, R, F>(f: F) -> impl Parser<'r, Out= O>
where
    F: FnOnce(Box<Parser<'r, Out= O>>) -> R,
    R: Parser<'r, Out= O> + 'static,
    O: 'static,
{
    let parser = Rc::new(RefCell::new(None));
    let weak = Rc::downgrade(&parser);
    *parser.borrow_mut() = Some(f(Box::new(weak)));
    parser
}


impl<'r, P> Parser<'r> for Rc<RefCell<Option<P>>> 
where P: Parser<'r> {
    type Out = P::Out;
    fn parse(&self, input: &'r [u8], position: Position) -> ParseResult<'r, P::Out> {
        match self.borrow().as_ref() {
            Some(parse_box) => parse_box.parse(input, position),
            None => Err(ParseError::RcKerfluffle),
        }
    }
}


impl<'r, P> Parser<'r> for Weak<RefCell<Option<P>>> 
where P: Parser<'r> {
    type Out = P::Out;
    fn parse(&self, input: &'r [u8], position: Position) -> ParseResult<'r, P::Out> {
        match self.upgrade() {
            Some(rc) => {
                match rc.borrow().as_ref() {
                    Some(parse_box) => parse_box.parse(input, position),
                    None => Err(ParseError::RcKerfluffle),
                }
            } 
            None => Err(ParseError::RcKerfluffle),
        }
    }
}

