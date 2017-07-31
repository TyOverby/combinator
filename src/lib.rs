#![feature(conservative_impl_trait)]
extern crate tendril;

pub mod utilities;
#[cfg(test)]
pub mod test;

use tendril::StrTendril;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

#[derive(Clone, Debug)]
pub enum ParseError {
    RcKerfluffle,
    ReachedEof,
    DidNotReachEof,
    Not(&'static str),
    ExpectedOther(StrTendril),
    Alternate(Vec<ParseError>),
}

impl ParseError {
    fn combine(self, other: ParseError) -> ParseError {
        use ParseError::*;
        match (self, other) {
            (Alternate(mut v1), Alternate(mut v2)) => {
                v1.append(&mut v2);
                Alternate(v1)
            }
            (Alternate(mut v1), other) => {
                v1.push(other);
                Alternate(v1)
            }
            (other, Alternate(mut v1)) => {
                v1.push(other);
                Alternate(v1)
            }
            (a, b) => Alternate(vec![a, b]),
        }
    }
}

pub type ParseResult<T> = Result<(T, StrTendril), ParseError>;

pub trait Parser {
    type Output;

    fn parse(&self, input: StrTendril) -> ParseResult<Self::Output>;
}

impl<O> Parser for Box<Parser<Output = O>> {
    type Output = O;
    fn parse(&self, input: StrTendril) -> ParseResult<O> {
        (&**self).parse(input)
    }
}

impl<O> Parser for Rc<Parser<Output = O>> {
    type Output = O;
    fn parse(&self, input: StrTendril) -> ParseResult<O> {
        (&**self).parse(input)
    }
}

impl<O> Parser for Rc<RefCell<Option<Box<Parser<Output = O>>>>> {
    type Output = O;
    fn parse(&self, input: StrTendril) -> ParseResult<O> {
        match self.borrow().as_ref() {
            Some(parse_box) => parse_box.parse(input),
            None => Err(ParseError::RcKerfluffle),
        }
    }
}


impl<O> Parser for Weak<RefCell<Option<Box<Parser<Output = O>>>>> {
    type Output = O;
    fn parse(&self, input: StrTendril) -> ParseResult<O> {
        match self.upgrade() {
            Some(rc) => {
                match rc.borrow().as_ref() {
                    Some(parse_box) => parse_box.parse(input),
                    None => Err(ParseError::RcKerfluffle),
                }
            } 
            None => Err(ParseError::RcKerfluffle),
        }
    }
}

impl<R, F> Parser for F
where
    F: Fn(StrTendril) -> ParseResult<R>,
{
    type Output = R;
    fn parse(&self, input: StrTendril) -> ParseResult<R> {
        self(input)
    }
}

pub fn string<S: Into<StrTendril>>(s: S) -> impl Parser<Output = StrTendril> {
    let s = s.into();
    move |input: StrTendril| if input.as_ref().starts_with(s.as_ref()) {
        let len = s.len32();
        let before = input.subtendril(0, len);
        let after = input.subtendril(len, input.len32() - len);
        Ok((before, after))
    } else {
        Err(ParseError::ExpectedOther(s.clone()))
    }
}

pub fn char(c: char) -> impl Parser<Output = char> {
    move |input: StrTendril| if input.as_ref().chars().next() == Some(c) {
        let len = c.len_utf8() as u32;
        let after = input.subtendril(len, input.len32() - len);
        Ok((c, after))
    } else {
        Err(ParseError::ReachedEof)
    }
}

pub fn and<A, B>(a: A, b: B) -> impl Parser<Output = (A::Output, B::Output)>
where
    A: Parser,
    B: Parser,
{
    move |input| {
        let (r_a, input) = a.parse(input)?;
        let (r_b, input) = b.parse(input)?;
        Ok(((r_a, r_b), input))
    }
}

pub fn ignore_and<A, B>(a: A, b: B) -> impl Parser<Output = B::Output>
where
    A: Parser,
    B: Parser,
{
    move |input| {
        let (_, input) = a.parse(input)?;
        let (r, input) = b.parse(input)?;
        Ok((r, input))
    }
}

pub fn and_ignore<A, B>(a: A, b: B) -> impl Parser<Output = A::Output>
where
    A: Parser,
    B: Parser,
{
    move |input| {
        let (r, input) = a.parse(input)?;
        let (_, input) = b.parse(input)?;
        Ok((r, input))
    }
}

pub fn or<R, A, B>(a: A, b: B) -> impl Parser<Output = R>
where
    A: Parser<Output = R>,
    B: Parser<Output = R>,
{
    move |input: StrTendril| {
        a.parse(input.clone()).or_else(|e| {
            b.parse(input).map_err(|a| a.combine(e))
        })
    }
}

pub fn optional<A>(a: A) -> impl Parser<Output = Option<A::Output>>
where
    A: Parser,
{
    move |input: StrTendril| match a.parse(input.clone()) {
        Ok((r, next)) => Ok((Some(r), next)),
        Err(_) => Ok((None, input)),
    }
}

pub fn map<I, O, A, F>(a: A, f: F) -> impl Parser<Output = O>
where
    A: Parser<Output = I>,
    F: Fn(I) -> O,
{
    move |input| match a.parse(input) {
        Ok((r, next)) => Ok((f(r), next)),
        Err(e) => Err(e),
    }
}


// TODO: this is unsound.
pub fn memoize<R, A>(a: A) -> impl Parser<Output = R>
where
    R: Clone,
    A: Parser<Output = R>,
{
    use std::collections::HashMap;
    use std::mem::transmute;
    let map: HashMap<usize, ParseResult<A::Output>> = HashMap::new();
    let cell = RefCell::new(map);

    move |input: StrTendril| {
        let input_ptr: usize = unsafe { transmute(input.as_ref().as_ptr()) };
        // lookup
        {
            let map = cell.borrow();
            if let Some(res) = map.get(&input_ptr) {
                return res.clone();
            }
        }

        let r = a.parse(input);
        // store
        cell.borrow_mut().insert(input_ptr, r.clone());
        r
    }
}


pub fn surround_chars<A>(left: char, a: A, right: char) -> impl Parser<Output = A::Output>
where
    A: Parser,
{
    and_ignore(ignore_and(char(left), a), char(right))
}

pub fn surround_string<L, R, A>(left: L, a: A, right: R) -> impl Parser<Output = A::Output>
where
    L: Into<StrTendril>,
    R: Into<StrTendril>,
    A: Parser,
{
    and_ignore(ignore_and(string(left), a), string(right))
}


// TODO: test this please oh god
pub fn self_reference<O, R, F>(f: F) -> impl Parser<Output = O>
where
    F: FnOnce(Box<Parser<Output = O>>) -> R,
    R: Parser<Output = O> + 'static,
    O: 'static,
{
    let parser = Rc::new(RefCell::new(None));
    let weak = Rc::downgrade(&parser);
    *parser.borrow_mut() = Some(Box::new(f(Box::new(weak))) as
        Box<Parser<Output = O> + 'static>);
    parser
}

pub fn shared<A>(a: A) -> impl Parser<Output = A::Output> + Clone
where
    A: Parser + 'static,
{
    Rc::new(a) as Rc<Parser<Output = A::Output>>
}

pub fn many<A>(a: A) -> impl Parser<Output = Vec<A::Output>>
where A: Parser {
    move |mut input: StrTendril| {
        let mut out = vec![];
        loop {
            match a.parse(input.clone()) {
                Ok((r, n)) => {
                    out.push(r);
                    input = n;
                }
                Err(_) => break,
            }
        }
        Ok((out, input))
    }
}

pub fn many1<A>(a: A) -> impl Parser<Output = Vec<A::Output>>
where A: Parser {
    move |mut input: StrTendril| {
        let mut out = vec![];
        loop {
            match a.parse(input.clone()) {
                Ok((r, n)) => {
                    out.push(r);
                    input = n;
                }
                Err(e) => {
                    if out.len() == 0 {  
                        return Err(e) 
                    } else {
                        break;
                    }
                }
            }
        }
        Ok((out, input))
    }
}

pub fn many_sep<A, B>(a: A, sep: B) -> impl Parser<Output = Vec<A::Output>>
where A: Parser,
      B: Parser {
    move |mut input: StrTendril| {
        let mut out = vec![];
        loop {
            match a.parse(input.clone()) {
                Ok((r, n)) => {
                    out.push(r);
                    if let Ok((_, n)) = sep.parse(n) {
                        input = n;
                    } else {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        Ok((out, input))
    }
}

pub fn many1_sep<A, B>(a: A, sep: B) -> impl Parser<Output = Vec<A::Output>>
where A: Parser,
      B: Parser {
    move |mut input: StrTendril| {
        let mut out = vec![];
        loop {
            match a.parse(input.clone()) {
                Ok((r, n)) => {
                    out.push(r);
                    if let Ok((_, n)) = sep.parse(n) {
                        input = n;
                    } else {
                        break;
                    }
                }
                Err(e) => {
                    if out.len() == 0 {
                        return Err(e)
                    } else {
                        break;
                    }
                }
            }
        }
        Ok((out, input))
    }
}
