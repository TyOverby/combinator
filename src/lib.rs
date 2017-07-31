#![feature(conservative_impl_trait)]
// extern crate tendril;

pub mod utilities;
#[cfg(test)]
pub mod test;

//use tendril::StrTendril;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

#[derive(Clone, Debug)]
pub enum ParseError {
    RcKerfluffle,
    ReachedEof,
    DidNotReachEof,
    Not(&'static str),
    NotS(String),
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

pub type ParseResult<'a, T> = Result<(T, &'a str), ParseError>;

pub trait Parser<'a> {
    type Output;

    fn parse(&self, input: &'a str) -> ParseResult<'a, Self::Output>;
}

impl<'a, O> Parser<'a> for Box<Parser<'a, Output = O>> {
    type Output = O;
    fn parse(&self, input: &'a str) -> ParseResult<'a, O> {
        (&**self).parse(input)
    }
}

impl<'a, O> Parser<'a> for Rc<Parser<'a, Output = O>> {
    type Output = O;
    fn parse(&self, input: &'a str) -> ParseResult<'a, O> {
        (&**self).parse(input)
    }
}

impl<'a, O> Parser<'a> for Rc<RefCell<Option<Box<Parser<'a, Output = O>>>>> {
    type Output = O;
    fn parse(&self, input: &'a str) -> ParseResult<'a, O> {
        match self.borrow().as_ref() {
            Some(parse_box) => parse_box.parse(input),
            None => Err(ParseError::RcKerfluffle),
        }
    }
}


impl<'a, O> Parser<'a> for Weak<RefCell<Option<Box<Parser<'a, Output = O>>>>> {
    type Output = O;
    fn parse(&self, input: &'a str) -> ParseResult<'a, O> {
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

impl<'a, R, F> Parser<'a> for F
where
    F: Fn(&'a str) -> ParseResult<'a, R>,
{
    type Output = R;
    fn parse(&self, input: &'a str) -> ParseResult<'a, R> {
        self(input)
    }
}

pub fn string<'a, S: Into<String>>(s: S) -> impl Parser<'a, Output = &'a str> {
    let s: String = s.into();
    move |input: &'a str| if input.starts_with(&s[..]) {
        let (before, after) = input.split_at(s.len());
        Ok((before, after))
    } else {
        Err(ParseError::NotS(s.clone()))
    }
}

pub fn char<'a>(c: char) -> impl Parser<'a, Output = char> {
    move |input: &'a str| {
        if input.chars().next() == Some(c) {
            let len = c.len_utf8();
            let after = &input[len..];
            Ok((c, after))
        } else {
            Err(ParseError::ReachedEof)
        }
    }
}

pub fn and<'a, A, B>(a: A, b: B) -> impl Parser<'a, Output = (A::Output, B::Output)>
where
    A: Parser<'a>,
    B: Parser<'a>,
{
    move |input| {
        let (r_a, input) = a.parse(input)?;
        let (r_b, input) = b.parse(input)?;
        Ok(((r_a, r_b), input))
    }
}

pub fn and_then<'a, A, B, F>(a: A, f: F) -> impl Parser<'a, Output = B::Output>
where
    A: Parser<'a>,
    B: Parser<'a>,
    F: Fn(A::Output) -> B
{
    move |input| {
        let (r_a, input) = a.parse(input)?;
        let b = f(r_a);
        let (r_b, input) = b.parse(input)?;
        Ok((r_b, input))
    }
}

pub fn ignore_and<'a, A, B>(a: A, b: B) -> impl Parser<'a, Output = B::Output>
where
    A: Parser<'a>,
    B: Parser<'a>,
{
    move |input| {
        let (_, input) = a.parse(input)?;
        let (r, input) = b.parse(input)?;
        Ok((r, input))
    }
}

pub fn and_ignore<'a, A, B>(a: A, b: B) -> impl Parser<'a, Output = A::Output>
where
    A: Parser<'a>,
    B: Parser<'a>,
{
    move |input| {
        let (r, input) = a.parse(input)?;
        let (_, input) = b.parse(input)?;
        Ok((r, input))
    }
}

pub fn or<'a, R, A, B>(a: A, b: B) -> impl Parser<'a, Output = R>
where
    A: Parser<'a, Output = R>,
    B: Parser<'a, Output = R>,
{
    move |input: &'a str| {
        a.parse(input.clone()).or_else(|e| {
            b.parse(input).map_err(|a| a.combine(e))
        })
    }
}

pub fn optional<'a, A>(a: A) -> impl Parser<'a, Output = Option<A::Output>>
where
    A: Parser<'a>,
{
    move |input: &'a str| match a.parse(input.clone()) {
        Ok((r, next)) => Ok((Some(r), next)),
        Err(_) => Ok((None, input)),
    }
}

pub fn map<'a, I, O, A, F>(a: A, f: F) -> impl Parser<'a, Output = O>
where
    A: Parser<'a, Output = I>,
    F: Fn(I) -> O,
{
    move |input| match a.parse(input) {
        Ok((r, next)) => Ok((f(r), next)),
        Err(e) => Err(e),
    }
}


/*
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

    move |input: | {
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
}*/


pub fn surround_chars<'a, A>(left: char, a: A, right: char) -> impl Parser<'a, Output = A::Output>
where
    A: Parser<'a>,
{
    and_ignore(ignore_and(char(left), a), char(right))
}

/*
pub fn surround_string<'a, A>(left: &'a str, a: A, right: &'a str) -> impl Parser<'a, Output = A::Output>
where
    A: Parser<'a>,
{
    and_ignore(ignore_and(string(left), a), string(right))
}*/



// TODO: test this please oh god
pub fn self_reference<'a, O, R, F>(f: F) -> impl Parser<'a, Output = O>
where
    F: FnOnce(Box<Parser<Output = O>>) -> R,
    R: Parser<'a, Output = O> + 'static,
    O: 'static,
{
    let parser = Rc::new(RefCell::new(None));
    let weak = Rc::downgrade(&parser);

    let f = |input: &'a str| {
        weak.parse(input)
    };

    *parser.borrow_mut() = Some(Box::new(f) as Box<Parser<'a, Output = O>>);

    parser
}

pub fn shared<'a, A>(a: A) -> impl Parser<'a, Output = A::Output> + Clone
where
    A: Parser<'a> + 'static,
{
    Rc::new(a) as Rc<Parser<Output = A::Output>>
}

pub fn many<'a, A>(a: A) -> impl Parser<'a, Output = Vec<A::Output>>
where A: Parser<'a> {
    move |mut input: &'a str| {
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

pub fn many1<'a, A>(a: A) -> impl Parser<'a, Output = Vec<A::Output>>
where A: Parser<'a> {
    move |mut input: &'a str| {
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

pub fn many_sep<'a, A, B>(a: A, sep: B) -> impl Parser<'a, Output = Vec<A::Output>>
where A: Parser<'a>,
      B: Parser<'a> {
    move |mut input: &'a str| {
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

pub fn many1_sep<'a, A, B>(a: A, sep: B) -> impl Parser<'a, Output = Vec<A::Output>>
where A: Parser<'a>,
      B: Parser<'a> {
    move |mut input: &'a str| {
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
