#![feature(conservative_impl_trait)]
extern crate tendril;

use tendril::StrTendril;

#[derive(Clone)]
pub enum ParseError {
    Bad,
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

    fn parse(&mut self, input: StrTendril) -> ParseResult<Self::Output>;
}

impl <O> Parser for Box<Parser<Output=O>> {
    type Output = O;
    fn parse(&mut self, input: StrTendril) -> ParseResult<O> {
        (&mut **self).parse(input)
    }
}

/*
impl <O> Parser for Rc<RefCell<Box<Parser<Output=O>>>> {
    type Output = O;
    fn parse(&mut self, input: StrTendril) -> ParseResult<O> {
        (&mut **self).parse(input)
    }
}*/

impl<R, F> Parser for F
where
    F: FnMut(StrTendril) -> ParseResult<R>,
{
    type Output = R;
    fn parse(&mut self, input: StrTendril) -> ParseResult<R> {
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
        Err(ParseError::Bad)
    }
}

pub fn char(c: char) -> impl Parser<Output = char> {
    move |input: StrTendril| if input.as_ref().chars().next() == Some(c) {
        let len = c.len_utf8() as u32;
        let after = input.subtendril(len, input.len32() - len);
        Ok((c, after))
    } else {
        Err(ParseError::Bad)
    }
}

pub fn and<A, B>(mut a: A, mut b: B) -> impl Parser<Output = (A::Output, B::Output)>
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

pub fn ignore_and<A, B>(mut a: A, mut b: B) -> impl Parser<Output = B::Output>
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

pub fn and_ignore<A, B>(mut a: A, mut b: B) -> impl Parser<Output = A::Output>
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

pub fn or<R, A, B>(mut a: A, mut b: B) -> impl Parser<Output = R>
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

pub fn optional<A>(mut a: A) -> impl Parser<Output = Option<A::Output>>
where
    A: Parser,
{
    move |input: StrTendril| match a.parse(input.clone()) {
        Ok((r, next)) => Ok((Some(r), next)),
        Err(_) => Ok((None, input)),
    }
}

pub fn map<I, O, A, F>(mut a: A, f: F) -> impl Parser<Output = O>
where
    A: Parser<Output = I>,
    F: Fn(I) -> O,
{
    move |input| match a.parse(input) {
        Ok((r, next)) => Ok((f(r), next)),
        Err(e) => Err(e),
    }
}

pub fn memoize<R, A>(mut a: A) -> impl Parser<Output = R>
where
    R: Clone,
    A: Parser<Output = R>,
{
    use std::collections::HashMap;
    use std::mem::transmute;
    let mut map: HashMap<usize, ParseResult<A::Output>> = HashMap::new();

    move |input: StrTendril| {
        let input_ptr: usize = unsafe { transmute(input.as_ref().as_ptr()) };
        let r = a.parse(input);
        map.insert(input_ptr, r.clone());
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


/*
pub fn self_reference<F, A>(f: F) -> impl Parser<Output = A::Output> 
where F: FnOnce(A) -> A,
      A: Parser,
      A::Output: 'static
{
    let mut parser: Option<Box<Parser<Output=A::Output>>> = None;

    let result = move |input| {
        parser.as_mut().unwrap().parse(input)
    };

    parser = Some(Box::new(result));

    result 
}
*/
