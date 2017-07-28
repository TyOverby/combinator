use super::*;
use super::utilities::*;

fn parse_ok<P>(input: &str, parser: P) -> P::Output
where
    P: Parser,
{
    parser.parse(input.into()).unwrap().0
}

fn parse_err<P>(input: &str, parser: P)
where
    P: Parser,
{
    assert!(parser.parse(input.into()).is_err());
}

#[test]
fn basic_string_parser() {
    assert_eq!(parse_ok("abc", string("abc")), "abc".into());

    assert_eq!(parse_ok("abcdef", and(string("abc"), string("def"))), (
        "abc".into(),
        "def".into(),
    ));

    parse_err("xyz", string("abc"));
    parse_err("ab", string("abc"));
}

#[test]
fn basic_char_parser() {
    assert_eq!(parse_ok("a", char('a')), 'a');

    assert_eq!(parse_ok("ab", and(char('a'), char('b'))), ('a', 'b'));

    parse_err("z", char('a'));
    parse_err("a", and(char('a'), char('b')));
}

#[test]
fn rec() {
    let o = self_reference(|recurse| {
        map(
            optional(map(surround_chars('(', recurse, ')'), |x| x + 1)),
            |o| o.unwrap_or(0),
        )
    });
    let p = shared(o);

    assert_eq!(parse_ok("", p.clone()), 0);
    assert_eq!(parse_ok("()", p.clone()), 1);
    assert_eq!(parse_ok("((()))", p.clone()), 3);

    /*
     * BROKEN
    let p = shared(memoize(p));

    assert_eq!(parse_ok("", p.clone()), 0);
    assert_eq!(parse_ok("()", p.clone()), 1);
    assert_eq!(parse_ok("((()))", p.clone()), 3);
    */
}

#[test]
fn ident() {
    assert_eq!(parse_ok("abc_123", identifier), "abc_123".into());
    assert_eq!(parse_ok("_abc", identifier), "_abc".into());
    parse_err("123abc", identifier);
}

#[test]
fn str_lit() {
    assert_eq!(parse_ok("\"abc123\"", string_literal), "abc123".into());
    assert_eq!(
        parse_ok("\"abc\\\"123\"", string_literal),
        "abc\\\"123".into()
    );
    assert_eq!(
        parse_ok(
            "\"abc\"\"123\"", 
            and(string_literal, string_literal)), 
        ("abc".into(), "123".into()));
    parse_err("\"foo", string_literal);
    parse_err("foo", string_literal);
}
