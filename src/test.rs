use super::*;
use super::utilities::*;

fn parse_ok<'r, P>(input: &'r[u8], parser: P) -> P::Out
where
    P: Parser<'r>,
{
    let input = input.as_ref();
    parser.parse(input, Position).unwrap().0
}

fn parse_err<'r, P>(input: &'r[u8], parser: P)
where
    P: Parser<'r>,
{
    assert!(parser.parse(input, Position).is_err());
}

#[test]
fn basic_string_parser() {
    assert_eq!(parse_ok(b"abc", "abc"), "abc");

    assert_eq!(parse_ok(b"abcdef", and("abc", "def")), (
        "abc",
        "def",
    ));

    parse_err(b"xyz", "abc");
    parse_err(b"ab", "abc");
}

#[test]
fn basic_char_parser() {
    assert_eq!(parse_ok(b"a", 'a'), 'a');

    assert_eq!(parse_ok(b"ab", and('a', 'b')), ('a', 'b'));

    parse_err(b"z", 'a');
    parse_err(b"a", and('a', 'b'));
}

#[test]
fn rec() {
    let o = self_reference(|recurse| {
        map(
            optional(map(surrounded_by('(', recurse, ')'), |x| x + 1)),
            |o| o.unwrap_or(0),
        )
    });

    assert_eq!(parse_ok(b"", as_ref(&o)), 0);
    assert_eq!(parse_ok(b"()", as_ref(&o)), 1);
    assert_eq!(parse_ok(b"((()))", as_ref(&o)), 3);

    /*
     * THIS IS BROKEN AND YOU SHOULD FEEL BAD
    let p = shared(memoize(p));

    assert_eq!(parse_ok("", p.clone()), 0);
    assert_eq!(parse_ok("()", p.clone()), 1);
    assert_eq!(parse_ok("((()))", p.clone()), 3);
    */
}

#[test]
fn ident() {
    assert_eq!(parse_ok(b"abc_123", identifier), "abc_123");
    assert_eq!(parse_ok(b"_abc", identifier), "_abc");
    parse_err(b"123abc", identifier);
}

#[test]
fn str_lit() {
    assert_eq!(parse_ok(b"\"abc123\"", string_literal), b"abc123");
    assert_eq!(
        parse_ok(b"\"abc\\\"123\"", string_literal),
        b"abc\\\"123"
    );
    assert_eq!(
        parse_ok(
            b"\"abc\"\"123\"", 
            and(string_literal, string_literal)), 
        (b"abc".as_ref(), b"123".as_ref()));
    parse_err(b"\"foo", string_literal);
    parse_err(b"foo", string_literal);
}

#[test]
fn digit_test() {
    assert_eq!(parse_ok(b"0", digit), 0);
    assert_eq!(parse_ok(b"1", digit), 1);
    assert_eq!(parse_ok(b"2", digit), 2);
    assert_eq!(parse_ok(b"3", digit), 3);
    assert_eq!(parse_ok(b"4", digit), 4);
    assert_eq!(parse_ok(b"5", digit), 5);
    assert_eq!(parse_ok(b"6", digit), 6);
    assert_eq!(parse_ok(b"7", digit), 7);
    assert_eq!(parse_ok(b"8", digit), 8);
    assert_eq!(parse_ok(b"9", digit), 9);

    parse_err(b"a", digit);
    parse_err(b"-", digit);
    parse_err(b"", digit);
}

#[test]
fn alpha_test() {
    assert_eq!(parse_ok(b"a", alpha), 'a');
    assert_eq!(parse_ok(b"b", alpha), 'b');
    assert_eq!(parse_ok(b"Z", alpha), 'Z');

    parse_err(b"-", alpha);
    parse_err(b"2", alpha);
    parse_err(b"", alpha);
}

#[test]
fn alpha_numeric_test() {
    assert_eq!(parse_ok(b"a", alphanumeric), 'a');
    assert_eq!(parse_ok(b"b", alphanumeric), 'b');
    assert_eq!(parse_ok(b"Z", alphanumeric), 'Z');
    assert_eq!(parse_ok(b"0", alphanumeric), '0');
    assert_eq!(parse_ok(b"1", alphanumeric), '1');
    assert_eq!(parse_ok(b"2", alphanumeric), '2');
    assert_eq!(parse_ok(b"3", alphanumeric), '3');
    assert_eq!(parse_ok(b"4", alphanumeric), '4');
    assert_eq!(parse_ok(b"5", alphanumeric), '5');
    assert_eq!(parse_ok(b"6", alphanumeric), '6');
    assert_eq!(parse_ok(b"7", alphanumeric), '7');
    assert_eq!(parse_ok(b"8", alphanumeric), '8');
    assert_eq!(parse_ok(b"9", alphanumeric), '9');

    parse_err(b"-", alphanumeric);
    parse_err(b"@", alphanumeric);
    parse_err(b"", alphanumeric);
}

/*
#[test]
fn many_test() {
    assert_eq!(parse_ok("123", many(digit)), vec![1,2,3]);
    assert_eq!(parse_ok("1", many(digit)), vec![1]);
    assert_eq!(parse_ok("", many(digit)), vec![]);
}

#[test]
fn many1_test() {
    assert_eq!(parse_ok("123", many1(digit)), vec![1,2,3]);
    assert_eq!(parse_ok("1", many1(digit)), vec![1]);
    parse_err("", many1(digit));
}

#[test]
fn many_sep_test() {
    assert_eq!(parse_ok("1,2,3", many_sep(digit, char(','))), vec![1,2,3]);
    assert_eq!(parse_ok("1", many_sep(digit, char(','))), vec![1]);
    assert_eq!(parse_ok("", many_sep(digit, char(','))), vec![]);
}

#[test]
fn many1_sep_test() {
    assert_eq!(parse_ok("1,2,3", many1_sep(digit, char(','))), vec![1,2,3]);
    assert_eq!(parse_ok("1", many1_sep(digit, char(','))), vec![1]);
    parse_err("", many1_sep(digit, char(',')));
}*/
