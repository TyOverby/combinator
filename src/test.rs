use super::*;
use super::utilities::*;

fn parse_ok<'a, P>(input: &'a str, parser: P) -> P::Output
where
    P: Parser<'a>,
{
    parser.parse(input.into()).unwrap().0
}

fn parse_err<'a, P>(input: &'a str, parser: P)
where
    P: Parser<'a>,
{
    assert!(parser.parse(input.into()).is_err());
}

#[test]
fn basic_string_parser() {
    assert_eq!(parse_ok("abc", string("abc")), "abc");

    assert_eq!(parse_ok("abcdef", and(string("abc"), string("def"))), 
               ("abc", "def"));

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

/*
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
*/

#[test]
fn ident() {
    assert_eq!(parse_ok("abc_123", identifier), "abc_123");
    assert_eq!(parse_ok("_abc", identifier), "_abc");
    parse_err("123abc", identifier);
}

#[test]
fn str_lit() {
    assert_eq!(parse_ok("\"abc123\"", string_literal), "abc123");
    assert_eq!(
        parse_ok("\"abc\\\"123\"", string_literal),
        "abc\\\"123"
    );
    assert_eq!(
        parse_ok(
            "\"abc\"\"123\"", 
            and(string_literal, string_literal)), 
        ("abc".into(), "123"));
    parse_err("\"foo", string_literal);
    parse_err("foo", string_literal);
}

#[test]
fn digit_test() {
    assert_eq!(parse_ok("0", digit), 0);
    assert_eq!(parse_ok("1", digit), 1);
    assert_eq!(parse_ok("2", digit), 2);
    assert_eq!(parse_ok("3", digit), 3);
    assert_eq!(parse_ok("4", digit), 4);
    assert_eq!(parse_ok("5", digit), 5);
    assert_eq!(parse_ok("6", digit), 6);
    assert_eq!(parse_ok("7", digit), 7);
    assert_eq!(parse_ok("8", digit), 8);
    assert_eq!(parse_ok("9", digit), 9);

    parse_err("a", digit);
    parse_err("-", digit);
    parse_err("", digit);
}

#[test]
fn alpha_test() {
    assert_eq!(parse_ok("a", alpha), 'a');
    assert_eq!(parse_ok("b", alpha), 'b');
    assert_eq!(parse_ok("Z", alpha), 'Z');

    parse_err("-", alpha);
    parse_err("2", alpha);
    parse_err("", alpha);
}

#[test]
fn alpha_numeric_test() {
    assert_eq!(parse_ok("a", alphanumeric), 'a');
    assert_eq!(parse_ok("b", alphanumeric), 'b');
    assert_eq!(parse_ok("Z", alphanumeric), 'Z');
    assert_eq!(parse_ok("0", alphanumeric), '0');
    assert_eq!(parse_ok("1", alphanumeric), '1');
    assert_eq!(parse_ok("2", alphanumeric), '2');
    assert_eq!(parse_ok("3", alphanumeric), '3');
    assert_eq!(parse_ok("4", alphanumeric), '4');
    assert_eq!(parse_ok("5", alphanumeric), '5');
    assert_eq!(parse_ok("6", alphanumeric), '6');
    assert_eq!(parse_ok("7", alphanumeric), '7');
    assert_eq!(parse_ok("8", alphanumeric), '8');
    assert_eq!(parse_ok("9", alphanumeric), '9');

    parse_err("-", alphanumeric);
    parse_err("@", alphanumeric);
    parse_err("", alphanumeric);
}

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
}

#[test]
fn and_then_test() {
    let dup2 = shared(and_then(alpha, |p| char(p)));
    assert_eq!(parse_ok("aa", dup2.clone()), 'a');
    parse_err("ab", dup2.clone());
}
