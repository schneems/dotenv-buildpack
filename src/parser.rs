#![allow(dead_code)]
#![allow(unused_imports)]

use std::ffi::OsString;

use nom::character::complete::char;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till, take_till1, take_while_m_n},
    character::complete::{alphanumeric1, anychar, multispace0, space0},
    combinator::map_res,
    error::ParseError,
    multi::many1,
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn hex_primary(input: &str) -> IResult<&str, u8> {
    map_res(take_while_m_n(2, 2, is_hex_digit), from_hex)(input)
}

fn hex_color(input: &str) -> IResult<&str, Color> {
    let (input, _) = tag("#")(input)?;
    let (input, (red, green, blue)) = tuple((hex_primary, hex_primary, hex_primary))(input)?;

    Ok((input, Color { red, green, blue }))
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd)]
struct EnvOut(Vec<(OsString, OsString)>);

fn get_env(input: &str) -> IResult<&str, EnvOut> {
    Ok((input, EnvOut(vec![])))
}

fn get_key_val(input: &str) -> IResult<&str, (&str, &str)> {
    let (input, (key, val)) = separated_pair(
        preceded(
            wrap_ws(alt((tag("export"), space0))),
            wrap_ws(alphanumeric1),
        ),
        alt((tag("="), tag(":"))),
        wrap_ws(maybe_quote(alphanumeric1)),
    )(input)?;

    Ok((input, (key, val)))
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn wrap_ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(space0, inner, space0)
}

fn quote<'a, F: 'a + Clone, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    alt((
        delimited(char('"'), inner.clone(), char('"')),
        delimited(char('\''), inner, char('\'')),
    ))
}

fn maybe_quote<'a, F: 'a + Clone, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    alt((quote(inner.clone()), inner))
}

fn till_equal(input: &str) -> IResult<&str, &str> {
    let (input, _) = space0(input)?;
    let (input, _) = alt((tag("export"), tag("")))(input)?;
    let (input, _) = space0(input)?;
    take_till1(|c| c == '=')(input)
}

// https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_get_key_val() {
        assert_eq!(get_key_val("a=b"), Ok(("", ("a", "b"))));
        assert_eq!(get_key_val("export a=b"), Ok(("", ("a", "b"))));
        assert_eq!(get_key_val("        export a=b"), Ok(("", ("a", "b"))));
        assert_eq!(get_key_val("a     =b"), Ok(("", ("a", "b"))));
        assert_eq!(get_key_val("a     =\"b\""), Ok(("", ("a", "b"))));
        assert_eq!(get_key_val("a     ='b'"), Ok(("", ("a", "b"))));
        assert!(get_key_val("a     =\"b'").is_err());
        assert!(get_key_val("a     ='b\"").is_err());
    }

    #[test]
    fn test_till_equal() {
        assert_eq!(till_equal("export XP="), Ok(("=", "XP")));
        assert_eq!(till_equal("lol="), Ok(("=", "lol")));
        assert_eq!(till_equal("   rofl ="), Ok(("=", "rofl ")));
    }

    #[test]
    fn test_empty() {
        assert_eq!(get_env(""), Ok(("", EnvOut(vec![]))));
    }

    #[test]
    fn test_hex() {
        assert_eq!(
            hex_color("#2F14DF"),
            Ok((
                "",
                Color {
                    red: 47,
                    green: 20,
                    blue: 223,
                }
            ))
        );
    }
}
