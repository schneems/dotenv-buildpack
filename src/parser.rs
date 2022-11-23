#![allow(dead_code)]
#![allow(unused_imports)]

use std::ffi::OsString;

use nom::complete::take;
use nom::error::ErrorKind;

use nom::bytes::complete::is_not;
use nom::character::complete::{char, digit1, space1};
use nom::character::is_digit;
use nom::sequence::pair;
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

fn getval(input: &str) -> IResult<&str, &str, EnvVarKeyError<&str>> {
    maybe_quote(alphanumeric1)(input)
}

fn maybe_export(input: &str) -> IResult<&str, &str, EnvVarKeyError<&str>> {
    alt((
        wrap_ws(preceded(tag("export"), space1)), // Handle optional `export PATH=foo` syntax
        space0,
    ))(input)
}

fn getkey(input: &str) -> IResult<&str, &str, EnvVarKeyError<&str>> {
    delimited(
        maybe_export,
        preceded(does_not_start_with_number, alphnumeric_and_underscore1),
        wrap_ws(alt((tag("="), tag(":")))),
    )(input)
}

fn get_key_val(input: &str) -> IResult<&str, (&str, &str), EnvVarKeyError<&str>> {
    let (input, (key, val)) = pair(getkey, wrap_ws(getval))(input)?;

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

use nom::Err::Error;
use nom::{AsChar, InputTakeAtPosition};

// https://github.com/Geal/nom/blob/main/examples/custom_error.rs
// https://users.rust-lang.org/t/nom-how-to-raise-an-error-convert-other-errors-to-nom-errors/24701
#[derive(Debug, PartialEq)]
pub enum EnvVarKeyError<I> {
    CannotStartWithNumber,
    InvalidCharacter(String),
    Nom(I, ErrorKind),
}

impl<I> ParseError<I> for EnvVarKeyError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        EnvVarKeyError::Nom(input, kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

fn does_not_start_with_number(input: &str) -> IResult<&str, &str, EnvVarKeyError<&str>> {
    match input.chars().next() {
        None => Err(Error(EnvVarKeyError::CannotStartWithNumber)),
        Some(c) => {
            if c.is_ascii_digit() {
                Err(Error(EnvVarKeyError::CannotStartWithNumber))
            } else {
                Ok((input, ""))
            }
        }
    }
}

fn alphnumeric_and_underscore1(input: &str) -> IResult<&str, &str, EnvVarKeyError<&str>> {
    input.split_at_position1_complete(
        |item| {
            let c = item.as_char();
            c != '_' && !c.is_alphanum()
        },
        ErrorKind::AlphaNumeric,
    )
}

// https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_env_key() {
        assert!(getkey("9NoNumbers=foo").is_err());
        assert!(getkey("No.Punctuation=bar").is_err());
        assert!(getkey("Underscore_Ok=baz").is_ok());
    }

    #[test]
    fn test_does_not_start_with_number() {
        assert!(does_not_start_with_number("9NoNumbers").is_err());
        assert_eq!(
            does_not_start_with_number("NoNumbers"),
            Ok(("NoNumbers", ""))
        );
        assert_eq!(
            does_not_start_with_number("NoNumbers9"),
            Ok(("NoNumbers9", ""))
        );
    }

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
        assert_eq!(
            get_key_val("MY_PATH=legendary"),
            Ok(("", ("MY_PATH", "legendary")))
        );
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
