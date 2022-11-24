#![allow(dead_code)]
use std::ffi::OsString;

use nom::sequence::pair;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric1, char, space0, space1},
    error::{ErrorKind, ParseError},
    sequence::{delimited, preceded},
    IResult,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd)]
struct EnvOut(Vec<(OsString, OsString)>);

fn get_env(input: &str) -> IResult<&str, EnvOut> {
    Ok((input, EnvOut(vec![])))
}

fn maybe_export(input: &str) -> IResult<&str, &str, EnvVarParseError<&str>> {
    alt((
        wrap_ws(preceded(tag("export"), space1)), // Handle optional `export PATH=foo` syntax
        space0,
    ))(input)
}

fn getkey(input: &str) -> IResult<&str, &str, EnvVarParseError<&str>> {
    delimited(
        maybe_export,
        preceded(does_not_start_with_number, alphanum_plus1('_')),
        wrap_ws(alt((tag("="), tag(":")))),
    )(input)
}

fn get_key_val(input: &str) -> IResult<&str, (&str, &str), EnvVarParseError<&str>> {
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

fn getval(input: &str) -> IResult<&str, &str, EnvVarParseError<&str>> {
    maybe_quote(alphanumeric1)(input)
}

fn quote<'a, F: 'a + Clone, O, E: ParseError<&'a str>>(
    input: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    alt((
        delimited(char('"'), input.clone(), char('"')),
        delimited(char('\''), input, char('\'')),
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

use nom::Err::Error;
use nom::{AsChar, InputTakeAtPosition};

// https://github.com/Geal/nom/blob/main/examples/custom_error.rs
// https://users.rust-lang.org/t/nom-how-to-raise-an-error-convert-other-errors-to-nom-errors/24701
#[derive(Debug, PartialEq)]
pub enum EnvVarParseError<I> {
    CannotStartWithNumber,
    InvalidCharacter(String),
    Nom(I, ErrorKind),
}

impl<I> ParseError<I> for EnvVarParseError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        EnvVarParseError::Nom(input, kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

fn does_not_start_with_number(input: &str) -> IResult<&str, &str, EnvVarParseError<&str>> {
    match input.chars().next() {
        None => Err(Error(EnvVarParseError::CannotStartWithNumber)),
        Some(c) => {
            if c.is_ascii_digit() {
                Err(Error(EnvVarParseError::CannotStartWithNumber))
            } else {
                Ok((input, ""))
            }
        }
    }
}

/// alphanum_plus1('_')("snake_case.here") // => Ok((".here", "snake_case"))
/// alphanum_plus1('-')("kebab-case.here") // => Ok((".here", "kebab-case"))
fn alphanum_plus1<'a, E: ParseError<&'a str>>(
    plus_char: char,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E> {
    move |input| {
        input.split_at_position1_complete(
            |item| {
                let c = item.as_char();
                c != plus_char && !c.is_alphanum()
            },
            ErrorKind::AlphaNumeric,
        )
    }
}

// https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md

#[cfg(test)]
mod tests {
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
    fn test_empty() {
        assert_eq!(get_env(""), Ok(("", EnvOut(vec![]))));
    }
}
