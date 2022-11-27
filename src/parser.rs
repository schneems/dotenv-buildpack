#![allow(dead_code)]
#![allow(unused_imports)]
use std::ffi::OsString;
use std::str::FromStr;

use nom::bytes::complete::{escaped_transform, is_not};
use nom::character::complete::{digit1, line_ending, not_line_ending};
use nom::combinator::{eof, not, recognize, value};
use nom::multi::many0;
use nom::sequence::{pair, terminated};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{space0, space1},
    error::{ErrorKind, ParseError},
    sequence::{delimited, preceded},
    IResult,
};
use nom::{AsChar, InputTakeAtPosition};

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd)]
struct EnvOut(Vec<(OsString, OsString)>);

fn get_key_val(input: &str) -> IResult<&str, (&str, String)> {
    let (input, (key, val)) = pair(getkey, wrap_ws(getval))(input)?;

    Ok((input, (key, val)))
}

fn get_env(input: &str) -> IResult<&str, EnvOut> {
    let (input, vector) = many0(get_key_val)(input)?;
    let vector = vector
        .into_iter()
        .map(|(k, v)| {
            (
                OsString::from_str(k).unwrap(),
                OsString::from_str(&v).unwrap(),
            )
        })
        .collect::<Vec<_>>();
    Ok((input, EnvOut(vector)))
}

fn maybe_export(input: &str) -> IResult<&str, &str> {
    alt((
        wrap_ws(preceded(tag("export"), space1)), // Handle optional `export PATH=foo` syntax
        space0,
    ))(input)
}

fn getkey(input: &str) -> IResult<&str, &str> {
    let (input, _) = maybe_export(input)?;
    let (input, _) = not(digit1)(input)?;
    let (input, output) = terminated(alphanum_plus1('_'), wrap_ws(tag("=")))(input)?;

    Ok((input, output))
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

// `FOO=bar` newline terminated or EOF
fn not_space(s: &str) -> IResult<&str, &str> {
    is_not(" \t\r\n")(s)
}

fn bash_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("#")(input)?;
    let (input, output) = not_line_ending(input)?;
    let (input, _) = alt((line_ending, eof))(input)?;

    // preceded(preceded(tag("#"), not_line_ending), alt((line_ending, eof)))(input)
    Ok((input, output))
}

// Gets the value when there are no quotes
fn getval(input: &str) -> IResult<&str, String> {
    let (input, _) = space0(input)?;
    let (input, output) = alt((get_quoted_val, get_unquoted_val))(input)?;
    let (input, _) = space0(input)?;
    Ok((input, output))
}

fn get_unquoted_val(input: &str) -> IResult<&str, String> {
    let (input, output) = not_space(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = alt((bash_comment, line_ending, eof))(input)?;

    Ok((input, output.to_string()))
}

fn get_quoted_val(input: &str) -> IResult<&str, String> {
    let (input, _) = tag("\"")(input)?;
    let (input, output) = escaped_transform(is_not("\\\""), '\\', value("\"", tag("\"")))(input)?;
    let (input, _) = tag("\"")(input)?;

    Ok((input, output))
}

// https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_quoted() {
        assert_eq!(
            get_quoted_val(r#""he\"llo""#),
            Ok(("", String::from(r#"he"llo"#)))
        );

        assert_eq!(
            get_quoted_val(r#""he\"l\"lo""#),
            Ok(("", String::from(r#"he"l"lo"#)))
        );

        assert_eq!(
            get_quoted_val(r#""he\"l\"lo""#),
            Ok(("", String::from(r#"he"l"lo"#)))
        );

        assert_eq!(
            get_quoted_val(r#""he\"l\"lo" more here"#),
            Ok((" more here", String::from(r#"he"l"lo"#)))
        );

        assert_eq!(
            get_quoted_val(r#""he\" lo""#),
            Ok(("", String::from(r#"he" lo"#)))
        );
    }

    #[test]
    fn test_getval() {
        assert_eq!(getval("hello "), Ok(("", "hello".to_string())));
        assert_eq!(getval("hello\n"), Ok(("", "hello".to_string())));
        assert!(getval("hel   lo\n").is_err());
        assert_eq!(getval("hello # comment"), Ok(("", "hello".to_string())));
        assert_eq!(getval("\"hello\""), Ok(("", "hello".to_string())));
    }

    #[test]
    fn test_env_key() {
        assert!(getkey("9NoNumbers=foo").is_err());
        assert!(getkey("No.Punctuation=bar").is_err());
        assert!(getkey("Underscore_Ok=baz").is_ok());
    }

    #[test]
    fn test_get_env() {
        assert_eq!(
            get_env("a=b\nb=c"),
            Ok((
                "",
                EnvOut(vec![
                    (
                        OsString::from_str("a").unwrap(),
                        OsString::from_str("b").unwrap()
                    ),
                    (
                        OsString::from_str("b").unwrap(),
                        OsString::from_str("c").unwrap()
                    )
                ])
            ))
        );
    }

    #[test]
    fn test_get_key_val() {
        assert_eq!(get_key_val("a=b"), Ok(("", ("a", "b".to_string()))));
        assert_eq!(get_key_val("a=b\nb=c"), Ok(("b=c", ("a", "b".to_string()))));
        assert_eq!(get_key_val("export a=b"), Ok(("", ("a", "b".to_string()))));
        assert_eq!(
            get_key_val("        export a=b"),
            Ok(("", ("a", "b".to_string())))
        );
        assert_eq!(get_key_val("a     =b"), Ok(("", ("a", "b".to_string()))));

        let out: IResult<&str, &str> = tag("\"")("a     =\\\"b");
        assert!(out.is_err());

        assert_eq!(
            get_key_val("a     =\"b\""),
            Ok(("", ("a", "b".to_string())))
        );
        // assert_eq!(
        //     get_key_val("a     =\\\"b"),
        //     Ok(("", ("a", "b".to_string())))
        // );

        // assert!(get_key_val("a     =\\\"b'").is_err());
        // assert!(get_key_val("a     ='b\"").is_err());
        assert_eq!(
            get_key_val("MY_PATH=legendary"),
            Ok(("", ("MY_PATH", "legendary".to_string())))
        );
    }

    #[test]
    fn test_empty() {
        assert_eq!(get_env(""), Ok(("", EnvOut(vec![]))));
    }
}
