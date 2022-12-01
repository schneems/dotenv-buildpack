// #![allow(dead_code)]
// #![allow(unused_imports)]
use std::ffi::OsString;
use std::str::FromStr;

use nom::bytes::complete::{escaped_transform, is_not};
use nom::character::complete::{digit1, line_ending, not_line_ending};
use nom::combinator::{eof, not, opt, value};
use nom::error::context;
use nom::multi::many_till;
use nom::sequence::{pair, terminated};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{space0, space1},
    error::ErrorKind,
    sequence::{delimited, preceded},
};
use nom::{AsChar, InputTakeAtPosition};

type VerboseResult<T, U> = nom::IResult<T, U, nom::error::VerboseError<T>>;

fn get_key_val(input: &str) -> VerboseResult<&str, (&str, String)> {
    let (input, (key, val)) = pair(getkey, wrap_ws(getval))(input)?;

    Ok((input, (key, val)))
}

pub fn get_env(input: &str) -> VerboseResult<&str, Vec<(OsString, OsString)>> {
    let (input, (vector, _)) = many_till(get_key_val, eof)(input)?;

    let vector = vector
        .into_iter()
        .map(|(k, v)| {
            (
                OsString::from_str(k).unwrap(),
                OsString::from_str(&v).unwrap(),
            )
        })
        .collect::<Vec<_>>();

    Ok((input, vector))
}

fn maybe_export(input: &str) -> VerboseResult<&str, &str> {
    alt((
        wrap_ws(preceded(tag("export"), space1)), // Handle optional `export PATH=foo` syntax
        space0,
    ))(input)
}

fn getkey(input: &str) -> VerboseResult<&str, &str> {
    let (input, _) = maybe_export(input)?;
    let (input, _) = context("key cannot start with a number", not(digit1))(input)?;
    let (input, output) = terminated(alphanum_plus1('_'), wrap_ws(tag("=")))(input)?;

    Ok((input, output))
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn wrap_ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> VerboseResult<&'a str, O>
where
    F: FnMut(&'a str) -> VerboseResult<&'a str, O>,
{
    delimited(space0, inner, space0)
}

/// alphanum_plus1('_')("snake_case.here") // => Ok((".here", "snake_case"))
/// alphanum_plus1('-')("kebab-case.here") // => Ok((".here", "kebab-case"))
fn alphanum_plus1<'a>(plus_char: char) -> impl FnMut(&'a str) -> VerboseResult<&'a str, &'a str> {
    move |input| {
        input.split_at_position1_complete(
            |item| {
                let c = item.as_char();
                c != plus_char && !c.is_alphanum()
            },
            ErrorKind::AlphaNumeric,
            // VerboseErrorKind::Nom(ErrorKind::AlphaNumeric), // ErrorKind::AlphaNumeric,
        )
    }
}

fn bash_comment(input: &str) -> VerboseResult<&str, &str> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("#")(input)?;
    let (input, output) = not_line_ending(input)?;

    Ok((input, output))
}

// Gets the value. Supports simple quoted string values or escaped values with no quotes
// does not currently support complex values of both like `hello"th ere"`
fn getval(input: &str) -> VerboseResult<&str, String> {
    let (input, _) = space0(input)?;
    let (input, output) = match input.chars().next() {
        Some('\'') => singlequote(input)?,
        Some('"') => doublequote(input)?,
        _ => get_unquoted_val(input)?,
    };

    let (input, _) = opt(bash_comment)(input)?;

    let (input, _) = context(
        "unexpected character after value, ensure intentional whitespace is escaped",
        alt((line_ending, eof)),
    )(input)?;
    Ok((input, output))
}

fn get_unquoted_val(input: &str) -> VerboseResult<&str, String> {
    let (input, output) = escaped_transform(
        is_not("\t\n\r \\;#"),
        '\\',
        alt((
            value(" ", tag(" ")),
            value("\t", tag("t")),
            value("\n", tag("n")),
            value("\r", tag("r")),
            value(";", tag(";")),
            value("#", tag("#")),
        )),
    )(input)?;

    Ok((input, output))
}

fn doublequote(input: &str) -> VerboseResult<&str, String> {
    let (input, _) = tag("\"")(input)?;

    // Allow escaped quotes within quotes
    //
    // This API is tricky, but makes sense when you're used to it. Here's what's
    // going on:
    //
    // The `escaped_transform` function is pulling characters until it finds as slash (`\`) or quote (`"`) i.e. `is_not("\"\\")
    // note that `is_not` will match on any character, not just characters in that order.
    //
    // When one of those characters is found it checks to see if the character is the control char (`\`).
    // If it is, the next character is passed to the parser. So if the string is `a\"` it will take
    // "a", then stop on the slash, and pass the quote (`"`) to the transformer. This will
    // replace the control char (shash) and the parsed character (quote) with the tag value `"`
    //
    // This continues until it hits an unescaped quote, this is the closing quote. It does not match the control char
    // so it does not call the transformer and instead returns.
    let (input, output) = escaped_transform(is_not("\"\\"), '\\', value("\"", tag("\"")))(input)?;
    let (input, _) = context("missing closing double quote `\"` in value", tag("\""))(input)?;

    Ok((input, output))
}

fn singlequote(input: &str) -> VerboseResult<&str, String> {
    let (input, _) = tag("'")(input)?;
    let (input, output) = escaped_transform(is_not("'\\"), '\\', value("'", tag("'")))(input)?;
    let (input, _) = context("missing closing single quote `'` in value", tag("'"))(input)?;

    Ok((input, output))
}

// https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md
#[cfg(test)]
mod tests {
    use nom::error::ErrorKind::{ManyTill, Not, Tag};
    use nom::error::VerboseError;
    use nom::error::VerboseErrorKind::{Context, Nom};

    use super::*;

    #[test]
    fn test_get_env_errors() {
        assert_eq!(
            get_env("FOO='no_closing_quote"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![
                    ("", Nom(Tag)),
                    ("", Context("missing closing single quote `'` in value")),
                    ("FOO='no_closing_quote", Nom(ManyTill))
                ]
            }))
        );

        assert_eq!(
            get_env("1OL=bad"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![
                    ("1OL=bad", Nom(Not)),
                    ("1OL=bad", Context("key cannot start with a number")),
                    ("1OL=bad", Nom(ManyTill))
                ]
            }))
        );
    }

    #[test]
    fn test_getval_bash_comment() {
        assert_eq!(
            getval(r#"hello # there"#),
            Ok(("", String::from(r#"hello"#)))
        );
    }

    #[test]
    fn test_get_unquoted_val_escapes_whitespace() {
        assert_eq!(
            get_unquoted_val(r#"hello"#),
            Ok(("", String::from(r#"hello"#)))
        );

        assert_eq!(
            get_unquoted_val(r#"he\ llo"#),
            Ok(("", String::from(r#"he llo"#)))
        );

        assert_eq!(
            get_unquoted_val(r#"he\tllo"#),
            Ok(("", String::from("he\tllo")))
        );

        assert_eq!(
            get_unquoted_val(r#"he\nllo"#),
            Ok(("", String::from("he\nllo")))
        );
    }

    #[test]
    fn test_get_quoted() {
        assert_eq!(getval("'he\\'llo'"), Ok(("", String::from("he'llo"))));
        assert_eq!(getval(r#""he\"llo""#), Ok(("", String::from(r#"he"llo"#))));

        assert_eq!(
            getval(r#""he\"l\"lo""#),
            Ok(("", String::from(r#"he"l"lo"#)))
        );

        assert_eq!(
            getval(r#""he\"l\"lo""#),
            Ok(("", String::from(r#"he"l"lo"#)))
        );

        assert_eq!(getval(r#""he\" lo""#), Ok(("", String::from(r#"he" lo"#))));

        assert!(getval(r#""he\"l\"lo" more here"#).is_err(),);
    }

    #[test]
    fn test_getval() {
        assert!(getval("hello ").is_err());
        assert!(getval("hel   lo\n").is_err());

        assert_eq!(getval("hello"), Ok(("", "hello".to_string())));
        assert_eq!(getval("hello\n"), Ok(("", "hello".to_string())));
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
                vec![
                    (
                        OsString::from_str("a").unwrap(),
                        OsString::from_str("b").unwrap()
                    ),
                    (
                        OsString::from_str("b").unwrap(),
                        OsString::from_str("c").unwrap()
                    )
                ]
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

        let out: VerboseResult<&str, &str> = tag("\"")("a     =\\\"b");
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
        assert_eq!(get_env(""), Ok(("", vec![])));
    }
}
