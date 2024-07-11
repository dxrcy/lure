use std::fmt::{self, Display};

use crate::lex::Token;

#[derive(Debug)]
pub struct ParseError {
    pub line: usize,
    pub error: ParseErrorKind,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    InvalidChar {
        found: char,
    },
    InvalidPunct {
        found: String,
    },
    NumberSuffix {
        found: String,
    },
    UnclosedString {
        found: String,
    },
    InvalidEscapeChar {
        found: char,
    },
    CharLiteralNotSingleChar {
        found: String,
    },
    Unexpected {
        found: Token,
        expected: String,
        reason: Option<&'static str>,
    },
    MissingFinalExpression,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.error {
            ParseErrorKind::InvalidChar { found } => {
                write!(
                    f,
                    "Invalid character 0x{:02x} on line {}. Currently only ASCII characters are supported.",
                    *found as u8,
                    self.line + 1
                )
            }

            ParseErrorKind::InvalidPunct { found } => {
                write!(
                    f,
                    "Unknown punctuation token `{}` on line {}",
                    found,
                    self.line + 1,
                )?;

                const BITWISE: &str = "Bitwise operations are not supported";
                const NOT: &str = "Use `not` for a logical 'NOT' operation";
                const LOG_OR: &str = "Use `or` for a logical 'OR' operation";
                const LOG_AND: &str = "Use `and` for a logical 'AND' operation";
                const LOG_XOR: &str = "Logical `XOR` operations are not supported";
                const EXPONENT: &str = "Exponentiation operations are not supported";
                const INC_DEC: &str = "Increment and decrement operations are not supported";
                const COMMENT: &str = "Use `#` to declare a comment";
                const ARITH_ASSIGN: &str = "Arithmetic assignment operations are not supported";
                const CONCAT: &str = "Use `&` to concatenate values";
                const NEQ: &str = "Use `!=` for a not-equal comparison";
                const MEMBER: &str = "Use `.` to access a member of a value";
                const TABLE_KEY: &str = "Use `:` to declare a table value with a key";
                const BIT_SHIFT: &str = "Bit-shift operations are not supported";
                const TERNARY: &str = "Use an `if-else` block as a ternary expression";

                let reasons: &[&str] = match found.as_str() {
                    ";" => &["Semicolons are not supported"],
                    "!" => &[NOT],
                    "~" => &[NOT, BITWISE],
                    "&&" => &[LOG_AND],
                    "||" => &[LOG_OR],
                    "|" => &[LOG_OR, BITWISE],
                    "^" => &[LOG_XOR, BITWISE, EXPONENT],
                    "**" => &[EXPONENT],
                    "++" => &[CONCAT, INC_DEC],
                    "--" => &[COMMENT, INC_DEC],
                    "//" => &[COMMENT],
                    "~=" => &[NEQ],
                    "/=" => &[NEQ, ARITH_ASSIGN],
                    "+=" | "-=" | "*=" | "%=" => &[ARITH_ASSIGN],
                    "**=" => &[ARITH_ASSIGN, EXPONENT],
                    "&=" | "|=" | "^=" => &[BITWISE, ARITH_ASSIGN],
                    "<<=" | ">>=" => &[BIT_SHIFT, ARITH_ASSIGN],
                    ":" => &[MEMBER, TABLE_KEY],
                    "::" => &[MEMBER],
                    "?" => &[TERNARY],
                    "<<" | ">>" | "<<<" | ">>>" => &[BIT_SHIFT],
                    "`" => &[
                        "Use \"...\" for string literals",
                        "Use `std.format` for string interpolation",
                    ],
                    "$" => &["Identifiers must only contain characters [a-zA-Z0-9_@]"],
                    "??" => &["Fallbacks for `nil` values are not supported"],
                    _ => return Ok(()),
                };
                for reason in reasons {
                    write!(f, "\n{}.", reason)?;
                }
                Ok(())
            }

            ParseErrorKind::NumberSuffix { found } => {
                write!(
                    f,
                    "Number literal suffixes are not supported. Unexpected `{}` on line {}.",
                    found,
                    self.line + 1
                )
            }

            ParseErrorKind::UnclosedString { found } => {
                write!(
                    f,
                    "Unclosed string literal {:?} on line {}.",
                    found,
                    self.line + 1
                )
            }

            ParseErrorKind::InvalidEscapeChar { found } => {
                write!(
                    f,
                    "Invalid escape character `{}` on line {}.",
                    found,
                    self.line + 1
                )
            }

            ParseErrorKind::CharLiteralNotSingleChar { found } => {
                write!(
                    f,
                    "Character literal must be a single character. Found '{}' on line {}.",
                    debug_format_no_quotes(found),
                    self.line + 1
                )
            }

            ParseErrorKind::Unexpected {
                found,
                expected,
                reason,
            } => {
                write!(
                    f,
                    "Unexpected {} on line {}. Expected {}.",
                    found,
                    self.line + 1,
                    expected
                )?;
                if let Some(reason) = reason {
                    write!(f, "\n{}.", reason)?;
                }
                Ok(())
            }

            ParseErrorKind::MissingFinalExpression => {
                write!(
                    f,
                    "If expression branch does not end with expression, on line {}.",
                    self.line + 1
                )
            }
        }
    }
}

fn debug_format_no_quotes(string: &str) -> String {
    let debug = format!("{:?}", string);
    let mut chars = debug.chars();
    chars.next();
    chars.next_back();
    chars.as_str().to_string()
}
