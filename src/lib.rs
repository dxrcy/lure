#![allow(clippy::needless_return)]

mod lex;
mod parse;
mod peek_more;
mod token_iter;

use core::fmt;
use std::fmt::Display;

pub use lex::lex_tokens;
use lex::Token;
pub use parse::parse_source_module;
pub use peek_more::PeekMore;
pub use token_iter::TokenIter;

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
                let reason = match found.as_str() {
                    "//" | "--" => "Use `#` to declare a comment",
                    "~=" | "/=" => "Use `!=` for a not-equal comparison",
                    _ => return Ok(()),
                };
                write!(f, "\n{}.", reason)?;
                Ok(())
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
