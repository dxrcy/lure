#![allow(clippy::needless_return)]

mod error;
mod lex;
mod parse;
mod peek_more;
mod token_iter;

pub use error::{ParseError, ParseErrorKind};
pub use lex::lex_tokens;
pub use parse::parse_source_module;
pub use peek_more::PeekMore;
pub use token_iter::TokenIter;
