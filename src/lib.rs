#![allow(clippy::needless_return)]

// #[macro_use]
// mod debug;

mod error;
// mod interpret;
mod lex;
mod parse;
mod peek_more;
mod token_iter;

pub use error::{ParseError, ParseErrorKind};
// pub use interpret::interpret_main;

pub use lex::lex_tokens;
pub use parse::parse_file_module;

use peek_more::PeekMore;
use token_iter::TokenIter;
