use crate::PeekMore;
use std::{fmt, fmt::Display};

pub type LexError = String;

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Literal(Literal),
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

macro_rules! make_keyword {
    ( $( $token:literal => $name:ident ),* $(,)? ) => {
        #[derive(Debug, PartialEq)]
        pub enum Keyword {
            $(
                $name,
            )*
        }

        impl TryFrom<&str> for Keyword {
            type Error = ();

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                Ok(match value {
                    $(
                        $token => Self::$name,
                    )*
                    _ => return Err(()),
                })
            }
        }

        impl Display for Keyword {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Self::$name => write!(f, "{}", $token),
                    )*
                }
            }
        }
    };
}

make_keyword! {
    "module" => Module,
    "template" => Template,
    "func" => Func,
    "let" => Let,
    "end" => End,
    "if" => If,
    "elif" => Elif,
    "else" => Else,
    "then" => Then,
    "while" => While,
    "for" => For,
    "match" => Match,
    "case" => Case,
    "in" => In,
    "to" => To,
    "do" => Do,
    "return" => Return,
    "break" => Break,
    "continue" => Continue,
    "as" => As,
    "self" => Self_,
    "and" => And,
    "or" => Or,
    "not" => Not,
    "(" => ParenLeft,
    ")" => ParenRight,
    "{" => BraceLeft,
    "}" => BraceRight,
    "[" => BracketLeft,
    "]" => BracketRight,
    "=" => SingleEqual,
    "," => Comma,
    "." => Dot,
    "==" => DoubleEqual,
    "/=" => NotEqual,
    "<" => LessThan,
    ">" => GreaterThan,
    "<=" => LessThanOrEqual,
    ">=" => GreaterThanOrEqual,
    "+" => Plus,
    "-" => Dash,
    "*" => Asterisk,
    "/" => Slash,
    "%" => Percent,
    "&" => Ampersand,
    ".." => Spread,
    "_" => Underscore,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eof => write!(f, "<EOF>"),
            Self::Ident(ident) => write!(f, "ident `{}`", ident),
            Self::Keyword(keyword) => write!(f, "`{}`", keyword),
            Self::Literal(literal) => write!(f, "literal {}", literal),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(boolean) => write!(f, "{}", boolean),
            Self::Number(number) => write!(f, "{}", number),
            Self::String(string) => write!(f, "{:?}", string),
        }
    }
}

pub fn lex_tokens(file: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();

    let mut chars = PeekMore::from(file.chars());

    while let Some(ch) = chars.next() {
        if !is_valid_char(ch) {
            return Err(format!("Invalid character: 0x{:02x}", ch as u8));
        }
        if ch.is_whitespace() {
            continue;
        }
        if ch == '#' {
            while let Some(ch) = chars.next() {
                if ch == '\n' {
                    break;
                }
            }
            continue;
        }
        if let Some(keyword) = parse_lone_punct(ch) {
            tokens.push(Token::Keyword(keyword));
        } else if ch == '"' || ch == '\'' {
            let quote = ch;
            let is_char = quote == '\'';
            let mut string = String::new();
            while let Some(ch) = chars.next() {
                if ch == '"' {
                    break;
                }
                string.push(ch);
                if is_char {
                    break;
                }
            }
            tokens.push(Token::Literal(Literal::String(string)));
        } else if ch.is_digit(10) {
            let mut number = String::from(ch);
            while let Some(&ch) = chars.peek().filter(|ch| ch.is_digit(10)) {
                chars.next();
                number.push(ch);
            }
            chars.back();
            if chars.peek().is_some_and(|ch| *ch == '.') {
                if let Some(&ch) = chars.peek().filter(|ch| ch.is_digit(10)) {
                    chars.next();
                    chars.next();
                    number.push('.');
                    number.push(ch);
                } else {
                    chars.back();
                }
            } else {
                chars.back();
            }
            while let Some(&ch) = chars.peek().filter(|ch| ch.is_digit(10)) {
                chars.next();
                number.push(ch);
            }
            let number: f64 = number.parse().expect("Number should be valid");
            tokens.push(Token::Literal(Literal::Number(number)));
        } else {
            let ident_is_punct = is_punct(ch);
            let mut ident = String::from(ch);
            while let Some(&ch) = chars.peek() {
                if ch.is_whitespace() || ident_is_punct != is_punct(ch) {
                    break;
                }
                ident.push(ch);
                chars.next();
            }
            chars.back();
            let token = match Keyword::try_from(ident.as_str()) {
                Ok(keyword) => Token::Keyword(keyword),
                Err(_) => match ident.as_str() {
                    "true" => Token::Literal(Literal::Bool(true)),
                    "false" => Token::Literal(Literal::Bool(false)),
                    "nil" => Token::Literal(Literal::Nil),
                    _ if ident_is_punct => {
                        return Err(format!("Unexpected punctuation: `{}`", ident))
                    }
                    _ => Token::Ident(ident),
                },
            };
            tokens.push(token);
        }
    }

    Ok(tokens)
}

fn is_valid_char(ch: char) -> bool {
    match ch as u8 {
        // Non-control ascii characters + whitespace
        0x09 | 0x0a | 0x0d | 0x20..=0x7e => true,
        _ => false,
    }
}

fn is_punct(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '@' => false,
        _ => true,
    }
}

fn parse_lone_punct(ch: char) -> Option<Keyword> {
    Some(match ch {
        '(' => Keyword::ParenLeft,
        ')' => Keyword::ParenRight,
        '{' => Keyword::BraceLeft,
        '}' => Keyword::BraceRight,
        '[' => Keyword::BracketLeft,
        ']' => Keyword::BracketRight,
        ',' => Keyword::Comma,
        _ => return None,
    })
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn lex_works() {
        let file = r#"
            # comment
            func main()
                print("abc") # oiajd
                let x = 2.3
            end
        "#;
        let tokens = lex_tokens(file).expect("Failed to lex");
        for token in &tokens {
            println!("{:?}", token);
        }
        let mut tokens = tokens.into_iter();
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Func)));
        assert_eq!(tokens.next(), Some(Token::Ident("main".to_string())));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::ParenLeft)));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::ParenRight)));
        assert_eq!(tokens.next(), Some(Token::Ident("print".to_string())));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::ParenLeft)));
        assert_eq!(
            tokens.next(),
            Some(Token::Literal(Literal::String("abc".to_string())))
        );
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::ParenRight)));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Let)));
        assert_eq!(tokens.next(), Some(Token::Ident("x".to_string())));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::SingleEqual)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(2.3))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::End)));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn lex_numbers_works() {
        let file = r#"
            2           +
            2.3         +
            12 . 34     +
            12.34       +
            12 .. 34    +
            12..34      +
            12. .34     +
            1.2.3       +
            12.34.56    +
            12.34..56   +
            12..34.56
        "#;
        let tokens = lex_tokens(file).expect("Failed to lex");
        for token in &tokens {
            println!("{:?}", token);
        }
        let mut tokens = tokens.into_iter();

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(2.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(2.3))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Dot)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(34.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.34))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Spread)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(34.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Spread)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(34.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Dot)));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Dot)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(34.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(1.2))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Dot)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(3.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.34))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Dot)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(56.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.34))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Spread)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(56.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Plus)));

        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(12.0))));
        assert_eq!(tokens.next(), Some(Token::Keyword(Keyword::Spread)));
        assert_eq!(tokens.next(), Some(Token::Literal(Literal::Number(34.56))));

        assert_eq!(tokens.next(), None);
    }
}
