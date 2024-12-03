use crate::{ParseError, ParseErrorKind, PeekMore};
use std::{fmt, fmt::Display};

//TODO: Change name
#[derive(Debug)]
pub struct Ref<T> {
    pub token: T,
    pub line: usize,
}

pub type TokenRef = Ref<Token>;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Literal(Literal),
    Eof,
}

pub type Number = f64;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Nil,
    Bool(bool),
    Char(char),
    Number(Number),
    String(String),
}

macro_rules! make_keyword {
    (
        $( $keyword:literal => $keyword_name:ident ),* $(,)?
        ::
        $( $punct:literal   => $punct_name:ident ),* $(,)?
    ) => {
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub enum Keyword {
            $( $keyword_name, )*
            $( $punct_name, )*
        }

        impl TryFrom<&str> for Keyword {
            type Error = ();

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                Ok(match value {
                    $( $keyword => Self::$keyword_name, )*
                    $( $punct   => Self::$punct_name, )*
                    _ => return Err(()),
                })
            }
        }

        impl Display for Keyword {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $( Self::$keyword_name => write!(f, "keyword `{}`", $keyword), )*
                    $( Self::$punct_name => write!(f, "punctuation `{}`", $punct), )*
                }
            }
        }
    };
}

// Does not include `nil`, `true`, `false`
make_keyword! {
    "module" => Module,
    "template" => Template,
    "func" => Func,
    "end" => End,
    "if" => If,
    "elif" => Elif,
    "else" => Else,
    "then" => Then,
    "while" => While,
    "for" => For,
    "case" => Case,
    "in" => In,
    "to" => To,
    "do" => Do,
    "return" => Return,
    "break" => Break,
    "continue" => Continue,
    "from" => From,
    "self" => Self_,
    "and" => And,
    "or" => Or,
    "not" => Not,
    :: // Punctuation
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
    "!=" => NotEqual,
    "<" => LessThan,
    ">" => GreaterThan,
    "<=" => LessEqual,
    ">=" => GreaterEqual,
    "+" => Plus,
    "-" => Dash,
    "*" => Asterisk,
    "/" => Slash,
    "%" => Percent,
    ":" => Colon,
    ".." => Spread,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eof => write!(f, "<EOF>"),
            Self::Ident(ident) => write!(f, "ident `{}`", ident),
            Self::Keyword(keyword) => write!(f, "{}", keyword),
            Self::Literal(literal) => write!(f, "literal {}", literal),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(bool) => write!(f, "{}", bool),
            Self::Number(number) => write!(f, "{}", number),
            Self::Char(char) => write!(f, "{}", char),
            Self::String(string) => write!(f, "{:?}", string),
        }
    }
}

impl From<Keyword> for Token {
    fn from(value: Keyword) -> Self {
        Self::Keyword(value)
    }
}

pub fn lex_tokens(file: &str) -> Result<Vec<TokenRef>, ParseError> {
    let mut tokens: Vec<TokenRef> = Vec::new();

    let mut chars = PeekMore::from(file.chars());
    let mut line = 0;

    // Newlines can only occur as whitespace, or inside string literals
    // So only need to be checked in these cases

    while let Some(ch) = chars.next() {
        if ch == '\n' {
            line += 1;
            continue;
        }
        if !is_valid_char(ch) {
            return Err(ParseError {
                line,
                error: ParseErrorKind::InvalidChar { found: ch },
            });
        }
        if ch.is_whitespace() {
            continue;
        }
        if ch == '#' {
            if chars.peek() == Some('[') && chars.peek() == Some('[') {
                let mut was_first_bracket = false;
                while let Some(ch) = chars.by_ref().next() {
                    if ch == '\n' {
                        line += 1;
                    }
                    if ch == ']' {
                        if was_first_bracket {
                            break;
                        }
                        was_first_bracket = true;
                    }
                }
            } else {
                chars.back();
                chars.back();
                for ch in chars.by_ref() {
                    if ch == '\n' {
                        line += 1;
                        break;
                    }
                }
            }
            continue;
        }
        if let Some(keyword) = parse_lone_punct(ch) {
            tokens.push(TokenRef {
                token: Token::Keyword(keyword),
                line,
            });
        } else if ch == '"' || ch == '\'' {
            let quote_ch = ch;
            let is_char = quote_ch == '\'';
            let mut string = String::new();
            loop {
                let Some(ch) = chars.next() else {
                    return Err(ParseError {
                        line,
                        error: ParseErrorKind::UnclosedString { found: string },
                    });
                };
                if ch == '\n' {
                    line += 1;
                }
                if ch == quote_ch {
                    break;
                }
                // Escaped char
                if ch == '\\' {
                    let Some(ch) = chars.next() else {
                        return Err(ParseError {
                            line,
                            error: ParseErrorKind::UnclosedString { found: string },
                        });
                    };
                    match ch {
                        '\\' | '"' | '\'' => string.push(ch),
                        'r' => string.push('\r'),
                        'n' => string.push('\n'),
                        'x' => {
                            let mut hex = String::new();
                            for _ in 0..2 {
                                let Some(ch) = chars.next() else {
                                    return Err(ParseError {
                                        line,
                                        error: ParseErrorKind::InvalidEscapeSequence {
                                            found: format!("x{}", hex),
                                        },
                                    });
                                };
                                hex.push(ch);
                            }
                            let hex: u8 = u8::from_str_radix(&hex, 16).unwrap();
                            string.push(hex as char);
                        }
                        _ => {
                            return Err(ParseError {
                                line,
                                error: ParseErrorKind::InvalidEscapeSequence {
                                    found: ch.to_string(),
                                },
                            })
                        }
                    }
                } else {
                    string.push(ch);
                };
            }
            let literal = if is_char {
                if string.len() != 1 {
                    return Err(ParseError {
                        line,
                        error: ParseErrorKind::CharLiteralNotSingleChar { found: string },
                    });
                }
                Literal::Char(string.chars().next().expect("Character should exist"))
            } else {
                Literal::String(string)
            };
            tokens.push(TokenRef {
                token: Token::Literal(literal),
                line,
            });
        } else if ch.is_ascii_digit() {
            let mut number = String::from(ch);
            while let Some(ch) = chars.peek().filter(|ch| ch.is_ascii_digit()) {
                chars.next();
                number.push(ch);
            }
            chars.back();
            if chars.peek().is_some_and(|ch| ch == '.') {
                if let Some(ch) = chars.peek().filter(|ch| ch.is_ascii_digit()) {
                    chars.next();
                    chars.next();
                    number.push('.');
                    number.push(ch);
                } else {
                    chars.back();
                    chars.back();
                }
            } else {
                chars.back();
            }
            while let Some(ch) = chars.peek().filter(|ch| ch.is_ascii_digit()) {
                chars.next();
                number.push(ch);
            }
            chars.back();
            if chars
                .peek()
                .is_some_and(|ch| !ch.is_ascii_digit() && !is_punct(ch))
            {
                let mut found = number;
                while let Some(ch) = chars.next().filter(|ch| !is_punct(*ch)) {
                    found.push(ch);
                }
                return Err(ParseError {
                    line,
                    error: ParseErrorKind::NumberSuffix { found },
                });
            }
            // If this fails, that means this lexing function is broken, not the
            // user's code
            let number: Number = number.parse().expect("Number should be valid");
            tokens.push(TokenRef {
                token: Token::Literal(Literal::Number(number)),
                line,
            });
        } else {
            let ident_is_punct = is_punct(ch);
            let mut ident = String::from(ch);
            let mut following_lone_punct = None;
            while let Some(ch) = chars.peek() {
                if ch.is_whitespace() || ident_is_punct != is_punct(ch) {
                    break;
                }
                if ident_is_punct {
                    if let Some(keyword) = parse_lone_punct(ch) {
                        following_lone_punct = Some(keyword);
                        break;
                    }
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
                        return Err(ParseError {
                            line,
                            error: ParseErrorKind::InvalidPunct { found: ident },
                        });
                    }
                    _ => Token::Ident(ident),
                },
            };
            tokens.push(TokenRef { token, line });
            if let Some(keyword) = following_lone_punct {
                tokens.push(TokenRef {
                    token: Token::Keyword(keyword),
                    line,
                });
            }
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
    !matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '@')
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
        // Note the first newline is escaped!
        let file = "\
            # comment
            func main()
                print(\"abc\") #oiajd
                let x = 2.3
            end
        ";
        let tokens = lex_tokens(file).expect("Failed to lex");
        for token in &tokens {
            println!("{:?}", token);
        }
        let mut tokens = tokens.into_iter();

        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::Func));
        assert_eq!(token_ref.line, 1);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Ident("main".to_string()));
        assert_eq!(token_ref.line, 1);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::ParenLeft));
        assert_eq!(token_ref.line, 1);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::ParenRight));
        assert_eq!(token_ref.line, 1);

        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Ident("print".to_string()));
        assert_eq!(token_ref.line, 2);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::ParenLeft));
        assert_eq!(token_ref.line, 2);
        let token_ref = tokens.next().unwrap();
        assert_eq!(
            token_ref.token,
            Token::Literal(Literal::String("abc".to_string()))
        );
        assert_eq!(token_ref.line, 2);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::ParenRight));
        assert_eq!(token_ref.line, 2);

        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::Let));
        assert_eq!(token_ref.line, 3);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Ident("x".to_string()));
        assert_eq!(token_ref.line, 3);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::SingleEqual));
        assert_eq!(token_ref.line, 3);
        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Literal(Literal::Number(2.3)));
        assert_eq!(token_ref.line, 3);

        let token_ref = tokens.next().unwrap();
        assert_eq!(token_ref.token, Token::Keyword(Keyword::End));
        assert_eq!(token_ref.line, 4);

        assert!(tokens.next().is_none());
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

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(2.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(2.3))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Dot));
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(34.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.34))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.0))
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Keyword(Keyword::Spread)
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(34.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.0))
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Keyword(Keyword::Spread)
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(34.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Dot));
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Dot));
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(34.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(1.2))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Dot));
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(3.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.34))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Dot));
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(56.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.34))
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Keyword(Keyword::Spread)
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(56.0))
        );
        assert_eq!(tokens.next().unwrap().token, Token::Keyword(Keyword::Plus));

        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(12.0))
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Keyword(Keyword::Spread)
        );
        assert_eq!(
            tokens.next().unwrap().token,
            Token::Literal(Literal::Number(34.56))
        );

        assert!(tokens.next().is_none());
    }
}
