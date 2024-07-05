use std::fs;

use lure::Backtrackable;

fn main() {
    let filename = "src/example.lur";

    let file = fs::read_to_string(filename).expect("Failed to read file");

    let tokens = parse_tokens(&file).expect("Failed to parse tokens");

    for token in &tokens {
        // if let Token::Ident(token) = token {
        // }
        println!("{:?}", token);
    }
}

#[derive(Debug)]
enum Token {
    Keyword(Keyword),
    Ident(String),
    Literal(Literal),
}

#[derive(Debug)]
enum Literal {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

macro_rules! make_keyword {
    ( $( $token:literal => $name:ident ),* $(,)? ) => {
        #[derive(Debug)]
        enum Keyword {
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
    "=" => Assign,
    "," => Comma,
    "." => Dot,
    "==" => Equal,
    "/=" => NotEqual,
    ">" => GreaterThan,
    "<" => LessThan,
    ">=" => GreaterThanOrEqual,
    "<=" => LessThanOrEqual,
    "+" => Plus,
    "-" => Dash,
    "*" => Asterisk,
    "/" => Slash,
    "%" => Percent,
    "&" => Ampersand,
    ".." => Spread,
    "_" => Underscore,
}

fn parse_tokens(file: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();

    let mut chars = Backtrackable::<4, _>::from(file.chars());

    while let Some(ch) = chars.next() {
        if !is_valid_char(ch) {
            return Err(format!("Invalid character: 0x{:02x}", ch as u8));
        }
        if ch == '#' {
            while let Some(ch) = chars.next() {
                if ch == '\n' {
                    break;
                }
            }
        } else if let Some(keyword) = parse_delim(ch) {
            tokens.push(Token::Keyword(keyword));
        } else if ch.is_digit(10) {
            let mut number = String::from(ch);
            while let Some(ch) = chars.next() {
                if !ch.is_digit(10) {
                    chars.back();
                    break;
                }
                number.push(ch);
            }
            if let Some(ch) = chars.next() {
                if ch == '.' {
                    if let Some(ch) = chars.next() {
                        if ch.is_digit(10) {
                            number.push('.');
                            number.push(ch);
                        } else {
                            chars.back();
                            chars.back();
                        }
                    }
                } else {
                    chars.back();
                }
            }
            while let Some(ch) = chars.next() {
                if !ch.is_digit(10) {
                    chars.back();
                    break;
                }
                number.push(ch);
            }
            let number: f64 = number.parse().expect("Number should be valid");
            tokens.push(Token::Literal(Literal::Number(number)));
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
        } else if ch.is_whitespace() {
            continue;
        } else {
            let ident_is_punct = is_punct(ch);
            let mut ident = String::from(ch);
            while let Some(ch) = chars.next() {
                if ch.is_whitespace() || ident_is_punct != is_punct(ch) {
                    break;
                }
                ident.push(ch);
            }
            chars.back();
            let token = match Keyword::try_from(ident.as_str()) {
                Ok(keyword) => Token::Keyword(keyword),
                Err(_) => match ident.as_str() {
                    "true" => Token::Literal(Literal::Bool(true)),
                    "false" => Token::Literal(Literal::Bool(false)),
                    "nil" => Token::Literal(Literal::Nil),
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

fn parse_delim(ch: char) -> Option<Keyword> {
    Some(match ch {
        '(' => Keyword::ParenLeft,
        ')' => Keyword::ParenLeft,
        '{' => Keyword::BraceLeft,
        '}' => Keyword::BraceRight,
        '[' => Keyword::BracketLeft,
        ']' => Keyword::BracketRight,
        _ => return None,
    })
}
