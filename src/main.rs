use lure::Backtrackable;

fn main() {
    let file = include_str!("example.lur");

    let tokens = parse_tokens(file).expect("Failed to parse tokens");

    for token in tokens {
        println!("{:?}", token);
    }
}

#[derive(Debug)]
enum TokenTree {
    Group(Delim, Vec<Token>),
    Token(Token),
}

#[derive(Debug)]
enum TokenPartial {
    Delim { delim: Delim, is_left: bool },
    Token(Token),
}

#[derive(Debug)]
enum Token {
    Literal(Literal),
    Ident(String),
    Punct(String),
    Keyword(Keyword),
}

#[derive(Debug)]
enum Literal {
    String(String),
    Number(f32),
}

#[derive(Debug)]
enum Keyword {
    Func,
    Let,
    End,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "func" => Self::Func,
            "let" => Self::Let,
            "end" => Self::End,
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
enum Delim {
    Paren,
    Brace,
    Bracket,
}

fn parse_tokens(file: &str) -> Result<Vec<TokenPartial>, String> {
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
        } else if let Some((delim, is_left)) = parse_delim(ch) {
            tokens.push(TokenPartial::Delim { delim, is_left });
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
            let number: f32 = number.parse().expect("Number should be valid");
            tokens.push(TokenPartial::Token(Token::Literal(Literal::Number(number))));
        } else if ch == '"' {
            let mut string = String::new();
            while let Some(ch) = chars.next() {
                if ch == '"' {
                    break;
                }
                string.push(ch);
            }
            tokens.push(TokenPartial::Token(Token::Literal(Literal::String(string))));
        } else if ch == '\'' {
            unimplemented!("character literals")
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
            let token = if ident_is_punct {
                Token::Punct(ident)
            } else {
                match Keyword::try_from(ident.as_str()) {
                    Ok(keyword) => Token::Keyword(keyword),
                    Err(_) => Token::Ident(ident),
                }
            };
            tokens.push(TokenPartial::Token(token));
        }
    }

    Ok(tokens)
}

fn parse_delim(ch: char) -> Option<(Delim, bool)> {
    match ch {
        '(' => Some((Delim::Paren, true)),
        ')' => Some((Delim::Paren, false)),
        '{' => Some((Delim::Brace, true)),
        '}' => Some((Delim::Brace, false)),
        '[' => Some((Delim::Bracket, true)),
        ']' => Some((Delim::Bracket, false)),
        _ => None,
    }
}

fn is_valid_char(ch: char) -> bool {
    match ch as u8 {
        0x09 | 0x0a | 0x0d | 0x20..=0x7e => true,
        _ => false,
    }
}

fn is_punct(ch: char) -> bool {
    !ch.is_alphanumeric() && ch != '_'
}

fn is_delim(ch: char) -> bool {
    match ch {
        '(' | ')' | '{' | '}' | '[' | ']' => true,
        _ => false,
    }
}
