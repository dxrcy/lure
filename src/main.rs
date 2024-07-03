use lure::Backtrackable;

fn main() {
    let file = include_str!("example.lur");

    let tokens = parse_tokens(file).expect("Failed to parse tokens");

    for token in tokens {
        println!("{}", token);
    }
}

// enum Token {
//     // Group(Delim, Vec<Token>),
//     // Keyword(Keyword),
//     Literal(Literal),
//     Ident(String),
//     Punct(String),
// }

// enum Delim {
//     Paren,
//     Brace,
//     Bracket,
// }

// enum Literal {
//     String(String),
//     Number(f32),
// }

type Token = String;

fn parse_tokens(file: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();

    let mut chars = file.chars().peekable();

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
        } else if is_delim(ch) {
            tokens.push(ch.to_string());
        } else if ch.is_digit(10) {
            let mut number = String::from(ch);
            while let Some(ch) = chars.next() {
                if !ch.is_digit(10) {
                    break;
                }
                number.push(ch);
                if chars.peek().is_some_and(|ch| *ch == '.') {
                    chars.next();
                    if chars.peek().is_some_and(|ch| ch.is_digit(10)) {
                        println!("OK");
                        number.push('.');
                    } else {
                        break;
                    }
                }
            }
            tokens.push(number);
        } else if ch == '"' {
            let mut string = String::new();
            while let Some(ch) = chars.next() {
                if ch == '"' {
                    break;
                }
                string.push(ch);
            }
            tokens.push(string);
        } else if ch == '\'' {
            unimplemented!("character literals")
        } else if ch.is_whitespace() {
            continue;
        } else {
            let ident_is_punct = is_punct(ch);
            let mut ident = String::from(ch);
            while let Some(ch) = chars.peek() {
                if ch.is_whitespace() || ident_is_punct != is_punct(*ch) {
                    break;
                }
                ident.push(*ch);
                chars.next();
            }
            tokens.push(ident);
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
    ch.is_alphanumeric() || ch == '_'
}

fn is_delim(ch: char) -> bool {
    match ch {
        '(' | ')' | '{' | '}' | '[' | ']' => true,
        _ => false,
    }
}
