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
    let mut token = String::new();

    let mut is_punct = false;
    let mut is_number = false;

    let mut chars = file.chars();
    while let Some(ch) = chars.next() {
        if !ch.is_digit(10) {
            is_number = false;
        }
        if ch == '#' {
            while let Some(ch) = chars.next() {
                if ch == '\n' {
                    break;
                }
            }
            continue;
        }
        if is_delim(ch) {
            if !token.is_empty() {
                tokens.push(token);
                token = String::new();
                is_number = true;
            }
            tokens.push(ch.to_string());
            continue;
        }
        if ch.is_whitespace() {
            if !token.is_empty() {
                tokens.push(token);
                token = String::new();
            }
            continue;
        }
        if ch == '"' {
            if !token.is_empty() {
                tokens.push(token);
                token = String::new();
            }
            let mut string = String::new();
            'a: {
                while let Some(ch) = chars.next() {
                    if ch == '"' {
                        break 'a;
                    }
                    string.push(ch);
                }
                return Err(format!("Unclosed string"));
            }
            tokens.push(string);
            continue;
        }
        if ch.is_alphanumeric() || (is_number && ch == '.') {
            if is_punct {
                if !token.is_empty() {
                    tokens.push(token);
                    token = String::new();
                }
            }
            is_punct = false;
        } else {
            if !is_punct {
                if !token.is_empty() {
                    tokens.push(token);
                    token = String::new();
                    is_number = false;
                }
            }
            is_punct = true;
        }
        token.push(ch);
    }

    Ok(tokens)
}

fn is_delim(ch: char) -> bool {
    match ch {
        '(' | ')' | '{' | '}' | '[' | ']' => true,
        _ => false,
    }
}
