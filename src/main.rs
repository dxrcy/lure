use std::fs;

use lure::{lex_tokens, parse_source_module};

fn main() {
    let filename = "src/example.lur";

    let file = fs::read_to_string(filename).expect("Failed to read file");

    let tokens = lex_tokens(&file).expect("Failed to lex tokens");

    // for token in &tokens {
    // if let Token::Ident(token) = token {
    // }
    // println!("{:?}", token);
    // }

    let source = match parse_source_module(tokens) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("\x1b[31mFailed to parse:\x1b[0m");
            eprintln!("\x1b[33m{}\x1b[0m", error);
            return;
        }
    };

    println!("{:#?}", source);
}
