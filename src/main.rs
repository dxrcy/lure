use std::fs;

use lure::{lex_tokens, parse_source_module};

fn main() {
    let filename = "src/example.lur";

    let file = fs::read_to_string(filename).expect("Failed to read file");

    let tokens = lex_tokens(&file).expect("Failed to lex tokens");

    for token in &tokens {
        // if let Token::Ident(token) = token {
        // }
        // println!("{:?}", token);
    }

    let source = parse_source_module(tokens).expect("Failed to parse source module");

    println!("{:#?}", source);
}
