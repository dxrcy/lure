use std::fs;

use lure::{lex_tokens, parse_source_module, ParseError};

fn main() {
    let filename = "src/example.lur";

    let file = fs::read_to_string(filename).expect("Failed to read file");

    let _ = match lex_and_parse(&file) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("\x1b[31mFailed to parse:\x1b[0m");
            eprintln!("\x1b[33m{}\x1b[0m", error);
            return;
        }
    };
}

fn lex_and_parse(file: &str) -> Result<(), ParseError> {
    let tokens = lex_tokens(file)?;

    let source = parse_source_module(tokens)?;

    println!("{:#?}", source);

    Ok(())
}
