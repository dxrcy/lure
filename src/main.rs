use std::fs;

fn main() {
    let filename = "src/example.lur";
    // let filename = "example/main.lur";

    let file = fs::read_to_string(filename).expect("Failed to read file");

    let tokens = lure::lex_tokens(&file).expect("Failed to lex");
    let body = lure::parse_module_body(tokens).expect("Failed to parse");
    println!("Successfully parsed.");

    println!("{:#?}", body);

    // if let Err(error) = lure::interpret_main(&file) {
    //     eprintln!("{}", error);
    // };
}
