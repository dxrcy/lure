use std::fs;

fn main() {
    let filename = "src/example.lur";

    let file = fs::read_to_string(filename).expect("Failed to read file");

    if let Err(error) = lure::interpret_main(&file) {
        eprintln!("{}", error);
    };
}
