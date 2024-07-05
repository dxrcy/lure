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

    let source = parse_source_module(tokens).expect("Failed to parse source module");

    println!("{:?}", source);
}

fn parse_source_module(tokens: Vec<Token>) -> Result<SourceModule, String> {
    let mut tokens = tokens.into_iter();

    let module = parse_module_statements(&mut tokens)?;

    Ok(SourceModule {
        module,
        linked: vec![],
    })
}

fn parse_module_statements(
    tokens: &mut impl Iterator<Item = Token>,
) -> Result<StatementList, String> {
    todo!();
}

#[derive(Debug)]
struct SourceModule {
    module: StatementList,
    linked: Vec<Module>,
}

#[derive(Debug)]
struct Module {
    name: ModuleName,
    statements: StatementList,
}

type ModuleName = String;

type StatementList = Vec<Statement>;

#[derive(Debug)]
enum Statement {
    Expr(Expr),
    Func(Func),
    Let(Let),
    Assign(Assign),
    If(If),
    Match(Match),
    While(While),
    For(For),
    Module(Module),
    //TODO: Template
}

#[derive(Debug)]
struct Func {
    name: Ident,
    params: Params,
}

type Ident = String;

#[derive(Debug)]
struct IdentPath {
    parents: Vec<Ident>,
    name: Ident,
}

#[derive(Debug)]
struct Params {
    params: Vec<Ident>,
    rest: Option<Ident>,
}

#[derive(Debug)]
struct Let {
    //TODO: Support destructuring
    name: Ident,
    value: Expr,
}

#[derive(Debug)]
struct Assign {
    name: Ident,
    value: Expr,
}

#[derive(Debug)]
enum Expr {
    Literal(Literal),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    IdentPath(IdentPath),
    Call(IdentPath, Params),
    If(If),
    Match(Match),
    Table(Table),
}

#[derive(Debug)]
enum UnaryOp {
    Not,
    Negative,
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Multiply,
    Subtract,
    Divide,
    Modulo,
    Concat,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
}

#[derive(Debug)]
struct If {
    if_: Box<ConditionalBranch>,
    elifs: Vec<ConditionalBranch>,
    else_: Option<StatementList>,
}

#[derive(Debug)]
struct ConditionalBranch {
    condition: Expr,
    body: StatementList,
}

#[derive(Debug)]
struct Match {
    pattern: Box<Expr>,
    branches: Vec<MatchBranch>,
}

#[derive(Debug)]
struct MatchBranch {
    //TODO: Restrict expressions and support destructuring
    pattern: Expr,
    //TODO: Add if guards
    body: StatementList,
}

#[derive(Debug)]
struct For {
    idents: Vec<Ident>,
    source: ForSource,
    body: StatementList,
}

#[derive(Debug)]
enum ForSource {
    Range(Expr, Expr),
    Iterable(Expr),
}

#[derive(Debug)]
struct While {
    branch: ConditionalBranch,
}

#[derive(Debug)]
struct Table {
    items: Vec<TableItem>,
}

#[derive(Debug)]
enum TableItem {
    Positional(usize, Box<Expr>),
    Named(Ident, Box<Expr>),
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
    "=" => SingleEqual,
    "," => Comma,
    "." => Dot,
    "==" => DoubleEqual,
    "/=" => NotEqual,
    "<" => LessThan,
    ">" => GreaterThan,
    "<=" => LessThanOrEqual,
    ">=" => GreaterThanOrEqual,
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

    while let Some(mut ch) = chars.next() {
        if !is_valid_char(ch) {
            return Err(format!("Invalid character: 0x{:02x}", ch as u8));
        }
        if ch.is_whitespace() {
            continue;
        }
        if ch == '#' {
            while let Some(ch) = chars.next() {
                if ch == '\n' {
                    break;
                }
            }
            continue;
        }
        if let Some(keyword) = parse_delim(ch) {
            tokens.push(Token::Keyword(keyword));
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
        ')' => Keyword::ParenRight,
        '{' => Keyword::BraceLeft,
        '}' => Keyword::BraceRight,
        '[' => Keyword::BracketLeft,
        ']' => Keyword::BracketRight,
        _ => return None,
    })
}
