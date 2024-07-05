use std::fs;

use lure::Backtrackable;

fn main() {
    let filename = "src/example.lur";

    let file = fs::read_to_string(filename).expect("Failed to read file");

    let tokens = parse_tokens(&file).expect("Failed to parse tokens");

    for token in &tokens {
        // if let Token::Ident(token) = token {
        // }
        // println!("{:?}", token);
    }

    let source = parse_source_module(tokens).expect("Failed to parse source module");

    println!("{:#?}", source);
}

type ParseError = String;

fn parse_source_module(tokens: Vec<Token>) -> Result<SourceModule, ParseError> {
    let mut tokens = TokenIter::new(tokens);

    let module = parse_statement_list(&mut tokens)?;

    Ok(SourceModule {
        module,
        linked: vec![],
    })
}

struct TokenIter {
    tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
}

impl TokenIter {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }
    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token::Eof)
    }
    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap_or(&Token::Eof)
    }
}

fn parse_statement_list(tokens: &mut TokenIter) -> Result<StatementList, ParseError> {
    let mut statements = Vec::new();

    loop {
        if tokens.peek() == &Token::Keyword(Keyword::End) {
            println!("(end)");
            break;
        }

        let token = tokens.next();

        match token {
            Token::Eof => {
                println!("(EOF) this is probably bad");
                break;
            }
            Token::Keyword(Keyword::Func) => {
                let name = match tokens.next() {
                    Token::Ident(name) => name,
                    _ => return Err("Expected function name".to_string()),
                };
                if !matches!(tokens.next(), Token::Keyword(Keyword::ParenLeft)) {
                    return Err("Expected `(`".to_string());
                }
                let mut params = Params::new();
                loop {
                    let token = tokens.next();
                    match token {
                        Token::Keyword(Keyword::ParenRight) => break,
                        Token::Ident(name) => {
                            params.params.push(name);
                            match tokens.next() {
                                Token::Keyword(Keyword::Comma) => (),
                                Token::Keyword(Keyword::ParenRight) => break,
                                _ => return Err("Expected `,` or `)`".to_string()),
                            }
                        }
                        Token::Keyword(Keyword::Spread) => {
                            let name = match tokens.next() {
                                Token::Ident(name) => name,
                                _ => return Err("Expected parameter name".to_string()),
                            };
                            params.rest = Some(name);
                            match tokens.next() {
                                Token::Keyword(Keyword::ParenRight) => break,
                                _ => return Err("Expected `)`".to_string()),
                            }
                        }
                        _ => return Err("Expected parameter name or `)`".to_string()),
                    }
                }

                let body = parse_statement_list(tokens)?;

                statements.push(Statement::Func(Func { name, params, body }));
            }
            _ => (),
        }
    }

    Ok(statements)
}

#[derive(Debug, PartialEq)]
struct SourceModule {
    module: StatementList,
    linked: Vec<Module>,
}

#[derive(Debug, PartialEq)]
struct Module {
    name: ModuleName,
    statements: StatementList,
}

type ModuleName = String;

type StatementList = Vec<Statement>;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
struct Func {
    name: Ident,
    params: Params,
    body: StatementList,
}

type Ident = String;

#[derive(Debug, PartialEq)]
struct IdentPath {
    parents: Vec<Ident>,
    name: Ident,
}

#[derive(Debug, PartialEq)]
struct Params {
    params: Vec<Ident>,
    rest: Option<Ident>,
}

#[derive(Debug, PartialEq)]
struct Let {
    //TODO: Support destructuring
    name: Ident,
    value: Expr,
}

#[derive(Debug, PartialEq)]
struct Assign {
    name: Ident,
    value: Expr,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
enum UnaryOp {
    Not,
    Negative,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
struct If {
    if_: Box<ConditionalBranch>,
    elifs: Vec<ConditionalBranch>,
    else_: Option<StatementList>,
}

#[derive(Debug, PartialEq)]
struct ConditionalBranch {
    condition: Expr,
    body: StatementList,
}

#[derive(Debug, PartialEq)]
struct Match {
    pattern: Box<Expr>,
    branches: Vec<MatchBranch>,
}

#[derive(Debug, PartialEq)]
struct MatchBranch {
    //TODO: Restrict expressions and support destructuring
    pattern: Expr,
    //TODO: Add if guards
    body: StatementList,
}

#[derive(Debug, PartialEq)]
struct For {
    idents: Vec<Ident>,
    source: ForSource,
    body: StatementList,
}

#[derive(Debug, PartialEq)]
enum ForSource {
    Range(Expr, Expr),
    Iterable(Expr),
}

#[derive(Debug, PartialEq)]
struct While {
    branch: ConditionalBranch,
}

#[derive(Debug, PartialEq)]
struct Table {
    items: Vec<TableItem>,
}

#[derive(Debug, PartialEq)]
enum TableItem {
    Positional(usize, Box<Expr>),
    Named(Ident, Box<Expr>),
}

impl Params {
    pub fn new() -> Self {
        Self {
            params: Vec::new(),
            rest: None,
        }
    }
}

type LexError = String;

#[derive(Debug, PartialEq)]
enum Token {
    Keyword(Keyword),
    Ident(String),
    Literal(Literal),
    Eof,
}

#[derive(Debug, PartialEq)]
enum Literal {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

macro_rules! make_keyword {
    ( $( $token:literal => $name:ident ),* $(,)? ) => {
        #[derive(Debug, PartialEq)]
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

fn parse_tokens(file: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();

    let mut chars = Backtrackable::<4, _>::from(file.chars());

    while let Some(ch) = chars.next() {
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
        if let Some(keyword) = parse_lone_punct(ch) {
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

fn parse_lone_punct(ch: char) -> Option<Keyword> {
    Some(match ch {
        '(' => Keyword::ParenLeft,
        ')' => Keyword::ParenRight,
        '{' => Keyword::BraceLeft,
        '}' => Keyword::BraceRight,
        '[' => Keyword::BracketLeft,
        ']' => Keyword::BracketRight,
        ',' => Keyword::Comma,
        _ => return None,
    })
}
