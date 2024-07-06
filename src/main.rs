use core::fmt;
use std::{fmt::Display, fs};

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

fn unexpected(found: impl Display, expected: impl Display) -> String {
    format!("Unexpected {}. Expected {}", found, expected)
}

fn parse_statement_list(tokens: &mut TokenIter) -> Result<StatementList, ParseError> {
    let mut statements = Vec::new();

    loop {
        if tokens.peek() == &Token::Keyword(Keyword::End) {
            println!("(end)");
            break;
        }

        let token = tokens.peek();

        match token {
            Token::Eof => {
                println!("(EOF) this is probably bad");
                break;
            }

            Token::Keyword(Keyword::Let) => {
                tokens.next();
                //TODO: Support destructuring
                let name = match tokens.next() {
                    Token::Ident(name) => name,
                    token => return Err(unexpected(token, "variable name")),
                };
                match tokens.next() {
                    Token::Keyword(Keyword::SingleEqual) => (),
                    token => return Err(unexpected(token, "`=`")),
                }
                let value = parse_expr(tokens)?;
                statements.push(Statement::Let(Let { name, value }));
            }

            Token::Keyword(Keyword::Func) => {
                tokens.next();
                let name = match tokens.next() {
                    Token::Ident(name) => name,
                    token => return Err(unexpected(token, "function name")),
                };
                match tokens.next() {
                    Token::Keyword(Keyword::ParenLeft) => (),
                    token => return Err(unexpected(token, "`(`")),
                }
                let mut params = DeclareParams::default();
                loop {
                    let token = tokens.next();
                    match token {
                        Token::Keyword(Keyword::ParenRight) => break,
                        Token::Keyword(Keyword::Spread) => {
                            let name = match tokens.next() {
                                Token::Ident(name) => name,
                                token => return Err(unexpected(token, "parameter name")),
                            };
                            params.rest = Some(name);
                            match tokens.next() {
                                Token::Keyword(Keyword::ParenRight) => break,
                                token => return Err(unexpected(token, "`)`")),
                            }
                        }
                        Token::Ident(name) => {
                            params.params.push(name);
                            match tokens.next() {
                                Token::Keyword(Keyword::Comma) => (),
                                Token::Keyword(Keyword::ParenRight) => break,
                                token => return Err(unexpected(token, "`,` or `)`")),
                            }
                        }
                        token => return Err(unexpected(token, "parameter name or `)`")),
                    }
                }

                let body = parse_statement_list(tokens)?;

                statements.push(Statement::Func(Func { name, params, body }));
            }

            _ => {
                println!("assuming start of expr :: {}", token);
                let expr = parse_expr(tokens)?;
                statements.push(Statement::Expr(expr))
            }
        }
    }

    Ok(statements)
}

//TODO: Re-order binary operations according to order of operations
fn parse_expr(tokens: &mut TokenIter) -> Result<Expr, ParseError> {
    match tokens.next() {
        Token::Literal(literal) => {
            return Ok(Expr::Literal(literal));
        }
        Token::Ident(ident) => {
            let ident_path = parse_ident_path(tokens, ident)?;
            match tokens.peek() {
                Token::Keyword(Keyword::ParenLeft) => {
                    tokens.next();
                    let mut params = CallParams::default();
                    loop {
                        match tokens.peek() {
                            Token::Keyword(Keyword::ParenRight) => {
                                tokens.next();
                                break;
                            }
                            Token::Keyword(Keyword::Spread) => {
                                tokens.next();
                                let expr = parse_expr(tokens)?;
                                params.rest = Some(Box::new(expr));
                                break;
                            }
                            _ => {
                                let expr = parse_expr(tokens)?;
                                params.params.push(expr);
                                match tokens.next() {
                                    Token::Keyword(Keyword::Comma) => (),
                                    Token::Keyword(Keyword::ParenRight) => break,
                                    token => return Err(unexpected(token, "`,` or `)`")),
                                }
                            }
                        }
                    }
                    println!("{:?} {:?}", ident_path, params);
                    return Ok(Expr::Call(ident_path, params));
                }
                Token::Keyword(Keyword::ParenRight) => (),
                token => unimplemented!("token following ident: {}", token),
            }
            return Ok(Expr::IdentPath(ident_path));
        }

        Token::Keyword(Keyword::Dash) => {
            let expr = parse_expr(tokens)?;
            return Ok(Expr::UnaryOp(UnaryOp::Negative, Box::new(expr)));
        }

        Token::Keyword(Keyword::Not) => {
            let expr = parse_expr(tokens)?;
            return Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(expr)));
        }

        // Token::Keyword(Keyword::BraceLeft) => {
        //
        // }

        Token::Keyword(Keyword::If) => unimplemented!("`if` expression"),
        Token::Keyword(Keyword::Match) => unimplemented!("`match` expression"),

        token => return Err(unexpected(token, "expression")),
    }
}

fn parse_ident_path(tokens: &mut TokenIter, ident: Ident) -> Result<IdentPath, ParseError> {
    let mut path = vec![ident];
    loop {
        let peek = tokens.peek();
        match peek {
            Token::Keyword(Keyword::Dot) => {
                tokens.next();
                let name = match tokens.next() {
                    Token::Ident(name) => name,
                    token => return Err(unexpected(token, "identifier name")),
                };
                path.push(name);
            }
            _ => break,
        }
    }
    let path = if path.len() > 1 {
        let name = path.pop().unwrap();
        IdentPath {
            parents: path,
            name,
        }
    } else {
        IdentPath {
            parents: Vec::new(),
            name: path.pop().unwrap(),
        }
    };
    Ok(path)
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
    params: DeclareParams,
    body: StatementList,
}

type Ident = String;

#[derive(Debug, PartialEq)]
struct IdentPath {
    parents: Vec<Ident>,
    name: Ident,
}

#[derive(Debug, Default, PartialEq)]
struct DeclareParams {
    params: Vec<Ident>,
    rest: Option<Ident>,
}

#[derive(Debug, Default, PartialEq)]
struct CallParams {
    params: Vec<Expr>,
    rest: Option<Box<Expr>>,
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
    IdentPath(IdentPath),
    Call(IdentPath, CallParams),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    If(If),
    Match(Match),
    Table(Table),
    Group(Box<Expr>),
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

        impl Display for Keyword {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Self::$name => write!(f, "{}", $token),
                    )*
                }
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

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eof => write!(f, "<EOF>"),
            Self::Ident(ident) => write!(f, "ident `{}`", ident),
            Self::Keyword(keyword) => write!(f, "`{}`", keyword),
            Self::Literal(literal) => write!(f, "literal {}", literal),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(boolean) => write!(f, "{}", boolean),
            Self::Number(number) => write!(f, "{}", number),
            Self::String(string) => write!(f, "{:?}", string),
        }
    }
}

fn parse_tokens(file: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();

    let mut chars = Backtrackable::from(file.chars());

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
