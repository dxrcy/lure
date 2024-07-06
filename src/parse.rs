use std::fmt::Display;

use crate::{
    lex::{Keyword, Literal, Token},
    PeekMore,
};

pub type ParseError = String;

struct TokenIter {
    tokens: PeekMore<std::vec::IntoIter<Token>>,
}

#[derive(Debug, PartialEq)]
pub struct SourceModule {
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

#[derive(Debug, Default, PartialEq)]
struct Table {
    items: Vec<TableItem>,
}

#[derive(Debug, PartialEq)]
enum TableItem {
    Positional(usize, Box<Expr>),
    Named(Literal, Box<Expr>),
}

impl TokenIter {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: PeekMore::from(tokens.into_iter()),
        }
    }
    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token::Eof)
    }
    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap_or(&Token::Eof)
    }
}

pub fn parse_source_module(tokens: Vec<Token>) -> Result<SourceModule, ParseError> {
    let mut tokens = TokenIter::new(tokens);

    let module = parse_statement_list(&mut tokens)?;

    Ok(SourceModule {
        module,
        linked: vec![],
    })
}

fn unexpected(found: impl Display, expected: impl Display) -> String {
    format!("Unexpected {}. Expected {}", found, expected)
}

fn parse_statement_list(tokens: &mut TokenIter) -> Result<StatementList, ParseError> {
    let mut statements = Vec::new();

    loop {
        match tokens.peek() == &Token::Keyword(Keyword::End) {
            true => {
                println!("(end)");
                break;
            }
            false => (),
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

        Token::Keyword(Keyword::BraceLeft) => {
            //TODO: Support nested keys ? This will be annoying to implement

            let table = Table::default();

            for index in 0.. {
                // Skip to look for `=`
                tokens.peek();
                let _key = tokens.peek();
                println!("{}", tokens.peek());
                let equals = tokens.peek();
                println!("{}", equals);
                println!("awdAWODIJAWOIDJOWAIJDO ");
                let key = match equals {
                    // Named table
                    Token::Keyword(Keyword::SingleEqual) => {
                        let key = tokens.next();
                        // println!("key: {}", key);
                        tokens.next();
                        match key {
                            Token::Literal(literal) => literal,
                            Token::Ident(ident) => Literal::String(ident),
                            _ => {
                                return Err(unexpected(
                                    key,
                                    "literal or identifier with `=`, or expression",
                                ))
                            }
                        }
                    }
                    // Positional table (any expression)
                    _ => Literal::Number(index as f64),
                };
                // println!("{}", key);
                let value = parse_expr(tokens)?;
                // println!("{} {:?}", key, value);
                break;
            }

            return Ok(Expr::Table(table));
        }

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
