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
    LessEqual,
    GreaterEqual,
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
    //TODO: Restrict expressions
    // remember: don't bind matched keys to variables !!
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
struct TableItem {
    key: Literal,
    value: Box<Expr>,
}
// enum TableItem {
//     Positional(usize, Box<Expr>),
//     Named(Literal, Box<Expr>),
// }

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
    fn back(&mut self) -> bool {
        self.tokens.back()
    }
}

macro_rules! unexpected {
    ( $found:expr, $expected_1:expr $( , $expected_n:expr )* $(,)? ) => {{
        format!(
            "Unexpected {}. Expected {}",
            $found,
            String::new()
                + &($expected_1).to_string()
                $(
                    + " or " + &($expected_n).to_string()
                )*
        )
    }};
}

pub fn parse_source_module(tokens: Vec<Token>) -> Result<SourceModule, ParseError> {
    let mut tokens = TokenIter::new(tokens);

    let module = parse_statement_list(&mut tokens)?;

    match tokens.peek() {
        Token::Eof => (),
        token => return Err(unexpected!(token, "statement", "end of file")),
    }

    Ok(SourceModule {
        module,
        linked: vec![],
    })
}

fn parse_statement_list(tokens: &mut TokenIter) -> Result<StatementList, ParseError> {
    let mut statements = Vec::new();

    loop {
        match tokens.peek() {
            Token::Eof => {
                println!("\x1b[31m(EOF) this is probably bad\x1b[0m");
                break;
            }

            Token::Keyword(Keyword::End) => {
                println!("(end)");
                tokens.back();
                break;
            }

            Token::Keyword(Keyword::Let) => {
                tokens.next();
                let name = match tokens.next() {
                    Token::Ident(name) => name,
                    token => return Err(unexpected!(token, "variable name")),
                };
                match tokens.next() {
                    Token::Keyword(Keyword::SingleEqual) => (),
                    token => return Err(unexpected!(token, Keyword::SingleEqual)),
                }
                let value = parse_expr(tokens)?;
                statements.push(Statement::Let(Let { name, value }));
            }

            Token::Keyword(Keyword::Func) => {
                tokens.next();
                let name = match tokens.next() {
                    Token::Ident(name) => name,
                    token => return Err(unexpected!(token, "function name")),
                };
                match tokens.next() {
                    Token::Keyword(Keyword::ParenLeft) => (),
                    token => return Err(unexpected!(token, Keyword::ParenLeft)),
                }
                let mut params = DeclareParams::default();
                loop {
                    let token = tokens.next();
                    match token {
                        Token::Keyword(Keyword::ParenRight) => break,
                        Token::Keyword(Keyword::Spread) => {
                            let name = match tokens.next() {
                                Token::Ident(name) => name,
                                token => return Err(unexpected!(token, "parameter name")),
                            };
                            params.rest = Some(name);
                            match tokens.next() {
                                Token::Keyword(Keyword::ParenRight) => break,
                                token => return Err(unexpected!(token, Keyword::ParenRight)),
                            }
                        }
                        Token::Ident(name) => {
                            params.params.push(name);
                            match tokens.next() {
                                Token::Keyword(Keyword::Comma) => (),
                                Token::Keyword(Keyword::ParenRight) => break,
                                token => {
                                    return Err(unexpected!(
                                        token,
                                        Keyword::Comma,
                                        Keyword::ParenRight
                                    ))
                                }
                            }
                        }
                        token => {
                            return Err(unexpected!(token, "parameter name", Keyword::ParenRight))
                        }
                    }
                }

                let body = parse_statement_list(tokens)?;

                match tokens.peek() {
                    Token::Keyword(Keyword::End) => {
                        tokens.next();
                    }
                    token => return Err(unexpected!(token, Keyword::End)),
                }

                statements.push(Statement::Func(Func { name, params, body }));
            }

            token => {
                println!("\x1b[33massuming start of expr ::\x1b[0m {}", token);
                let expr = parse_expr(tokens)?;
                statements.push(Statement::Expr(expr))
            }
        }
    }

    Ok(statements)
}

//TODO: Re-order binary operations according to order of operations
fn parse_expr(tokens: &mut TokenIter) -> Result<Expr, ParseError> {
    let left = parse_expr_part(tokens)?;

    let op = match tokens.peek() {
        Token::Keyword(Keyword::Plus) => BinaryOp::Add,
        Token::Keyword(Keyword::Dash) => BinaryOp::Subtract,
        Token::Keyword(Keyword::Asterisk) => BinaryOp::Multiply,
        Token::Keyword(Keyword::Slash) => BinaryOp::Divide,
        Token::Keyword(Keyword::Percent) => BinaryOp::Modulo,
        Token::Keyword(Keyword::Ampersand) => BinaryOp::Concat,
        Token::Keyword(Keyword::And) => BinaryOp::And,
        Token::Keyword(Keyword::Or) => BinaryOp::Or,
        Token::Keyword(Keyword::DoubleEqual) => BinaryOp::Equal,
        Token::Keyword(Keyword::NotEqual) => BinaryOp::NotEqual,
        Token::Keyword(Keyword::LessThan) => BinaryOp::LessThan,
        Token::Keyword(Keyword::LessEqual) => BinaryOp::LessEqual,
        Token::Keyword(Keyword::GreaterThan) => BinaryOp::GreaterThan,
        Token::Keyword(Keyword::GreaterEqual) => BinaryOp::GreaterEqual,

        _ => {
            tokens.back();
            return Ok(left);
        }
    };

    tokens.next();
    let right = parse_expr(tokens)?;
    Ok(Expr::BinaryOp(op, Box::new(left), Box::new(right)))
}

fn parse_expr_part(tokens: &mut TokenIter) -> Result<Expr, ParseError> {
    match tokens.next() {
        Token::Keyword(Keyword::ParenLeft) => {
            let expr = parse_expr(tokens)?;
            match tokens.next() {
                Token::Keyword(Keyword::ParenRight) => Ok(expr),
                token => Err(unexpected!(token, Keyword::ParenRight)),
            }
        }
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
                                    token => {
                                        return Err(unexpected!(
                                            token,
                                            Keyword::Comma,
                                            Keyword::ParenRight,
                                        ))
                                    }
                                }
                            }
                        }
                    }
                    return Ok(Expr::Call(ident_path, params));
                }
                Token::Keyword(Keyword::ParenRight) => (),

                _ => {
                    tokens.back();
                }
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

            let mut table = Table::default();

            for index in 0.. {
                // Skip to look for `=`
                let _key = tokens.peek();
                let equals = tokens.peek();
                let key = match equals {
                    // Named table
                    Token::Keyword(Keyword::SingleEqual) => {
                        let key = tokens.next();
                        tokens.next();
                        match key {
                            Token::Literal(literal) => literal,
                            Token::Ident(ident) => Literal::String(ident),
                            _ => {
                                return Err(unexpected!(
                                    key,
                                    "literal or identifier with `=`",
                                    "expression",
                                ))
                            }
                        }
                    }
                    // Positional table (any expression)
                    _ => Literal::Number(index as f64),
                };

                let value = parse_expr(tokens)?;
                let value = Box::new(value);

                table.items.push(TableItem { key, value });

                match tokens.peek() {
                    Token::Keyword(Keyword::Comma) => {
                        tokens.next();
                    }
                    _ => {
                        tokens.back();
                    }
                }

                match tokens.peek() {
                    Token::Eof => {
                        return Err(unexpected!(
                            tokens.next(),
                            "table item",
                            Keyword::BraceRight
                        ))
                    }
                    Token::Keyword(Keyword::BraceRight) => {
                        tokens.next();
                        break;
                    }
                    _ => {
                        tokens.back();
                    }
                }
            }

            return Ok(Expr::Table(table));
        }

        Token::Keyword(Keyword::If) => unimplemented!("`if` expression"),
        Token::Keyword(Keyword::Match) => unimplemented!("`match` expression"),

        token => return Err(unexpected!(token, "expression")),
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
                    token => return Err(unexpected!(token, "identifier name")),
                };
                path.push(name);
            }
            _ => {
                tokens.back();
                break;
            }
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
