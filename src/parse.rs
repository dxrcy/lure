use crate::{
    lex::{Keyword, Literal, Token},
    TokenIter,
};

pub type ParseError = String;

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

// Ik this should be called 'Args'. Who cares.
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
    let mut tokens = TokenIter::from(tokens);

    let module = tokens.expect_statement_list()?;

    tokens.expect_eof()?;

    Ok(SourceModule {
        module,
        linked: vec![],
    })
}

impl TokenIter {
    fn expect_eof(&mut self) -> Result<(), ParseError> {
        match self.peek() {
            Token::Eof => Ok(()),
            token => Err(unexpected!(token, "end of file")),
        }
    }

    fn expect_statement_list(&mut self) -> Result<StatementList, ParseError> {
        let mut statements = Vec::new();

        loop {
            let statement = match self.peek() {
                Token::Eof => {
                    println!("\x1b[31m(EOF) this is probably bad\x1b[0m");
                    break;
                }
                Token::Keyword(Keyword::End) => {
                    println!("(end)");
                    break;
                }

                Token::Keyword(Keyword::Let) => self.expect_let_statement()?,
                Token::Keyword(Keyword::Func) => self.expect_func_statement()?,

                token => {
                    println!("\x1b[33massuming start of expr ::\x1b[0m {}", token);
                    self.expect_expr_statement()?
                }
            };

            statements.push(statement);
        }

        Ok(statements)
    }

    fn try_parse<F, T>(&mut self, func: F) -> Option<T>
    where
        F: Fn(&mut Self) -> Result<T, ParseError>,
    {
        let index = self.get_index();
        match func(self) {
            Ok(statement) => Some(statement),
            Err(_) => {
                self.set_index(index);
                None
            }
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<(), ParseError> {
        let token = self.next();
        if token == &Token::Keyword(keyword) {
            Ok(())
        } else {
            Err(unexpected!(token, keyword))
        }
    }

    fn expect_let_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_keyword(Keyword::Let)?;

        let name = self.expect_ident()?;

        self.expect_keyword(Keyword::SingleEqual)?;

        let value = self.expect_expr()?;

        Ok(Statement::Let(Let { name, value }))
    }

    fn expect_func_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_keyword(Keyword::Func)?;

        let name = self.expect_ident()?;

        self.expect_keyword(Keyword::ParenLeft)?;

        let mut params = DeclareParams::default();
        loop {
            match self.next() {
                Token::Keyword(Keyword::ParenRight) => break,

                Token::Keyword(Keyword::Spread) => {
                    let ident = match self.next() {
                        Token::Ident(ident) => ident.to_owned(),
                        token => return Err(unexpected!(token, "parameter name")),
                    };
                    params.rest = Some(ident);
                    self.expect_keyword(Keyword::ParenRight)?;
                }

                Token::Ident(ident) => {
                    params.params.push(ident.to_owned());
                    match self.next() {
                        Token::Keyword(Keyword::Comma) => (),
                        Token::Keyword(Keyword::ParenRight) => break,
                        token => {
                            return Err(unexpected!(token, Keyword::Comma, Keyword::ParenRight))
                        }
                    }
                }

                token => return Err(unexpected!(token, "parameter name", Keyword::ParenRight)),
            }
        }

        let body = self.expect_statement_list()?;

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::Func(Func { name, params, body }))
    }

    fn expect_expr_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expect_expr()?;
        Ok(Statement::Expr(expr))
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        match self.next() {
            Token::Ident(ident) => Ok(ident.to_owned()),
            token => Err(unexpected!(token, "identifier name")),
        }
    }

    fn expect_expr(&mut self) -> Result<Expr, ParseError> {
        let left = self.expect_expr_part()?;

        let op = match self.peek() {
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
                return Ok(left);
            }
        };
        self.next();

        let right = self.expect_expr()?;

        Ok(Expr::BinaryOp(op, Box::new(left), Box::new(right)))
    }

    fn expect_expr_part(&mut self) -> Result<Expr, ParseError> {
        match self.next() {
            Token::Keyword(Keyword::ParenLeft) => {
                let expr = self.expect_expr()?;
                self.expect_keyword(Keyword::ParenRight)?;
                Ok(expr)
            }

            Token::Literal(literal) => {
                return Ok(Expr::Literal(literal.to_owned()));
            }

            Token::Ident(ident) => {
                //TODO: Save index
                let ident = ident.to_owned();
                let ident_path = self.expect_ident_path(ident)?;

                match self.peek() {
                    Token::Keyword(Keyword::ParenLeft) => {
                        self.next();

                        let mut params = CallParams::default();
                        loop {
                            match self.peek() {
                                Token::Keyword(Keyword::ParenRight) => {
                                    self.next();
                                    break;
                                }

                                Token::Keyword(Keyword::Spread) => {
                                    self.next();
                                    let expr = self.expect_expr()?;
                                    params.rest = Some(Box::new(expr));
                                    break;
                                }

                                _ => {
                                    let expr = self.expect_expr()?;
                                    params.params.push(expr);
                                    match self.next() {
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

                    _ => {
                        return Ok(Expr::IdentPath(ident_path));
                    }
                }
            }

            Token::Keyword(Keyword::Dash) => {
                let expr = self.expect_expr()?;
                return Ok(Expr::UnaryOp(UnaryOp::Negative, Box::new(expr)));
            }
            Token::Keyword(Keyword::Not) => {
                let expr = self.expect_expr()?;
                return Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(expr)));
            }

            Token::Keyword(Keyword::BraceLeft) => {
                let mut table = Table::default();
                let mut implicit_key = 0;

                loop {
                    let index = self.get_index();

                    let key_token = self.next().to_owned();
                    let key = match self.next() {
                        Token::Keyword(Keyword::SingleEqual) => match key_token {
                            Token::Literal(literal) => literal.to_owned(),
                            Token::Ident(ident) => Literal::String(ident.to_owned()),

                            _ => {
                                return Err(unexpected!(
                                    Keyword::SingleEqual, // Perhaps more context would be nice
                                    "literal or identifier with `=`",
                                    "expression",
                                ));
                            }
                        },
                        _ => {
                            self.set_index(index);
                            let key = implicit_key;
                            implicit_key += 1;
                            Literal::Number(key as f64)
                        }
                    };

                    let value = self.expect_expr()?;
                    let value = Box::new(value);

                    table.items.push(TableItem { key, value });

                    match self.next() {
                        Token::Keyword(Keyword::Comma) => {
                            if self.peek() == &Token::Keyword(Keyword::BraceRight) {
                                self.next();
                                break;
                            }
                        }
                        Token::Keyword(Keyword::BraceRight) => break,
                        token => {
                            return Err(unexpected!(token, Keyword::Comma, Keyword::BraceRight))
                        }
                    }
                }

                return Ok(Expr::Table(table));
            }

            token => return Err(unexpected!(token, "expression")),
        }
    }

    fn expect_ident_path(&mut self, ident: Ident) -> Result<IdentPath, ParseError> {
        let mut path = vec![ident];

        loop {
            match self.peek() {
                Token::Keyword(Keyword::Dot) => {
                    self.next();
                    match self.next() {
                        Token::Ident(ident) => {
                            path.push(ident.to_owned());
                        }
                        token => {
                            return Err(unexpected!(token, "identifier name"));
                        }
                    }
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
}
