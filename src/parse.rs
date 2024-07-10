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
    If(IfStatement),
    Match(MatchStatement),
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

//TODO: Get better names for struct fields :)
#[derive(Debug, PartialEq)]
struct LValue {
    origin: Ident,
    parts: Vec<LValuePart>,
}

#[derive(Debug, PartialEq)]
enum LValuePart {
    Ident(Ident),
    Subscript(Expr),
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
    lvalue: LValue,
    value: Expr,
}

#[derive(Debug, PartialEq)]
enum Expr {
    Literal(Literal),
    LValue(LValue),
    Call(LValue, CallParams),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    If(IfExpr),
    Match(MatchExpr),
    Table(Table),
    Group(Box<Expr>),
}

#[derive(Debug, PartialEq)]
enum UnaryOp {
    Not,
    Negative,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
struct IfStatement {
    if_branch: Box<IfBranch>,
    elif_branches: Vec<IfBranch>,
    else_branch: Option<StatementList>,
}

#[derive(Debug, PartialEq)]
struct IfExpr {
    if_branch: Box<IfBranch>,
    elif_branches: Vec<IfBranch>,
    else_branch: StatementList,
}

#[derive(Debug, PartialEq)]
struct IfBranch {
    condition: Expr,
    body: StatementList,
}

#[derive(Debug, PartialEq)]
struct MatchStatement {
    target: Box<Expr>,
    case_branches: Vec<MatchBranch>,
    else_branch: Option<StatementList>,
}

#[derive(Debug, PartialEq)]
struct MatchExpr {
    target: Box<Expr>,
    case_branches: Vec<MatchBranch>,
    else_branch: StatementList,
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
    branch: IfBranch,
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

impl BinaryOp {
    /// ```
    /// (unary)
    ///
    /// * / %
    /// + -
    /// (concat)
    /// < > >= <=
    /// == /=
    /// and
    /// or
    /// ```
    fn precendence(&self) -> u8 {
        use BinaryOp::*;
        match self {
            Multiply | Divide | Modulo => 1,
            Add | Subtract => 2,
            Concat => 3,
            LessThan | LessEqual | GreaterThan | GreaterEqual => 4,
            Equal | NotEqual => 5,
            And => 6,
            Or => 7,
        }
    }
}

impl PartialOrd for BinaryOp {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.precendence().partial_cmp(&other.precendence())
    }
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
            token => Err(unexpected!(token, Token::Eof)),
        }
    }
    fn expect_keyword(&mut self, keyword: Keyword) -> Result<(), ParseError> {
        match self.next() {
            Token::Keyword(other) if other == &keyword => Ok(()),
            token => Err(unexpected!(token, keyword)),
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
                Token::Keyword(Keyword::Elif) => {
                    println!("(elif)");
                    break;
                }
                Token::Keyword(Keyword::Else) => {
                    println!("(else)");
                    break;
                }
                Token::Keyword(Keyword::Case) => {
                    println!("(case)");
                    break;
                }

                Token::Keyword(Keyword::Let) => self.expect_let_statement()?,
                Token::Keyword(Keyword::Func) => self.expect_func_statement()?,
                Token::Keyword(Keyword::If) => self.expect_if_statement()?,
                Token::Keyword(Keyword::Match) => self.expect_match_statement()?,

                token => {
                    let token = token.to_owned();
                    match self.try_parse_assign() {
                        Some(statement) => statement?,
                        None => {
                            println!("\x1b[33massuming start of expr ::\x1b[0m {}", token);
                            self.expect_expr_statement()?
                        }
                    }
                }
            };

            statements.push(statement);
        }

        Ok(statements)
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

    fn expect_if_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_keyword(Keyword::If)?;

        let condition = self.expect_expr()?;
        self.expect_keyword(Keyword::Then)?;
        let body = self.expect_statement_list()?;
        let if_branch = Box::new(IfBranch { condition, body });

        let mut elif_branches = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Elif) => {
                    self.next();
                    let condition = self.expect_expr()?;
                    self.expect_keyword(Keyword::Then)?;
                    let body = self.expect_statement_list()?;
                    elif_branches.push(IfBranch { condition, body });
                }
                Token::Keyword(Keyword::Else) => break,
                Token::Keyword(Keyword::End) => break,
                token => {
                    return Err(unexpected!(
                        token,
                        Keyword::Elif,
                        Keyword::Else,
                        Keyword::End
                    ))
                }
            }
        }

        let else_branch = match self.peek() {
            Token::Keyword(Keyword::Else) => {
                self.next();
                let body = self.expect_statement_list()?;
                Some(body)
            }
            Token::Keyword(Keyword::End) => None,
            token => return Err(unexpected!(token, Keyword::Else, Keyword::End)),
        };

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::If(IfStatement {
            if_branch,
            elif_branches,
            else_branch,
        }))
    }

    fn expect_if_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect_keyword(Keyword::If)?;

        let condition = self.expect_expr()?;
        self.expect_keyword(Keyword::Then)?;
        let body = self.expect_statement_list()?;
        let if_branch = Box::new(IfBranch { condition, body });

        let mut elif_branches = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Elif) => {
                    self.next();
                    let condition = self.expect_expr()?;
                    self.expect_keyword(Keyword::Then)?;
                    let body = self.expect_statement_list()?;
                    elif_branches.push(IfBranch { condition, body });
                }
                Token::Keyword(Keyword::Else) => break,
                token => return Err(unexpected!(token, Keyword::Elif, Keyword::Else,)),
            }
        }

        let else_branch = match self.peek() {
            Token::Keyword(Keyword::Else) => {
                self.next();
                let body = self.expect_statement_list()?;
                body
            }
            token => return Err(unexpected!(token, Keyword::Else)),
        };

        self.expect_keyword(Keyword::End)?;

        Ok(Expr::If(IfExpr {
            if_branch,
            elif_branches,
            else_branch,
        }))
    }

    fn expect_match_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_keyword(Keyword::Match)?;

        let target = Box::new(self.expect_expr()?);

        let mut case_branches = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Case) => {
                    self.next();
                    let pattern = self.expect_expr()?;
                    self.expect_keyword(Keyword::Then)?;
                    let body = self.expect_statement_list()?;
                    case_branches.push(MatchBranch { pattern, body });
                }
                Token::Keyword(Keyword::Else) => break,
                Token::Keyword(Keyword::End) => break,
                token => {
                    return Err(unexpected!(
                        token,
                        Keyword::Case,
                        Keyword::Else,
                        Keyword::End
                    ))
                }
            }
        }

        let else_branch = match self.peek() {
            Token::Keyword(Keyword::Else) => {
                self.next();
                let body = self.expect_statement_list()?;
                Some(body)
            }
            Token::Keyword(Keyword::End) => None,
            token => return Err(unexpected!(token, Keyword::Else, Keyword::End)),
        };

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::Match(MatchStatement {
            target,
            case_branches,
            else_branch,
        }))
    }

    fn expect_match_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect_keyword(Keyword::Match)?;

        let target = Box::new(self.expect_expr()?);

        let mut case_branches = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Case) => {
                    self.next();
                    let pattern = self.expect_expr()?;
                    self.expect_keyword(Keyword::Then)?;
                    let body = self.expect_statement_list()?;
                    case_branches.push(MatchBranch { pattern, body });
                }
                Token::Keyword(Keyword::Else) => break,
                token => return Err(unexpected!(token, Keyword::Case, Keyword::Else,)),
            }
        }

        let else_branch = match self.peek() {
            Token::Keyword(Keyword::Else) => {
                self.next();
                let body = self.expect_statement_list()?;
                body
            }
            token => return Err(unexpected!(token, Keyword::Else)),
        };

        self.expect_keyword(Keyword::End)?;

        Ok(Expr::Match(MatchExpr {
            target,
            case_branches,
            else_branch,
        }))
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

        let left_op = match self.peek() {
            Token::Keyword(Keyword::Plus) => BinaryOp::Add,
            Token::Keyword(Keyword::Dash) => BinaryOp::Subtract,
            Token::Keyword(Keyword::Asterisk) => BinaryOp::Multiply,
            Token::Keyword(Keyword::Slash) => BinaryOp::Divide,
            Token::Keyword(Keyword::Percent) => BinaryOp::Modulo,
            Token::Keyword(Keyword::Ampersand) => BinaryOp::Concat, // This may change
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

        match right {
            // Swap order of binary operations
            // UNLESS the inner (rightmost) operation has higher precedence
            // Note that equal-precendence operations will always swap, to
            // maintain default left-to-right order
            Expr::BinaryOp(right_op, middle, right) if !(right_op < left_op) => {
                // Preserve literal order of left-middle-right, but make the
                // leftmost expression the nested one
                return Ok(Expr::BinaryOp(
                    right_op,
                    Box::new(Expr::BinaryOp(left_op, Box::new(left), middle)),
                    right,
                ));
            }

            _ => {
                return Ok(Expr::BinaryOp(left_op, Box::new(left), Box::new(right)));
            }
        }
    }

    fn expect_expr_part(&mut self) -> Result<Expr, ParseError> {
        match self.next() {
            Token::Keyword(Keyword::ParenLeft) => {
                let expr = self.expect_expr()?;
                self.expect_keyword(Keyword::ParenRight)?;
                Ok(Expr::Group(Box::new(expr)))
            }

            Token::Literal(literal) => {
                return Ok(Expr::Literal(literal.to_owned()));
            }

            Token::Ident(ident) => {
                //TODO: Save index
                let ident = ident.to_owned();
                let lvalue = self.expect_lvalue(ident)?;

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

                        return Ok(Expr::Call(lvalue, params));
                    }

                    _ => {
                        return Ok(Expr::LValue(lvalue));
                    }
                }
            }

            Token::Keyword(Keyword::Dash) => {
                return Ok(Expr::UnaryOp(
                    UnaryOp::Negative,
                    Box::new(self.expect_expr()?),
                ));
            }
            Token::Keyword(Keyword::Not) => {
                return Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(self.expect_expr()?)));
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

            Token::Keyword(Keyword::If) => {
                self.reverse(1);
                return Ok(self.expect_if_expr()?);
            }

            Token::Keyword(Keyword::Match) => {
                self.reverse(1);
                return Ok(self.expect_match_expr()?);
            }

            token => return Err(unexpected!(token, "expression")),
        }
    }

    fn expect_lvalue(&mut self, origin: Ident) -> Result<LValue, ParseError> {
        let mut parts = Vec::new();

        loop {
            let part = match self.peek() {
                Token::Keyword(Keyword::Dot) => {
                    self.next();
                    match self.next() {
                        Token::Ident(ident) => LValuePart::Ident(ident.to_owned()),
                        token => {
                            return Err(unexpected!(token, "identifier name"));
                        }
                    }
                }

                Token::Keyword(Keyword::BracketLeft) => {
                    self.next();
                    let expr = self.expect_expr()?;
                    self.expect_keyword(Keyword::BracketRight)?;
                    LValuePart::Subscript(expr)
                }

                _ => break,
            };
            parts.push(part);
        }

        Ok(LValue { origin, parts })
    }

    //TODO: Refactor this ideally. it is not very nice
    fn try_parse_assign(&mut self) -> Option<Result<Statement, ParseError>> {
        // We don't yet know if it is an assignment statement or not
        let index = self.get_index();

        let origin = match self.next() {
            Token::Ident(ident) => ident.to_owned(),

            _ => {
                self.set_index(index);
                return None;
            }
        };

        let Ok(lvalue) = self.expect_lvalue(origin) else {
            self.set_index(index);
            return None;
        };

        if self.expect_keyword(Keyword::SingleEqual).is_err() {
            self.set_index(index);
            return None;
        };

        // Ok now from here we know it MUST be an assignment

        let value = match self.expect_expr() {
            Ok(expr) => expr,
            Err(err) => return Some(Err(err)),
        };

        Some(Ok(Statement::Assign(Assign { lvalue, value })))
    }
}
