use crate::{
    lex::{Keyword, Literal, Number, Token, TokenRef},
    ParseError, ParseErrorKind, TokenIter,
};

// pub type ModuleName = String;

pub type StatementBody = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Func(FuncStatement),
    Assign(AssignStatement),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Return(ReturnStatement),
    Break,
    Continue,
    // TODO(feat): Don't allow expr as statement
    // Only function calls
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub struct FuncStatement {
    pub name: Ident,
    pub params: FuncParams,
    pub body: StatementBody,
}

#[derive(Debug, PartialEq)]
pub struct FuncExpr {
    pub params: FuncParams,
    pub body: StatementBody,
}

pub type Ident = String;

#[derive(Debug, PartialEq)]
pub struct Chain<T> {
    pub origin: Ident,
    pub chain: Vec<T>,
}

impl<T> Chain<T> {
    pub fn from(origin: Ident, chain: Vec<T>) -> Self {
        Self { origin, chain }
    }
}

/// Anything which can appear to the left of an assignment
///
/// Note: Not used for `let` statements
pub type AssignableChain = Chain<AssignableSegment>;

#[derive(Debug, PartialEq)]
pub enum AssignableSegment {
    Name(Ident),
    Index(Expr),
    Slice(Expr, Expr),
}

/// Any single value, which can be a combination of field or subscript accesses,
/// and function calls
pub type AccessibleChain = Chain<AccessibleSegment>;

#[derive(Debug, PartialEq)]
pub enum AccessibleSegment {
    Name(Ident),
    Index(Expr),
    Slice(Expr, Expr),
    Call(FuncArgs),
}

#[derive(Debug, Default, PartialEq)]
pub struct FuncParams {
    pub params: Vec<Ident>,
    pub rest_param: Option<Ident>,
}

#[derive(Debug, Default, PartialEq)]
pub struct FuncArgs {
    pub args: Vec<Expr>,
    pub spread_arg: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub values: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct AssignStatement {
    pub name: AssignableChain,
    pub value: Expr,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Group(Box<Expr>),
    Literal(Literal),
    Chain(Box<AccessibleChain>),
    UnaryOp {
        op: UnaryOp,
        right: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Table {
        table: TableExpr,
        metatable: Option<AccessibleChain>,
    },
    If(IfExpr),
    Func(FuncExpr),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Negative,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOp {
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
pub struct IfStatement {
    pub if_branch: Box<ConditionalBranch>,
    pub elif_branches: Vec<ConditionalBranch>,
    pub else_branch: Option<StatementBody>,
}

#[derive(Debug, PartialEq)]
pub struct IfExpr {
    pub if_branch: Box<ConditionalBranch>,
    pub elif_branches: Vec<ConditionalBranch>,
    pub else_branch: StatementBody,
}

#[derive(Debug, PartialEq)]
pub struct ConditionalBranch {
    pub condition: Expr,
    pub body: StatementBody,
}

#[derive(Debug, PartialEq)]
pub struct ForStatement {
    pub key: Option<Ident>,
    pub value: Option<Ident>,
    pub source: ForSource,
    pub body: StatementBody,
}

#[derive(Debug, PartialEq)]
pub enum ForSource {
    Range(Expr, Expr),
    Iterable(Expr),
}

#[derive(Debug, PartialEq)]
pub struct WhileStatement {
    pub branch: ConditionalBranch,
}

#[derive(Debug, Default, PartialEq)]
pub struct TableExpr {
    pub entries: Vec<TableEntry>,
    pub base_table: Option<AccessibleChain>,
}

#[derive(Debug, PartialEq)]
pub enum TableEntry {
    Literal {
        key: Literal,
        value: Box<Expr>,
    },
    Chain {
        key: AssignableChain,
        value: Box<Expr>,
    },
    Func(FuncStatement),
}

/// >=1 items
#[derive(Debug, PartialEq)]
pub struct Plural<T> {
    pub first: T,
    pub rest: Vec<T>,
}

// impl<T> Plural<T> {
//     pub fn from(first: T, rest: Vec<T>) -> Self {
//         Self { first, rest }
//     }
// }

macro_rules! unexpected {
    (
        $line:expr,
        $found:expr,
        [ $expected_1:expr $(, $expected_n:expr )* $(,)? ]
        $(, $reason:expr )? $(,)?
    ) => {{
        let found = ($found).into();
        ParseError {
            line: $line,
            error: ParseErrorKind::Unexpected {
                found,
                expected: String::new()
                    + &($expected_1).to_string()
                    $(
                        + " or " + &($expected_n).to_string()
                    )*,
                reason: unexpected!(@reason $($reason)?),
            },
        }
    }};

    (@reason) => { None };
    (@reason $reason:expr) => { Some($reason) };
}

impl BinaryOp {
    /// ```lure
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
        Some(self.cmp(other))
    }
}
impl Ord for BinaryOp {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.precendence().cmp(&other.precendence())
    }
}

pub fn parse_file_module(tokens: Vec<TokenRef>) -> Result<TableExpr, ParseError> {
    let mut tokens = TokenIter::from(tokens);

    let table = tokens.expect_file_contents()?;

    // Does this need to be here ?
    tokens.expect_eof()?;

    Ok(table)
}

impl TokenIter {
    fn expect_eof(&mut self) -> Result<(), ParseError> {
        match self.peek() {
            Token::Eof => Ok(()),
            token => Err(unexpected!(self.line(), token.to_owned(), [Token::Eof])),
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword, reason: &'static str) -> Result<(), ParseError> {
        match self.next() {
            Token::Keyword(other) if other == &keyword => Ok(()),
            token => Err(unexpected!(
                self.line(),
                token.to_owned(),
                [keyword],
                reason
            )),
        }
    }

    fn expect_keyword_end(&mut self) -> Result<(), ParseError> {
        self.expect_keyword(Keyword::End, "Code block must be closed with `end` keyword")?;
        Ok(())
    }

    fn expect_file_contents(&mut self) -> Result<TableExpr, ParseError> {
        self.expect_table(true)
    }

    fn expect_body(&mut self) -> Result<StatementBody, ParseError> {
        let mut statements = Vec::new();

        loop {
            let statement = match self.peek() {
                Token::Eof
                | Token::Keyword(Keyword::End | Keyword::Elif | Keyword::Else | Keyword::Case) => {
                    println!("(close)");
                    break;
                }

                Token::Keyword(Keyword::Func) => {
                    self.next();
                    Statement::Func(self.expect_func_statement()?)
                }
                Token::Keyword(Keyword::If) => {
                    self.next();
                    Statement::If(self.expect_if_statement()?)
                }
                Token::Keyword(Keyword::For) => {
                    self.next();
                    Statement::For(self.expect_for_statement()?)
                }
                Token::Keyword(Keyword::While) => {
                    self.next();
                    Statement::While(self.expect_while_statement()?)
                }

                Token::Keyword(Keyword::Break) => {
                    self.next();
                    Statement::Break
                }
                Token::Keyword(Keyword::Continue) => {
                    self.next();
                    Statement::Continue
                }
                Token::Keyword(Keyword::Return) => {
                    self.next();
                    Statement::Return(self.expect_return_statement()?)
                }

                _ => {
                    // let token = token.to_owned();
                    match self.try_assign_statement() {
                        Some(statement) => {
                            let statement = statement?;
                            Statement::Assign(statement)
                        }
                        None => {
                            // println!("\x1b[33massuming start of expr ::\x1b[0m {}", token);
                            Statement::Expr(self.expect_expr()?)
                        }
                    }
                }
            };

            statements.push(statement);
        }

        Ok(statements)
    }

    fn next_token_ends_body(&mut self) -> bool {
        matches!(
            self.peek(),
            Token::Keyword(Keyword::End)
                | Token::Keyword(Keyword::Elif)
                | Token::Keyword(Keyword::Else)
        )
    }

    fn expect_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        if self.next_token_ends_body() {
            return Ok(ReturnStatement { values: Vec::new() });
        }

        let mut values = Vec::new();

        loop {
            let value = self.expect_expr()?;
            values.push(value);

            if self.next_token_ends_body() {
                break;
            }

            match self.peek() {
                Token::Keyword(Keyword::Comma) => {
                    self.next();
                }

                token => {
                    return Err(unexpected!(
                        self.line(),
                        token.to_owned(),
                        // Do not include `elif` and `else`
                        [Keyword::Comma, Keyword::End],
                        "`return` must be not be followed by another statement",
                    ));
                }
            }
        }

        Ok(ReturnStatement { values })
    }

    fn expect_func_statement(&mut self) -> Result<FuncStatement, ParseError> {
        let name = self.expect_ident()?;

        self.expect_keyword(
            Keyword::ParenLeft,
            "`func` statement must include parameter list between parentheses",
        )?;

        let params = self.expect_params()?;

        let body = self.expect_body()?;

        self.expect_keyword_end()?;

        Ok(FuncStatement { name, params, body })
    }

    fn expect_func_expr(&mut self) -> Result<FuncExpr, ParseError> {
        match self.next() {
            Token::Keyword(Keyword::ParenLeft) => (),
            token => {
                let token = token.to_owned();
                let reason = if matches!(token, Token::Ident(_)) {
                    "`func` expression cannot be named"
                } else {
                    "`func` expression must include parameter list between parentheses"
                };
                return Err(unexpected!(
                    self.line(),
                    token,
                    [Keyword::ParenLeft],
                    reason
                ));
            }
        }

        let params = self.expect_params()?;

        let body = self.expect_body()?;

        self.expect_keyword_end()?;

        Ok(FuncExpr { params, body })
    }

    fn expect_params(&mut self) -> Result<FuncParams, ParseError> {
        let mut params = FuncParams::default();
        loop {
            match self.next() {
                Token::Keyword(Keyword::ParenRight) => break,

                Token::Keyword(Keyword::Spread) => {
                    let ident = match self.next() {
                        Token::Ident(ident) => ident.to_owned(),
                        token => {
                            return Err(unexpected!(
                                self.line(),
                                token.to_owned(),
                                ["parameter name"],
                                "Spread operator must be followed by a parameter name",
                            ))
                        }
                    };
                    params.rest_param = Some(ident);
                    self.expect_keyword(
                        Keyword::ParenRight,
                        "Spread parameter must be last parameter",
                    )?;
                    break;
                }

                Token::Ident(ident) => {
                    params.params.push(ident.to_owned());
                    match self.next() {
                        Token::Keyword(Keyword::Comma) => (),
                        Token::Keyword(Keyword::ParenRight) => break,
                        token => {
                            return Err(unexpected!(
                                self.line(),
                                token.to_owned(),
                                [Keyword::Comma, Keyword::ParenRight],
                                "Parameters must be separated with commas",
                            ))
                        }
                    }
                }

                token => {
                    return Err(unexpected!(
                        self.line(),
                        token.to_owned(),
                        ["parameter name", Keyword::ParenRight],
                        "Parameter list must end with `)`",
                    ));
                }
            }
        }

        Ok(params)
    }

    fn expect_if_statement(&mut self) -> Result<IfStatement, ParseError> {
        let condition = self.expect_expr()?;
        self.expect_keyword(
            Keyword::Then,
            "`if` condition must be followed with `then` keyword",
        )?;
        let body = self.expect_body()?;
        let if_branch = Box::new(ConditionalBranch { condition, body });

        let mut elif_branches = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Elif) => {
                    self.next();
                    let condition = self.expect_expr()?;
                    self.expect_keyword(
                        Keyword::Then,
                        "`elif` condition must be followed with `then` keyword",
                    )?;
                    let body = self.expect_body()?;
                    elif_branches.push(ConditionalBranch { condition, body });
                }
                Token::Keyword(Keyword::Else) => break,
                Token::Keyword(Keyword::End) => break,
                token => {
                    return Err(unexpected!(
                        self.line(),
                        token.to_owned(),
                        [Keyword::Elif, Keyword::Else, Keyword::End],
                        "`if` statement must end with `end` keyword",
                    ));
                }
            }
        }

        let else_branch = match self.peek() {
            Token::Keyword(Keyword::Else) => {
                self.next();
                let body = self.expect_body()?;
                Some(body)
            }
            Token::Keyword(Keyword::End) => None,
            token => {
                return Err(unexpected!(
                    self.line(),
                    token.to_owned(),
                    [Keyword::Else, Keyword::End],
                    "`if` statement must end with `end` keyword",
                ));
            }
        };

        self.expect_keyword_end()?;

        Ok(IfStatement {
            if_branch,
            elif_branches,
            else_branch,
        })
    }

    fn expect_if_expr(&mut self) -> Result<IfExpr, ParseError> {
        let if_statement = self.expect_if_statement()?;

        let IfStatement {
            if_branch,
            elif_branches,
            else_branch,
        } = if_statement;

        let Some(else_branch) = else_branch else {
            return Err(unexpected!(
                self.line(),
                Token::Keyword(Keyword::End),
                [Keyword::Elif, Keyword::Else],
                "`if` expressions must include an `else` branch",
            ));
        };

        Ok(IfExpr {
            if_branch,
            elif_branches,
            else_branch,
        })
    }

    fn expect_for_statement(&mut self) -> Result<ForStatement, ParseError> {
        let key = self.expect_ident_or_spread()?;
        let value = if self.peek() == &Token::Keyword(Keyword::SingleEqual) {
            self.next();
            let value = self.expect_ident_or_spread()?;
            if key.is_none() {
                return Err(unexpected!(
                    self.line(),
                    Keyword::Spread,
                    ["identifier name"],
                    "cannot discard key in this way. Use `for .. in` or `for value in` instead"
                ));
            }
            value
        } else {
            None
        };

        self.expect_keyword(Keyword::In, "`for` statement must include `in` keyword")?;

        let from = self.expect_expr()?;

        let source = match self.next() {
            Token::Keyword(Keyword::To) => {
                let to = self.expect_expr()?;
                self.expect_keyword(
                    Keyword::Do,
                    "`for` statement must include `do` keyword after iteration range",
                )?;
                ForSource::Range(from, to)
            }
            Token::Keyword(Keyword::Do) => ForSource::Iterable(from),

            token => {
                return Err(unexpected!(
                    self.line(),
                    token.to_owned(),
                    [Keyword::To, Keyword::Do],
                    "`for` statement must include `to` or `do` keyword after iterable expression"
                ));
            }
        };

        let body = self.expect_body()?;

        self.expect_keyword_end()?;

        Ok(ForStatement {
            key,
            value,
            source,
            body,
        })
    }

    fn expect_while_statement(&mut self) -> Result<WhileStatement, ParseError> {
        let condition = self.expect_expr()?;

        self.expect_keyword(
            Keyword::Do,
            "`while` statement must include `do` keyword following condition expression",
        )?;

        let body = self.expect_body()?;

        self.expect_keyword_end()?;

        Ok(WhileStatement {
            branch: ConditionalBranch { condition, body },
        })
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        match self.next() {
            Token::Ident(ident) => Ok(ident.to_owned()),
            token => Err(unexpected!(
                self.line(),
                token.to_owned(),
                ["identifier name"],
                // reason omitted
            )),
        }
    }

    fn expect_ident_or_spread(&mut self) -> Result<Option<Ident>, ParseError> {
        match self.next() {
            Token::Keyword(Keyword::Spread) => Ok(None),
            Token::Ident(ident) => Ok(Some(ident.to_owned())),
            token => Err(unexpected!(
                self.line(),
                token.to_owned(),
                ["identifier name", Keyword::Spread],
                // reason omitted
            )),
        }
    }

    fn expect_expr(&mut self) -> Result<Expr, ParseError> {
        let left = self.expect_subexpr()?;

        let left_op = match self.peek() {
            Token::Keyword(Keyword::Plus) => BinaryOp::Add,
            Token::Keyword(Keyword::Dash) => BinaryOp::Subtract,
            Token::Keyword(Keyword::Asterisk) => BinaryOp::Multiply,
            Token::Keyword(Keyword::Slash) => BinaryOp::Divide,
            Token::Keyword(Keyword::Percent) => BinaryOp::Modulo,
            Token::Keyword(Keyword::Colon) => BinaryOp::Concat, // This may change
            Token::Keyword(Keyword::And) => BinaryOp::And,
            Token::Keyword(Keyword::Or) => BinaryOp::Or,
            Token::Keyword(Keyword::DoubleEqual) => BinaryOp::Equal,
            Token::Keyword(Keyword::NotEqual) => BinaryOp::NotEqual,
            Token::Keyword(Keyword::LessThan) => BinaryOp::LessThan,
            Token::Keyword(Keyword::LessEqual) => BinaryOp::LessEqual,
            Token::Keyword(Keyword::GreaterThan) => BinaryOp::GreaterThan,
            Token::Keyword(Keyword::GreaterEqual) => BinaryOp::GreaterEqual,

            Token::Keyword(Keyword::SingleEqual) => {
                return Err(unexpected!(
                    self.line(),
                    Keyword::SingleEqual,
                    ["rest of expression", "statement"],
                    "Assignment operations cannot be used as expressions"
                ));
            }

            _ => {
                return Ok(left);
            }
        };
        self.next();
        let left = Box::new(left);

        let right = self.expect_expr()?;

        match right {
            // Swap order of binary operations
            // UNLESS the inner (rightmost) operation has higher precedence
            // Note that equal-precendence operations will always swap, to
            // maintain default left-to-right order
            Expr::BinaryOp {
                op: right_op,
                left: middle,
                right,
            } if right_op >= left_op => {
                // Preserve literal order of left-middle-right, but make the
                // leftmost expression the nested one
                let left = Box::new(Expr::BinaryOp {
                    op: left_op,
                    left,
                    right: middle,
                });
                return Ok(Expr::BinaryOp {
                    op: right_op,
                    left,
                    right,
                });
            }

            _ => {
                let right = Box::new(right);
                return Ok(Expr::BinaryOp {
                    op: left_op,
                    left,
                    right,
                });
            }
        }
    }

    fn expect_subexpr(&mut self) -> Result<Expr, ParseError> {
        match self.next() {
            Token::Keyword(Keyword::ParenLeft) => {
                let expr = self.expect_expr()?;
                self.expect_keyword(
                    Keyword::ParenRight,
                    "Unmatched parenthesis in expression group",
                )?;
                Ok(Expr::Group(Box::new(expr)))
            }

            Token::Literal(literal) => {
                return Ok(Expr::Literal(literal.to_owned()));
            }

            Token::Ident(ident) => {
                let origin = ident.to_owned();
                let value = self.expect_accessible_chain(origin)?;
                return Ok(Expr::Chain(Box::new(value)));
            }

            Token::Keyword(Keyword::Dash) => {
                return Ok(Expr::UnaryOp {
                    op: UnaryOp::Negative,
                    right: Box::new(self.expect_expr()?),
                });
            }
            Token::Keyword(Keyword::Not) => {
                return Ok(Expr::UnaryOp {
                    op: UnaryOp::Not,
                    right: Box::new(self.expect_expr()?),
                });
            }

            Token::Keyword(Keyword::BraceLeft) => {
                let table = self.expect_table_expr()?;
                return Ok(Expr::Table {
                    table,
                    metatable: None,
                });
            }

            Token::Keyword(Keyword::As) => {
                let origin = self.expect_ident()?;
                let name = self.expect_accessible_chain(origin)?;
                self.expect_keyword(
                    Keyword::BraceLeft,
                    "Metatable name must be followed by table",
                )?;
                let table = self.expect_table_expr()?;
                return Ok(Expr::Table {
                    table,
                    metatable: Some(name),
                });
            }

            Token::Keyword(Keyword::If) => {
                return Ok(Expr::If(self.expect_if_expr()?));
            }

            Token::Keyword(Keyword::Func) => {
                return Ok(Expr::Func(self.expect_func_expr()?));
            }

            token => {
                let token = token.to_owned();
                let reason = match token {
                    Token::Keyword(Keyword::For) => "Cannot use `for` statement as an expression",
                    Token::Keyword(Keyword::While) => {
                        "Cannot use `while` statement as an expression"
                    }
                    // Generic reason
                    _ => "Cannot parse the tokens as an expression",
                };
                return Err(unexpected!(self.line(), token, ["expression"], reason));
            }
        }
    }

    fn expect_accessible_chain(&mut self, origin: Ident) -> Result<AccessibleChain, ParseError> {
        //TODO: Save index
        // ^ ????? Why?

        let mut rest = Vec::new();
        loop {
            let part = match self.peek() {
                Token::Keyword(Keyword::Dot) => {
                    self.next();
                    match self.next() {
                        Token::Ident(ident) => {
                            let name = ident.to_owned();
                            AccessibleSegment::Name(name)
                        }
                        token => {
                            let reason = match token {
                                Token::Keyword(Keyword::BracketLeft) => {
                                    "Subscript expression should not directly follow `.`"
                                }
                                _ => "",
                            };
                            return Err(unexpected!(
                                self.line(),
                                token.to_owned(),
                                ["table entry name or expression"],
                                reason,
                            ));
                        }
                    }
                }

                Token::Keyword(Keyword::BracketLeft) => {
                    self.next();
                    let index = self.expect_expr()?;
                    match self.next() {
                        Token::Keyword(Keyword::BracketRight) => AccessibleSegment::Index(index),
                        Token::Keyword(Keyword::Comma) => {
                            let end = self.expect_expr()?;
                            self.expect_keyword(
                                Keyword::BracketRight,
                                "Subscript expression should end with `]`",
                            )?;
                            AccessibleSegment::Slice(index, end)
                        }
                        token => {
                            return Err(unexpected!(
                                self.line(),
                                token.to_owned(),
                                // Or rest of expression ?
                                [Keyword::BracketRight, Keyword::Comma],
                                "Subscript expression should end with `]`",
                            ));
                        }
                    }
                }

                Token::Keyword(Keyword::ParenLeft) => {
                    self.next();

                    let mut params = FuncArgs::default();
                    loop {
                        match self.peek() {
                            Token::Keyword(Keyword::ParenRight) => {
                                self.next();
                                break;
                            }

                            Token::Keyword(Keyword::Spread) => {
                                self.next();
                                let expr = self.expect_expr()?;
                                params.spread_arg = Some(Box::new(expr));
                                self.expect_keyword(
                                    Keyword::ParenRight,
                                    "Spread argument must be last argument",
                                )?;
                                break;
                            }

                            _ => {
                                let expr = self.expect_expr()?;
                                params.args.push(expr);
                                match self.next() {
                                    Token::Keyword(Keyword::Comma) => (),
                                    Token::Keyword(Keyword::ParenRight) => break,
                                    token => {
                                        return Err(unexpected!(
                                            self.line(),
                                            token.to_owned(),
                                            [Keyword::Comma, Keyword::ParenRight],
                                            "Arguments must be separated with commas",
                                        ))
                                    }
                                }
                            }
                        }
                    }

                    AccessibleSegment::Call(params)
                }

                _ => {
                    break;
                }
            };
            rest.push(part);
        }

        return Ok(AccessibleChain::from(origin, rest));
    }

    fn expect_table_expr(&mut self) -> Result<TableExpr, ParseError> {
        self.expect_table(false)
    }

    fn expect_table(&mut self, is_file: bool) -> Result<TableExpr, ParseError> {
        let mut table = TableExpr::default();
        let mut implicit_key = 0;

        let delim_token = if is_file {
            Token::Eof
        } else {
            Token::Keyword(Keyword::BraceRight)
        };

        // Empty table
        if self.peek() == &delim_token {
            self.next();
            return Ok(table);
        }

        loop {
            match self.peek() {
                Token::Keyword(Keyword::Comma) => {
                    return Err(unexpected!(
                        self.line(),
                        Keyword::Comma,
                        [
                            delim_token,
                            "literal or identifier with `=`",
                            "expression",
                            "function",
                        ],
                        "Tables do not use commas to separate entries",
                    ));
                }

                token if token == &delim_token => {
                    self.next();
                    break;
                }

                Token::Keyword(Keyword::Spread) => {
                    self.next();
                    let origin = self.expect_ident()?;
                    let value = self.expect_accessible_chain(origin)?;
                    table.base_table = Some(value);
                    if is_file {
                        self.expect_eof()?; // TODO: "Spread key must be last key"
                    } else {
                        self.expect_keyword(Keyword::BraceRight, "Spread key must be last key")?;
                    }
                    break;
                }

                Token::Keyword(Keyword::Func) => {
                    let index = self.get_index();
                    self.next();
                    // Handle function expression (with implicit key)
                    if self.peek() == &Token::Keyword(Keyword::ParenLeft) {
                        self.set_index(index);
                    } else {
                        let func = self.expect_func_statement()?;
                        table.entries.push(TableEntry::Func(func));
                        continue;
                    }
                }

                _ => (),
            }

            let entry: TableEntry = 'entry: {
                let index = self.get_index();

                // Both of these can fail to match, if the following token is not `=`
                // Leave this as a `match` statement for readability
                match self.peek() {
                    Token::Ident(_) => {
                        // Consumes `=` and value expression
                        if let Some(assign) = self.try_assign_statement() {
                            let assign = assign?;
                            let key = assign.name;
                            let value = Box::new(assign.value);
                            break 'entry TableEntry::Chain { key, value };
                        }
                    }

                    Token::Literal(literal) => {
                        let key = literal.to_owned();
                        self.next();
                        if self.next() == &Token::Keyword(Keyword::SingleEqual) {
                            let value = Box::new(self.expect_expr()?);
                            break 'entry TableEntry::Literal { key, value };
                        }
                    }

                    _ => (),
                }

                // Fallback to positional expression
                self.set_index(index);
                let key = Literal::Number(implicit_key as Number);
                implicit_key += 1;
                let value = Box::new(self.expect_expr()?);
                TableEntry::Literal { key, value }
            };

            table.entries.push(entry);
        }

        Ok(table)
    }

    fn expect_assignable_chain(&mut self, origin: Ident) -> Result<AssignableChain, ParseError> {
        let mut parts = Vec::new();

        loop {
            let part = match self.peek() {
                Token::Keyword(Keyword::Dot) => {
                    self.next();
                    match self.next() {
                        Token::Ident(ident) => AssignableSegment::Name(ident.to_owned()),
                        token => {
                            return Err(unexpected!(
                                self.line(),
                                token.to_owned(),
                                ["identifier name"],
                                "Identifier name must follow `.`" // Terrible explanation again
                            ));
                        }
                    }
                }

                Token::Keyword(Keyword::BracketLeft) => {
                    self.next();

                    let start = self.expect_expr()?;
                    let mut end = None;

                    if self.peek() == &Token::Keyword(Keyword::Comma) {
                        self.next();
                        end = Some(self.expect_expr()?);
                    }

                    self.expect_keyword(
                        Keyword::BracketRight,
                        "Subscript must be delimited by `]`",
                    )?;

                    match end {
                        None => AssignableSegment::Index(start),
                        Some(end) => AssignableSegment::Slice(start, end),
                    }
                }

                _ => break,
            };
            parts.push(part);
        }

        Ok(AssignableChain::from(origin, parts))
    }

    //TODO: Refactor this ideally. it is not very nice
    fn try_assign_statement(&mut self) -> Option<Result<AssignStatement, ParseError>> {
        // We don't yet know if it is an assignment statement or not
        let index = self.get_index();

        let Ok(origin) = self.expect_ident() else {
            self.set_index(index);
            return None;
        };

        let Ok(left_value) = self.expect_assignable_chain(origin) else {
            self.set_index(index);
            return None;
        };

        // Just check it
        if self.next() != &Token::Keyword(Keyword::SingleEqual) {
            self.set_index(index);
            return None;
        };

        // Ok now from here we know it MUST be an assignment

        let value = match self.expect_expr() {
            Ok(expr) => expr,
            Err(err) => return Some(Err(err)),
        };

        Some(Ok(AssignStatement {
            name: left_value,
            value,
        }))
    }
}
