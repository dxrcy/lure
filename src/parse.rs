use crate::{
    lex::{Keyword, Literal, Token, TokenRef},
    ParseError, ParseErrorKind, TokenIter,
};

#[derive(Debug, PartialEq)]
pub struct SourceModule {
    module: StatementList,
    linked: Vec<Module>,
}

#[derive(Debug, PartialEq)]
struct Module {
    name: ModuleName,
    body: StatementList,
}

type ModuleName = String;

type StatementList = Vec<Statement>;

#[derive(Debug, PartialEq)]
enum Statement {
    Expr(Expr),
    Func(FuncStatement),
    Let(Let),
    Assign(Assign),
    If(IfStatement),
    While(While),
    For(For),
    Module(Module),
    Template(Template),
    Return(Return),
    Break,
    Continue,
}

#[derive(Debug, PartialEq)]
struct Template {
    name: Ident,
    items: Vec<TemplateItem>,
}

#[derive(Debug, PartialEq)]
enum TemplateItem {
    Key {
        name: Ident,
        default_value: Option<Expr>,
    },
    Func(FuncStatement),
}

#[derive(Debug, PartialEq)]
struct FuncStatement {
    name: Ident,
    params: DeclareParams,
    body: StatementList,
}

#[derive(Debug, PartialEq)]
struct FuncExpr {
    params: DeclareParams,
    body: StatementList,
}

type Ident = String;

type LValue = FirstRest<Ident, LValuePart>;

#[derive(Debug, PartialEq)]
enum LValuePart {
    Ident(Ident),
    Index(Expr),
    Slice(Expr, Expr),
}

//TODO: Get better name !!!
type ExprPath = FirstRest<Ident, ExprPathPart>;

#[derive(Debug, PartialEq)]
enum ExprPathPart {
    Name { name: Ident },
    Call { params: CallParams },
    Index { index: Expr },
    Slice { start: Expr, end: Expr },
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
    names: Plural<Ident>,
    value: Expr,
}

#[derive(Debug, PartialEq)]
struct Return {
    values: Plural<Expr>,
}

#[derive(Debug, PartialEq)]
struct Assign {
    lvalue: LValue,
    value: Expr,
}

#[derive(Debug, PartialEq)]
enum Expr {
    Group(Box<Expr>),
    Literal(Literal),
    Path(Box<ExprPath>),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    If(IfExpr),
    Table(Table),
    TemplateTable(LValue, Table),
    Func(FuncExpr),
}

#[derive(Debug, PartialEq)]
enum UnaryOp {
    Not,
    Negative,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
struct For {
    key_name: Ident,
    value_name: Option<Ident>,
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
    spread: Option<LValue>,
}

#[derive(Debug, PartialEq)]
struct TableItem {
    key: Literal,
    value: Box<Expr>,
}

//TODO: Get better name !!!
#[derive(Debug, PartialEq)]
struct FirstRest<T, U> {
    first: T,
    rest: Vec<U>,
}

/// >=1 items
type Plural<T> = FirstRest<T, T>;

impl<T, U> FirstRest<T, U> {
    pub fn from(first: T, rest: Vec<U>) -> Self {
        Self { first, rest }
    }
}

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

pub fn parse_source_module(tokens: Vec<TokenRef>) -> Result<SourceModule, ParseError> {
    let mut tokens = TokenIter::from(tokens);

    let module = tokens.expect_body()?;

    // Does this need to be here ?
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
            token => Err(unexpected!(self.line(), token.to_owned(), [Token::Eof])),
        }
    }

    fn expect_keyword_reason(
        &mut self,
        keyword: Keyword,
        reason: &'static str,
    ) -> Result<(), ParseError> {
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

    fn expect_end(&mut self) -> Result<(), ParseError> {
        self.expect_keyword_reason(Keyword::End, "Code block must be closed with `end` keyword")?;
        Ok(())
    }

    fn expect_body(&mut self) -> Result<StatementList, ParseError> {
        let mut statements = Vec::new();

        loop {
            let statement = match self.peek() {
                Token::Eof
                | Token::Keyword(Keyword::End | Keyword::Elif | Keyword::Else | Keyword::Case) => {
                    println!("(close)");
                    break;
                }

                Token::Keyword(Keyword::Let) => {
                    self.next();
                    self.expect_let_statement()?
                }
                Token::Keyword(Keyword::Func) => {
                    self.next();
                    self.expect_func_statement()?
                }
                Token::Keyword(Keyword::If) => {
                    self.next();
                    self.expect_if_statement()?
                }
                Token::Keyword(Keyword::For) => {
                    self.next();
                    self.expect_for_statement()?
                }
                Token::Keyword(Keyword::While) => {
                    self.next();
                    self.expect_while_statement()?
                }
                Token::Keyword(Keyword::Module) => {
                    self.next();
                    self.expect_module_statement()?
                }
                Token::Keyword(Keyword::Template) => {
                    self.next();
                    self.expect_template_statement()?
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
                    self.expect_return_statement()?
                }

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

    //TODO: Maybe have all of these return their actual type, not just
    // `Statement`. Then wrap in statement after 'expecting'.

    fn expect_return_statement(&mut self) -> Result<Statement, ParseError> {
        let value_first = self.expect_expr()?;

        let mut value_rest = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Comma) => {
                    self.next();
                }
                _ => break,
            }
            let value = self.expect_expr()?;
            value_rest.push(value);
        }

        let values = Plural::from(value_first, value_rest);

        Ok(Statement::Return(Return { values }))
    }

    fn expect_let_statement(&mut self) -> Result<Statement, ParseError> {
        let name_first = self.expect_ident()?;

        let mut name_rest = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Comma) => {
                    self.next();
                }
                _ => break,
            }
            let name = self.expect_ident()?;
            name_rest.push(name);
        }

        let names = Plural::from(name_first, name_rest);

        self.expect_keyword_reason(
            Keyword::SingleEqual,
            "`let` declaration must include `=` to instantiate variable",
        )?;

        let value = self.expect_expr()?;

        Ok(Statement::Let(Let { names, value }))
    }

    fn expect_module_statement(&mut self) -> Result<Statement, ParseError> {
        let name = self.expect_ident()?;

        let body = self.expect_body()?;

        self.expect_end()?;

        Ok(Statement::Module(Module { name, body }))
    }

    fn expect_template_statement(&mut self) -> Result<Statement, ParseError> {
        let name = self.expect_ident()?;

        self.expect_keyword_reason(Keyword::BraceLeft, "`template` statement must include `{`")?;

        let mut items = Vec::new();
        loop {
            match self.next() {
                Token::Keyword(Keyword::BraceRight) => {
                    break;
                }

                Token::Keyword(Keyword::Func) => {
                    let func = self.expect_func_statement()?;
                    let Statement::Func(func) = func else {
                        panic!("Statement should be `Statement::Func`");
                    };
                    items.push(TemplateItem::Func(func));
                }

                Token::Ident(ident) => {
                    let name = ident.to_owned();
                    let default_value = match self.next() {
                        Token::Keyword(Keyword::Comma) => {
                            None
                        }

                        Token::Keyword(Keyword::SingleEqual) => {
                            let value = self.expect_expr()?;
                            self.expect_keyword_reason(
                                Keyword::Comma,
                                "Template key with default value must be followed with `,`",
                            )?;
                            Some(value)
                        }

                        token => {
                            return Err(unexpected!(
                                self.line(),
                                token.to_owned(),
                                [Keyword::Comma, Keyword::SingleEqual],
                                "Template key must be followed with `,` or `=` to declare a default value"
                            ))
                        }
                    };
                    items.push(TemplateItem::Key {
                        name,
                        default_value,
                    });
                }

                token => {
                    return Err(unexpected!(
                        self.line(),
                        token.to_owned(),
                        ["template item name", Keyword::Func, Keyword::BraceRight],
                        "Templates can only contain keys and functions",
                    ))
                }
            }
        }

        Ok(Statement::Template(Template { name, items }))
    }

    fn expect_func_statement(&mut self) -> Result<Statement, ParseError> {
        let name = self.expect_ident()?;

        self.expect_keyword_reason(
            Keyword::ParenLeft,
            "`func` statement must include parameter list between parentheses",
        )?;

        let params = self.expect_param_list()?;

        let body = self.expect_body()?;

        self.expect_end()?;

        Ok(Statement::Func(FuncStatement { name, params, body }))
    }

    fn expect_func_expr(&mut self) -> Result<Expr, ParseError> {
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

        let params = self.expect_param_list()?;

        let body = self.expect_body()?;

        self.expect_end()?;

        Ok(Expr::Func(FuncExpr { params, body }))
    }

    fn expect_param_list(&mut self) -> Result<DeclareParams, ParseError> {
        let mut params = DeclareParams::default();
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
                    params.rest = Some(ident);
                    self.expect_keyword_reason(
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
                        "Parameter list must end with `)`"
                    ));
                }
            }
        }

        Ok(params)
    }

    fn expect_if(&mut self) -> Result<IfStatement, ParseError> {
        let condition = self.expect_expr()?;
        self.expect_keyword_reason(
            Keyword::Then,
            "`if` condition must be followed with `then` keyword",
        )?;
        let body = self.expect_body()?;
        let if_branch = Box::new(IfBranch { condition, body });

        let mut elif_branches = Vec::new();
        loop {
            match self.peek() {
                Token::Keyword(Keyword::Elif) => {
                    self.next();
                    let condition = self.expect_expr()?;
                    self.expect_keyword_reason(
                        Keyword::Then,
                        "`elif` condition must be followed with `then` keyword",
                    )?;
                    let body = self.expect_body()?;
                    elif_branches.push(IfBranch { condition, body });
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

        self.expect_end()?;

        Ok(IfStatement {
            if_branch,
            elif_branches,
            else_branch,
        })
    }

    fn expect_if_statement(&mut self) -> Result<Statement, ParseError> {
        let if_statement = self.expect_if()?;
        Ok(Statement::If(if_statement))
    }

    fn expect_if_expr(&mut self) -> Result<Expr, ParseError> {
        let if_statement = self.expect_if()?;

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

        Ok(Expr::If(IfExpr {
            if_branch,
            elif_branches,
            else_branch,
        }))
    }

    fn expect_for_statement(&mut self) -> Result<Statement, ParseError> {
        let key_name = self.expect_ident()?;

        let value_name = match self.peek() {
            Token::Keyword(Keyword::Comma) => {
                self.next();
                Some(self.expect_ident()?)
            }
            _ => None,
        };

        self.expect_keyword_reason(Keyword::In, "`for` statement must include `in` keyword")?;

        let from = self.expect_expr()?;

        let source = match self.next() {
            Token::Keyword(Keyword::To) => {
                let to = self.expect_expr()?;
                self.expect_keyword_reason(
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

        self.expect_end()?;

        Ok(Statement::For(For {
            key_name,
            value_name,
            source,
            body,
        }))
    }

    fn expect_while_statement(&mut self) -> Result<Statement, ParseError> {
        let condition = self.expect_expr()?;

        self.expect_keyword_reason(
            Keyword::Do,
            "`while` statement must include `do` keyword following condition expression",
        )?;

        let body = self.expect_body()?;

        self.expect_end()?;

        Ok(Statement::While(While {
            branch: IfBranch { condition, body },
        }))
    }

    fn expect_expr_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expect_expr()?;
        Ok(Statement::Expr(expr))
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

        let right = self.expect_expr()?;

        match right {
            // Swap order of binary operations
            // UNLESS the inner (rightmost) operation has higher precedence
            // Note that equal-precendence operations will always swap, to
            // maintain default left-to-right order
            Expr::BinaryOp(right_op, middle, right) if right_op >= left_op => {
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
                self.expect_keyword_reason(
                    Keyword::ParenRight,
                    "Unmatched parenthesis in expression group",
                )?;
                Ok(Expr::Group(Box::new(expr)))
            }

            Token::Literal(literal) => {
                return Ok(Expr::Literal(literal.to_owned()));
            }

            Token::Ident(ident) => {
                //TODO: Save index
                let origin = ident.to_owned();

                let mut rest = Vec::new();
                loop {
                    let part = match self.peek() {
                        Token::Keyword(Keyword::Dot) => {
                            self.next();
                            match self.next() {
                                Token::Ident(ident) => {
                                    let name = ident.to_owned();
                                    ExprPathPart::Name { name }
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
                                        ["table key or item name"],
                                        reason,
                                    ));
                                }
                            }
                        }

                        Token::Keyword(Keyword::BracketLeft) => {
                            self.next();
                            let index = self.expect_expr()?;
                            match self.next() {
                                Token::Keyword(Keyword::BracketRight) => {
                                    ExprPathPart::Index { index }
                                }
                                Token::Keyword(Keyword::Comma) => {
                                    let start = index;
                                    let end = self.expect_expr()?;
                                    self.expect_keyword_reason(
                                        Keyword::BracketRight,
                                        "Subscript expression should end with `]`",
                                    )?;
                                    ExprPathPart::Slice { start, end }
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
                                        self.expect_keyword_reason(
                                            Keyword::ParenRight,
                                            "Spread argument must be last argument",
                                        )?;
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

                            ExprPathPart::Call { params }
                        }

                        _ => {
                            break;
                        }
                    };
                    rest.push(part);
                }

                return Ok(Expr::Path(Box::new(ExprPath::from(origin, rest))));
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
                let table = self.expect_table()?;
                return Ok(table);
            }

            Token::Keyword(Keyword::As) => {
                let ident = self.expect_ident()?;
                let name = self.expect_lvalue(ident)?;
                self.expect_keyword_reason(
                    Keyword::BraceLeft,
                    "Template name must be followed by table",
                )?;
                let table = self.expect_table()?;
                let Expr::Table(table) = table else {
                    panic!("Expression should be `Expr::Table`");
                };
                return Ok(Expr::TemplateTable(name, table));
            }

            Token::Keyword(Keyword::If) => {
                return self.expect_if_expr();
            }

            Token::Keyword(Keyword::Func) => {
                return self.expect_func_expr();
            }

            token => {
                let token = token.to_owned();
                let reason = match token {
                    Token::Keyword(Keyword::Let) => "Cannot use `let` declaration as an expression",
                    Token::Keyword(Keyword::For) => "Cannot use `for` statement as an expression",
                    Token::Keyword(Keyword::While) => {
                        "Cannot use `while` statement as an expression"
                    }
                    Token::Keyword(Keyword::Module) => "Cannot use module as an expression",
                    Token::Keyword(Keyword::Template) => "Cannot use template as an expression",
                    // Generic reason
                    _ => "Cannot parse the tokens as an expression",
                };
                return Err(unexpected!(self.line(), token, ["expression"], reason));
            }
        }
    }

    fn expect_table(&mut self) -> Result<Expr, ParseError> {
        let mut table = Table::default();
        let mut implicit_key = 0;

        // Empty table
        if self.peek() == &Token::Keyword(Keyword::BraceRight) {
            self.next();
            return Ok(Expr::Table(table));
        }

        loop {
            let index = self.get_index();

            if self.peek() == &Token::Keyword(Keyword::Spread) {
                self.next();
                let ident = self.expect_ident()?;
                let lvalue = self.expect_lvalue(ident)?;
                table.spread = Some(lvalue);
                self.expect_keyword_reason(Keyword::BraceRight, "Spread key must be last key")?;
                break;
            }

            let key_token = self.next().to_owned();
            let key = match self.next() {
                Token::Keyword(Keyword::SingleEqual) => match key_token {
                    Token::Literal(literal) => literal.to_owned(),
                    Token::Ident(ident) => Literal::String(ident.to_owned()),

                    _ => {
                        return Err(unexpected!(
                            self.line(),
                            Keyword::SingleEqual,
                            ["literal or identifier with `=`", "expression"],
                            "Table key must be a literal or an identifier followed by `=`",
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

            let value = Box::new(self.expect_expr()?);

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
                    return Err(unexpected!(
                        self.line(),
                        token.to_owned(),
                        [Keyword::Comma, Keyword::BraceRight],
                        "Table items must be separated with commas",
                    ))
                }
            }
        }

        Ok(Expr::Table(table))
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

                    self.expect_keyword_reason(
                        Keyword::BracketRight,
                        "Subscript must be delimited by `]`",
                    )?;

                    match end {
                        None => LValuePart::Index(start),
                        Some(end) => LValuePart::Slice(start, end),
                    }
                }

                _ => break,
            };
            parts.push(part);
        }

        Ok(LValue::from(origin, parts))
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

        Some(Ok(Statement::Assign(Assign { lvalue, value })))
    }
}
