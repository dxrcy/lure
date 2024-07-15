use std::{collections::HashMap, fmt};

use crate::{
    error::InterpretError,
    lex::{Literal, Number},
    lex_tokens,
    parse::{
        AccessibleSegment, ChainOrigin, Expr, FuncParams, FuncStatement, Ident, LetStatement,
        Module, ModuleName, Statement, StatementBody,
    },
    parse_module_body, ParseError,
};

type ModuleMap = HashMap<ModuleName, ModuleValues>;

type ModuleValues = HashMap<Ident, Value>;

#[derive(Debug)]
enum Value {
    Nil,
    Bool(bool),
    Number(Number),
    Char(char),
    String(String),
    Table(Table),
    Func(Func),
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Nil => Value::Nil,
            Literal::Bool(bool) => Value::Bool(bool),
            Literal::Number(number) => Value::Number(number),
            Literal::Char(char) => Value::Char(char),
            Literal::String(string) => Value::String(string),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "<NIL>"),
            Self::Bool(true) => write!(f, "<true>"),
            Self::Bool(false) => write!(f, "<false>"),
            Self::Number(number) => write!(f, "{}", number),
            Self::Char(char) => write!(f, "'{}'", char),
            Self::String(string) => write!(f, r#""{}""#, string),
            Self::Table(_) => write!(f, "<TABLE>"),
            Self::Func(_) => write!(f, "<FUNC>"),
        }
    }
}

#[derive(Debug)]
struct Table {
    entries: HashMap<Value, Value>,
}

#[derive(Debug)]
struct Func {
    name: Ident,
    self_param: bool,
    params: FuncParams,
    body: StatementBody,
}

pub fn interpret_main(file: &str) -> Result<(), InterpretError> {
    let mut modules = ModuleMap::new();
    interpret_file(&mut modules, file)?;
    Ok(())
}

fn interpret_file(modules: &mut ModuleMap, file: &str) -> Result<ModuleValues, InterpretError> {
    let body = lex_and_parse(file).map_err(InterpretError::Parse)?;

    let mut module = ModuleValues::new();

    for statement in body {
        match statement {
            Statement::Expr(expr) => {
                _ = interpret_expr(modules, expr)?;
            }

            Statement::Let(LetStatement { names, value }) => {
                let value = interpret_expr(modules, value)?;
                println!("{:?}", value);
            }

            _ => {
                println!("(todo statement) {:?}", statement);
            }
        }
    }

    Ok(module)
}

fn interpret_expr(modules: &mut ModuleMap, expr: Expr) -> Result<Value, InterpretError> {
    match expr {
        Expr::Group(inner) => interpret_expr(modules, *inner),

        Expr::Literal(literal) => Ok(literal.into()),

        Expr::Chain(chain) => {
            println!("CHAIN");
            // debugging
            if chain.origin == ChainOrigin::Name("print".to_string()) {
                let mut chain = chain.chain.into_iter();
                let Some(AccessibleSegment::Call(args)) =
                    chain.next().filter(|_| chain.next().is_none())
                else {
                    panic!("cannot access `print` like that");
                };
                for arg in args.args {
                    let value = interpret_expr(modules, arg)?;
                    println!("\x1b[36m{}\x1b[0m", value);
                }
                if args.spread_arg.is_some() {
                    unimplemented!("spread args");
                }
                return Ok(Value::Nil);
            }

            let ChainOrigin::Name(mut origin) = chain.origin else {
                todo!("access self");
            };
            for segment in chain.chain {
                origin = match segment {
                    AccessibleSegment::Name(name) => {
                        todo!("access name.{name}");
                    }
                    AccessibleSegment::Index(index) => {
                        let index = interpret_expr(modules, index)?;
                        todo!("access name[{index:?}]");
                    }
                    AccessibleSegment::Slice(start, end) => {
                        let start = interpret_expr(modules, start)?;
                        let end = interpret_expr(modules, end)?;
                        todo!("access name[{start:?}, {end:?}]");
                    }
                    AccessibleSegment::Call(args) => {
                        todo!("access name(...)");
                    }
                };
            }
            println!("chain evaluates to: {:?}", origin);
            todo!();
        }

        _ => {
            println!("(todo expr) {:?}", expr);
            Ok(Value::Nil)
        }
    }
}

fn lex_and_parse(file: &str) -> Result<StatementBody, ParseError> {
    let tokens = lex_tokens(file)?;
    let body = parse_module_body(tokens)?;
    Ok(body)
}
