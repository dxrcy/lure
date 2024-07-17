use std::{collections::HashMap, fmt};

use crate::{
    error::InterpretError,
    lex::{Literal, Number},
    lex_tokens,
    parse::{
        AccessibleSegment, ChainOrigin, Expr, FuncParams, Ident, LetStatement, ModuleName,
        Statement, StatementBody,
    },
    parse_module_body, ParseError,
};

#[derive(Debug)]
struct Context {
    scope_chain: ScopeChain,
    modules: ModuleMap,
}

type ScopeChain = Vec<Scope>;

#[derive(Debug, Default)]
struct Scope {
    //TODO: Count references
    values: HashMap<ValueHeapID, ValueHeap>,
    names: HashMap<String, ValueRef>,
    //TODO: modules
    //TODO: templates
}

type ValueHeapID = usize;

#[derive(Debug)]
enum Value {
    Atom(ValueAtom),
    Heap(ValueHeap),
}

#[derive(Clone, Copy, Debug)]
enum ValueRef {
    Atom(ValueAtom),
    Reference(ValueHeapID),
}

#[derive(Clone, Copy, Debug)]
enum ValueAtom {
    Nil,
    Bool(bool),
    Number(Number),
    Char(char),
}

#[derive(Debug)]
enum ValueHeap {
    String(String),
    Table(Table),
    Func(Func),
}

type ModuleMap = HashMap<ModuleName, ModuleValues>;

type ModuleValues = HashMap<Ident, Value>;

impl From<ValueAtom> for Value {
    fn from(value: ValueAtom) -> Self {
        Self::Atom(value)
    }
}
impl From<ValueHeap> for Value {
    fn from(value: ValueHeap) -> Self {
        Self::Heap(value)
    }
}

impl From<ValueAtom> for ValueRef {
    fn from(value: ValueAtom) -> Self {
        Self::Atom(value.into())
    }
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Nil => ValueAtom::Nil.into(),
            Literal::Bool(bool) => ValueAtom::Bool(bool).into(),
            Literal::Number(number) => ValueAtom::Number(number).into(),
            Literal::Char(char) => ValueAtom::Char(char).into(),
            Literal::String(string) => ValueHeap::String(string).into(),
        }
    }
}

// impl<T> From<T> for ValueRef
// where
//     T: Into<Value>,
// {
//     fn from(value: T) -> Self {
//         ValueRef::Owned(value.into())
//     }
// }

// impl Clone for Value {
//     fn clone(&self) -> Self {
//         match self {
//             Value::Nil => Value::Nil,
//             Value::Bool(bool) => Value::Bool(*bool),
//             Value::Number(number) => Value::Number(*number),
//             Value::Char(char) => Value::Char(*char),
//             _ => panic!("Tried to clone non-copy `Value`. This is wrong I think?"),
//         }
//     }
// }

// impl ValueRef {
//     pub fn copy_or_reference(&mut self) -> Self {
//         match self {
//             // Copy reference
//             ValueRef::Reference(reference) => ValueRef::Reference(*reference),
//             ValueRef::Owned(value) => {
//                 let value = match value {
//                     // Copy atomic value
//                     Value::Nil => Value::Nil,
//                     Value::Bool(bool) => Value::Bool(*bool),
//                     Value::Number(number) => Value::Number(*number),
//                     Value::Char(char) => Value::Char(*char),
//                     // Take reference to owned value
//                     _ => {
//                         return ValueRef::Reference(value);
//                     }
//                 };
//                 ValueRef::Owned(value)
//             }
//         }
//     }
// }

impl fmt::Display for ValueAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "<NIL>"),
            Self::Bool(true) => write!(f, "<true>"),
            Self::Bool(false) => write!(f, "<false>"),
            Self::Number(number) => write!(f, "{}", number),
            Self::Char(char) => write!(f, "'{}'", char),
        }
    }
}
impl fmt::Display for ValueHeap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => write!(f, r#""{}""#, string),
            Self::Table(_) => write!(f, "<TABLE>"),
            Self::Func(_) => write!(f, "<FUNC>"),
        }
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom(value) => write!(f, "{}", value),
            Self::Heap(value) => write!(f, "{}", value),
        }
    }
}
impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueRef::Atom(value) => write!(f, "(atom){}", value),
            ValueRef::Reference(reference) => write!(f, "(ref){}", reference),
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
    let scope_chain = ScopeChain::new();
    let modules = ModuleMap::new();
    let mut context = Context {
        scope_chain,
        modules,
    };

    interpret_file(&mut context, file)?;

    Ok(())
}

fn interpret_file(ctx: &mut Context, file: &str) -> Result<ModuleValues, InterpretError> {
    let body = lex_and_parse(file).map_err(InterpretError::Parse)?;

    let scope = Scope::default();
    ctx.scope_chain.push(scope);

    for statement in body {
        match statement {
            Statement::Expr(expr) => {
                _ = interpret_expr(ctx, expr)?;
            }

            Statement::Let(LetStatement { names, value }) => {
                if !names.rest.is_empty() {
                    unimplemented!("multiple names in `let` statement");
                }
                let name = names.first;
                let value = interpret_expr(ctx, value)?;

                println!("LET {:?} = {:?}", name, value);
                let scope = ctx
                    .scope_chain
                    .last_mut()
                    .expect("Should exist because just pushed");
                scope.names.insert(name, value);
            }

            _ => {
                println!("(todo statement) {:?}", statement);
            }
        }
    }

    let module = ModuleValues::new();

    Ok(module)
}

fn insert_to_heap(ctx: &mut Context, value: ValueHeap) -> ValueRef {
    let scope = ctx.scope_chain.last_mut().expect("Must have >0 scopes");
    let id = scope.values.len();
    scope.values.insert(id, value);
    ValueRef::Reference(id)
}

fn interpret_expr<'a>(ctx: &'a mut Context, expr: Expr) -> Result<ValueRef, InterpretError> {
    match expr {
        Expr::Group(inner) => interpret_expr(ctx, *inner),

        Expr::Literal(literal) => {
            let value: Value = literal.into();
            match value {
                Value::Atom(atom) => Ok(atom.into()),
                Value::Heap(heap) => {
                    let reference = insert_to_heap(ctx, heap);
                    Ok(reference)
                }
            }
        }

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
                    let value = interpret_expr(ctx, arg)?;
                    println!("\x1b[36m{}\x1b[0m", value);
                }
                if args.spread_arg.is_some() {
                    unimplemented!("spread args");
                }
                return Ok(ValueAtom::Nil.into());
            }

            let ChainOrigin::Name(origin) = chain.origin else {
                todo!("access self");
            };

            let Some(mut head) = lookup_variable(ctx, &origin) else {
                return Err(InterpretError::Run(format!(
                    "Undefined variable `{}`",
                    origin
                )));
            };
            println!("ORIGIN: {:?}", head);

            for segment in chain.chain {
                head = match segment {
                    AccessibleSegment::Name(name) => {
                        todo!("access name.{name}");
                    }
                    AccessibleSegment::Index(index) => {
                        let index = interpret_expr(ctx, index)?;
                        todo!("access name[{index:?}]");
                    }
                    AccessibleSegment::Slice(start, end) => {
                        let start = interpret_expr(ctx, start)?;
                        let end = interpret_expr(ctx, end)?;
                        todo!("access name[{start:?}, {end:?}]");
                    }
                    AccessibleSegment::Call(args) => {
                        todo!("access name(...)");
                    }
                };
            }

            println!("chain evaluates to: {:?}", head);
            Ok(head)
        }

        _ => {
            println!("(todo expr) {:?}", expr);
            Ok(ValueAtom::Nil.into())
        }
    }
}

fn lookup_variable(ctx: &Context, name: &str) -> Option<ValueRef> {
    for scope in ctx.scope_chain.iter().rev() {
        if let Some(value) = scope.names.get(name) {
            println!("found: {:?}", value);
            return Some(*value);
        };
    }
    return None;
}

fn lex_and_parse(file: &str) -> Result<StatementBody, ParseError> {
    let tokens = lex_tokens(file)?;
    let body = parse_module_body(tokens)?;
    Ok(body)
}
