use std::{collections::HashMap, fmt, io::Write};

use crate::{
    debug::OUT,
    error::InterpretError,
    lex::{Literal, Number},
    lex_tokens,
    parse::{
        AccessibleSegment, AssignStatement, AssignableSegment, Chain, ChainOrigin, Expr,
        FuncParams, Ident, LetStatement, ModuleName, Statement, StatementBody,
    },
    parse_module_body, ParseError,
};

#[derive(Debug)]
struct Context {
    scope_chain: ScopeChain,
    modules: ModuleMap,
    //TODO: Count references
    values: HashMap<ValueID, Value>,
}

type ScopeChain = Vec<Scope>;

#[derive(Debug, Default)]
struct Scope {
    names: HashMap<String, ValueID>,
    //TODO: modules
    //TODO: templates
}

type ValueID = usize;

#[derive(Debug)]
enum ValueMaybeFree {
    Free(Value),
    Ref(ValueID),
}

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

type ModuleMap = HashMap<ModuleName, ModuleValues>;

type ModuleValues = HashMap<Ident, Value>;

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Nil => Value::Nil.into(),
            Literal::Bool(bool) => Value::Bool(bool).into(),
            Literal::Number(number) => Value::Number(number).into(),
            Literal::Char(char) => Value::Char(char).into(),
            Literal::String(string) => Value::String(string).into(),
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
    //TODO: On insert KEY, clone value ALWAYS.
    //TODO: On insert VALUE, insert to scope/ctx, and reference (unless atom).
    entries: HashMap<Value, ValueID>,
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
        values: HashMap::new(),
    };

    println!("\x1b[1mRemember to follow real stdout with `tail -f target/out.fifo`\x1b[0m");
    print_out!("\x1bc\x1b[0m");

    interpret_file(&mut context, file)?;

    Ok(())
}

fn insert_to_heap(ctx: &mut Context, value: Value) -> ValueID {
    let id = ctx.values.len();
    ctx.values.insert(id, value);
    id
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
                let value = match interpret_expr(ctx, value)? {
                    ValueMaybeFree::Free(value) => insert_to_heap(ctx, value),
                    ValueMaybeFree::Ref(value_ref) => value_ref,
                };

                println!("LET {:?} = {:?}", name, value);
                let scope = ctx
                    .scope_chain
                    .last_mut()
                    .expect("Should exist because just pushed");
                scope.names.insert(name, value);
            }

            Statement::Assign(AssignStatement { name, value }) => {
                let Some(lvalue_id) = lookup_variable_chain(ctx, &name) else {
                    return Err(InterpretError::Run(format!(
                        "Variable not defined: `{name:?}`"
                    )));
                };
                let value = interpret_expr(ctx, value)?;
                modify_value(ctx, lvalue_id, value);
            }

            _ => {
                println!("(todo statement) {:?}", statement);
            }
        }
    }

    ctx.scope_chain
        .pop()
        .expect("Should exist because just pushed");

    let module = ModuleValues::new();

    Ok(module)
}

fn interpret_expr<'a>(ctx: &'a mut Context, expr: Expr) -> Result<ValueMaybeFree, InterpretError> {
    match expr {
        Expr::Group(inner) => interpret_expr(ctx, *inner),

        Expr::Literal(literal) => Ok(ValueMaybeFree::Free(literal.into())),

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
                for (i, arg) in args.args.into_iter().enumerate() {
                    if i > 0 {
                        print_out!(" ");
                    }
                    let value = match interpret_expr(ctx, arg)? {
                        ValueMaybeFree::Free(value) => &value,
                        ValueMaybeFree::Ref(value_ref) => access_value(ctx, value_ref),
                    };
                    print_out!("{}", value);
                }
                print_out!("\n");
                if args.spread_arg.is_some() {
                    unimplemented!("spread args");
                }
                return Ok(ValueMaybeFree::Free(Value::Nil));
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
                        value_index(ctx, head, Expr::Literal(Literal::String(name)))?
                    }
                    AccessibleSegment::Index(index) => value_index(ctx, head, index)?,
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
            Ok(ValueMaybeFree::Ref(head))
        }

        _ => {
            println!("(todo expr) {:?}", expr);
            Ok(ValueMaybeFree::Free(Value::Nil))
        }
    }
}

fn maybe_access_value(ctx: &mut Context, value: ValueMaybeFree) -> &Value {
    match value {
        ValueMaybeFree::Free(value) => &value,
        ValueMaybeFree::Ref(value_ref) => access_value(ctx, value_ref),
    }
}

fn access_value(ctx: &mut Context, id: ValueID) -> &Value {
    ctx.values
        .get(&id)
        .expect("Value should exist in heap with that ID")
}

fn modify_value(ctx: &mut Context, id: ValueID, new: ValueMaybeFree) {
    let value = ctx
        .values
        .get_mut(&id)
        .expect("Value should exist in heap with that ID");
    match value {
        ValueMaybeFree::Free(value) => &value,
        ValueMaybeFree::Ref(value_ref) => access_value(ctx, value_ref),
    }
    *value = maybe_access_value(ctx, new);
}

//TODO: WILL NOT MUTATE LVALUES
fn value_index(ctx: &mut Context, _id: ValueID, index: Expr) -> Result<ValueID, InterpretError> {
    let index = interpret_expr(ctx, index)?;
    todo!("access name[{index:?}]");
}

fn lookup_variable(ctx: &Context, name: &str) -> Option<ValueID> {
    for scope in ctx.scope_chain.iter().rev() {
        if let Some(value) = scope.names.get(name) {
            println!("found: {:?}", value);
            return Some(*value);
        };
    }
    return None;
}

fn lookup_variable_chain(ctx: &Context, name: &Chain<AssignableSegment>) -> Option<ValueID> {
    let origin = match &name.origin {
        ChainOrigin::Name(name) => name,
        ChainOrigin::Self_ => unimplemented!("assign to `self`"),
    };
    let origin = lookup_variable(ctx, &origin)?;
    if !name.chain.is_empty() {
        unimplemented!("assignable chain");
    }
    return Some(origin);
}

fn lex_and_parse(file: &str) -> Result<StatementBody, ParseError> {
    let tokens = lex_tokens(file)?;
    let body = parse_module_body(tokens)?;
    Ok(body)
}
