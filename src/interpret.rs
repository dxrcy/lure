use std::collections::HashMap;

use crate::{
    error::InterpretError,
    lex_tokens,
    parse::{Module, ModuleName, StatementBody},
    parse_module_body, ParseError,
};

type ModuleMap = HashMap<ModuleName, Module>;

pub fn interpret_main(file: &str) -> Result<(), InterpretError> {
    let mut modules = ModuleMap::new();
    interpret_file(&mut modules, file)?;
    Ok(())
}

fn interpret_file(modules: &mut ModuleMap, file: &str) -> Result<(), InterpretError> {
    let module = lex_and_parse(file).map_err(|err| InterpretError::Parse(err))?;

    println!("{:#?}", module);

    Ok(())
}

fn lex_and_parse(file: &str) -> Result<StatementBody, ParseError> {
    let tokens = lex_tokens(file)?;

    let module = parse_module_body(tokens)?;

    Ok(module)
}
