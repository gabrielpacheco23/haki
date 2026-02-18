use crate::compiler::{compile, optimize_ast};
use crate::env::{Env, standard_env};
use crate::evaluate::eval;
use crate::expr::LispExp;
use crate::helpers::{apply_macro, apply_procedure, expand_macros, expand_quasiquote};
use crate::parser::{read_from_tokens, tokenize};
use crate::repl::repl;
use crate::stdlib::load_stdlib;
use crate::vm::*;

use std::rc::Rc;

mod compiler;
mod env;
mod evaluate;
mod expr;
mod helpers;
mod parser;
mod repl;
mod stdlib;
mod tests;
mod vm;

#[derive(Copy, Clone, PartialEq)]
pub enum ExecMode {
    Normal,
    Dump,
}

fn run_script(path: &str, env: &mut Env) -> Result<LispExp, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Error reading the file '{}': {}", path, e))?;

    run_source(&content, env, ExecMode::Normal)
}

pub fn run_source(source: &str, env: &mut Env, mode: ExecMode) -> Result<LispExp, String> {
    let tokens = tokenize(source);
    let mut tokens_iter = tokens.into_iter().peekable();
    let mut vm = Vm::new();
    let mut last_result = LispExp::Void;

    while tokens_iter.peek().is_some() {
        let raw_ast = read_from_tokens(&mut tokens_iter)?;

        let expanded_ast = expand_macros(raw_ast, env)?;

        let optimized_ast = optimize_ast(expanded_ast);

        if mode == ExecMode::Dump {
            println!("Expanded AST: {}", optimized_ast);
        }

        if optimized_ast != LispExp::Void {
            let mut chunk = Chunk::new();

            compile(&optimized_ast, &mut chunk, false)?;
            chunk.code.push(OpCode::Return);

            if mode == ExecMode::Dump {
                disassemble_chunk(&chunk, "Block");
            } else {
                last_result = vm.execute(Rc::new(chunk), env.clone())?;
            }
        }
    }

    Ok(last_result)
}

fn main() {
    let mut global_env = standard_env();

    let macros_src = include_str!("../std/macros.lsp");
    let lib_src = include_str!("../std/lib.lsp");

    load_stdlib(macros_src, &mut global_env);
    load_stdlib(lib_src, &mut global_env);

    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        let file_path = &args[1];
        match run_script(file_path, &mut global_env) {
            Ok(_) => std::process::exit(0),
            Err(e) => {
                eprintln!("Error executing the script: {}", e);
                std::process::exit(1);
            }
        }
    } else {
        repl(global_env)
    }
}
