use crate::compiler::{CompilerState, compile, optimize_ast};
use crate::env::{Env, standard_env};
use crate::evaluate::eval;
use crate::expr::{LispExp, lisp_fmt};
use crate::heap::{Heap, collect_garbage};
use crate::helpers::{
    apply_macro, apply_procedure, ast_to_value, expand_macros, expand_quasiquote, value_to_ast,
};
use crate::parser::{read_from_tokens, tokenize};
use crate::repl::repl;
use crate::stdlib::load_stdlib;
use crate::value::Value;
use crate::vm::*;

use std::rc::Rc;

mod compiler;
mod env;
mod evaluate;
mod expr;
mod heap;
mod upvalue;

#[macro_use]
mod helpers;
mod parser;
mod repl;
mod stdlib;
mod tests;
mod value;
mod vm;

#[derive(Copy, Clone, PartialEq)]
pub enum ExecMode {
    Normal,
    Dump,
}

fn run_script(path: &str, env: &mut Env, heap: &mut Heap) -> Result<LispExp, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Error reading the file '{}': {}", path, e))?;

    run_source(&content, env, ExecMode::Normal, heap, false, false)
}

fn run_code(code: &str, mut env: Env, heap: &mut Heap) -> Result<Value, String> {
    let result_val = run_source(code, &mut env, ExecMode::Normal, heap, false, false)?;
    Ok(ast_to_value(&result_val, heap))
}

pub fn run_source(
    source: &str,
    env: &mut Env,
    mode: ExecMode,
    heap: &mut Heap,
    debug_gc: bool,
    is_repl: bool,
) -> Result<LispExp, String> {
    let tokens = tokenize(source);
    let mut tokens_iter = tokens.into_iter().peekable();
    let mut vm = Vm::new();
    let mut compiler_state = CompilerState::new();
    let mut last_result = Value::void();

    while tokens_iter.peek().is_some() {
        let raw_ast = read_from_tokens(&mut tokens_iter)?;

        let expanded_ast = expand_macros(raw_ast, env, heap)?;

        let optimized_ast = optimize_ast(expanded_ast);

        if mode == ExecMode::Dump {
            println!(
                "Expanded AST: {}",
                lisp_fmt(ast_to_value(&optimized_ast, heap), &heap)
            );
        }

        if optimized_ast != LispExp::Void {
            let mut chunk = Chunk::new();

            compile(
                &optimized_ast,
                &mut chunk,
                false,
                heap,
                &mut compiler_state,
                1,
            )?;
            // chunk.code.push(OpCode::Return);
            chunk.write(OpCode::Return, 1);

            if mode == ExecMode::Dump {
                disassemble_chunk(&chunk, "Block", &heap);
            } else {
                last_result = vm.execute(Rc::new(chunk), env.clone(), heap, is_repl)?;
            }
        }
        collect_garbage(heap, env, last_result, &vm.stack, debug_gc);
    }

    Ok(value_to_ast(last_result, heap))
}

fn main() {
    let mut heap = Heap::new();
    let mut global_env = standard_env(&mut heap);

    let macros_src = include_str!("../std/macros.lsp");
    let lib_src = include_str!("../std/lib.lsp");

    load_stdlib(macros_src, &mut global_env, &mut heap);
    load_stdlib(lib_src, &mut global_env, &mut heap);

    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        let file_path = &args[1];
        match run_script(file_path, &mut global_env, &mut heap) {
            Ok(_) => std::process::exit(0),
            Err(e) => {
                eprintln!("\x1b[1;31mFailed executing the script: {}\x1b[0m", e);
                std::process::exit(1);
            }
        }
    } else {
        repl(global_env, &mut heap)
    }
}
