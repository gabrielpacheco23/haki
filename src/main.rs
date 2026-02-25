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
use std::io::Write;

use std::rc::Rc;

mod compiler;
mod env;
mod evaluate;
mod expr;
mod heap;
mod jit;
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

    run_source(
        &content,
        env,
        ExecMode::Normal,
        heap,
        false,
        &mut CompilerState::new(),
        false,
    )
}

fn run_code(code: &str, mut env: Env, heap: &mut Heap) -> Result<Value, String> {
    let result_val = run_source(
        code,
        &mut env,
        ExecMode::Normal,
        heap,
        false,
        &mut CompilerState::new(),
        false,
    )?;
    Ok(ast_to_value(&result_val, heap))
}

pub fn run_source(
    source: &str,
    env: &mut Env,
    mode: ExecMode,
    heap: &mut Heap,
    debug_gc: bool,
    compiler_state: &mut CompilerState,
    is_repl: bool,
) -> Result<LispExp, String> {
    let tokens = tokenize(source);
    let mut tokens_iter = tokens.into_iter().peekable();
    let mut vm = Vm::new();
    // let mut compiler_state = CompilerState::new();
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

            compile(&optimized_ast, &mut chunk, false, heap, compiler_state, 1)?;
            // chunk.code.push(OpCode::Return);
            chunk.write(OpCode::Return, 1);

            if mode == ExecMode::Dump {
                disassemble_chunk(&chunk, "Block", &heap);
            } else {
                last_result = vm.execute(Rc::new(chunk), env.clone(), heap, is_repl)?;
            }
        }
        collect_garbage(heap, env, last_result, &vm.stack, &vm.frames, debug_gc);
    }

    Ok(value_to_ast(last_result, heap))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // SCRIPT MODE
    if args.len() > 1 && args[1] != "--pack" {
        let mut heap = Heap::new();
        let mut global_env = standard_env(&mut heap);

        load_stdlib(
            include_str!("../std/macros.lsp"),
            &mut global_env,
            &mut heap,
        );
        load_stdlib(include_str!("../std/lib.lsp"), &mut global_env, &mut heap);

        let file_path = &args[1];
        match run_script(file_path, &mut global_env, &mut heap) {
            Ok(_) => std::process::exit(0),
            Err(e) => {
                eprintln!("\x1b[1;31mFailed executing the script: {}\x1b[0m", e);
                std::process::exit(1);
            }
        }
    }

    // PACKER MODE (--pack)
    if args.len() > 1 && args[1] == "--pack" {
        if args.len() < 3 {
            println!("Uso correto: haki --pack <arquivo.hk>");
            std::process::exit(1);
        }
        pack_executable(&args[2]);
        std::process::exit(0);
    }

    // REPL MODE
    let mut heap = Heap::new();
    let mut global_env = standard_env(&mut heap);
    load_stdlib(
        include_str!("../std/macros.lsp"),
        &mut global_env,
        &mut heap,
    );
    load_stdlib(include_str!("../std/lib.lsp"), &mut global_env, &mut heap);

    if let Ok(exe_path) = std::env::current_exe() {
        if let Ok(exe_bytes) = std::fs::read(&exe_path) {
            let magic = get_magic_signature();
            if let Some(pos) = exe_bytes.windows(magic.len()).rposition(|w| w == magic) {
                let payload = &exe_bytes[pos + magic.len()..];
                let script = String::from_utf8_lossy(payload).to_string();

                let mut compiler_state = CompilerState::new();
                match run_source(
                    &script,
                    &mut global_env,
                    ExecMode::Normal,
                    &mut heap,
                    false,
                    &mut compiler_state,
                    false,
                ) {
                    Ok(_) => std::process::exit(0),
                    Err(e) => {
                        eprintln!("\x1b[1;31mErro fatal no standalone: {}\x1b[0m", e);
                        std::process::exit(1);
                    }
                }
            }
        }
    }

    repl(global_env, &mut heap);
}

fn pack_executable(target_file: &str) {
    let exe_path = std::env::current_exe().expect("Failed to obtain the current executable");
    let exe_bytes = std::fs::read(&exe_path).expect("Failed to read its own executable");

    let mut final_script = String::new();
    let content = std::fs::read_to_string(target_file).expect("Failed to read the script");

    for line in content.lines() {
        if line.starts_with("(require ") {
            if let (Some(start), Some(end)) = (line.find('"'), line.rfind('"')) {
                if start != end {
                    let req_path = &line[start + 1..end];
                    let req_content = std::fs::read_to_string(req_path).unwrap_or_else(|_| {
                        panic!("Failed to read the required library: {}", req_path)
                    });
                    final_script.push_str(&req_content);
                    final_script.push('\n');
                    continue;
                }
            }
        }
        final_script.push_str(line);
        final_script.push('\n');
    }

    let out_name = if cfg!(target_os = "windows") {
        "app.exe"
    } else {
        "app"
    };
    let mut out_file =
        std::fs::File::create(out_name).expect("Failed to create the final executable");

    let magic = get_magic_signature();

    out_file.write_all(&exe_bytes).unwrap();
    out_file.write_all(&magic).unwrap();
    out_file.write_all(final_script.as_bytes()).unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = out_file.metadata().unwrap().permissions();
        perms.set_mode(0o755); // Rwxr-xr-x
        out_file.set_permissions(perms).unwrap();
    }

    println!(
        "The app was packaged into a single standalone file: {}",
        out_name
    );
}

fn get_magic_signature() -> Vec<u8> {
    let mut magic = Vec::new();
    magic.extend_from_slice(b"[[[HAKI_");
    magic.extend_from_slice(b"PAYLOAD_");
    magic.extend_from_slice(b"START]]]");
    magic
}
