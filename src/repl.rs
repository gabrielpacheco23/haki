use colored::Colorize;
use rustyline::completion::Completer;
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::{Editor, Helper};
use std::borrow::Cow;

use crate::expr::lisp_fmt;
use crate::heap::Heap;
use crate::helpers::ast_to_value;
use crate::{ExecMode, env::Env, expr::LispExp, run_source};

fn print_haki_splash() {
    let splash = r#"
    __  __      __   _
   / / / /___ _/ /__(_)
  / /_/ / __ `/ //_/ / 
 / __  / /_/ / ,< / /  
/_/ /_/\__,_/_/|_/_/   

v1.5.0 - Developed by Gabriel Pacheco 
"#;
    println!("{}", splash.bold().cyan());
}

pub fn repl(mut env: Env, heap: &mut Heap) {
    print_haki_splash();
    println!("{}", "Press Ctrl+D or type 'quit' to exit.\n".dimmed());

    let mut rl = Editor::new().expect("Error starting terminal");
    rl.set_helper(Some(LispHelper::default()));
    let _ = rl.load_history("history.txt");

    let mut buffer = String::new();

    loop {
        let prompt = if buffer.is_empty() {
            "λ > ".bold().yellow().to_string()
        } else {
            ".. ".bold().yellow().to_string()
        };

        match rl.readline(&prompt) {
            Ok(line) => {
                let input = line.trim_end();

                if buffer.is_empty() && (input == "exit" || input == "quit") {
                    break;
                }
                if buffer.is_empty() && input.is_empty() {
                    continue;
                }

                buffer.push_str(input);
                buffer.push('\n');

                let mut open_parens = 0;
                let mut in_string = false;
                let mut escape = false;

                for c in buffer.chars() {
                    if escape {
                        escape = false;
                        continue;
                    }
                    match c {
                        '\\' if in_string => escape = true,
                        '"' => in_string = !in_string,
                        '(' | '[' if !in_string => open_parens += 1,
                        ')' | ']' if !in_string => open_parens -= 1,
                        _ => {}
                    }
                }

                if open_parens <= 0 {
                    let code_to_run = buffer.trim();
                    let _ = rl.add_history_entry(code_to_run);

                    let (mode, code) = if code_to_run.starts_with(":dump ") {
                        (ExecMode::Dump, &code_to_run[6..])
                    } else {
                        (ExecMode::Normal, code_to_run)
                    };

                    match run_source(code, &mut env, mode, heap, false, true) {
                        Ok(val) => {
                            if mode == ExecMode::Normal && !matches!(val, LispExp::Void) {
                                println!(
                                    "=> {}",
                                    lisp_fmt(ast_to_value(&val, heap), &heap)
                                        .to_string()
                                        .green()
                                );
                            }
                        }
                        Err(e) => eprintln!("{} {}", "[Error]".bold().red(), e.red()),
                    }

                    buffer.clear();
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("{}", "Interrupted (Ctrl+C)".dimmed());
                buffer.clear(); // Se der Ctrl+C no meio de um código longo, ele apenas limpa o buffer
            }
            Err(ReadlineError::Eof) => {
                println!("{}", "End of File (Ctrl+D)".dimmed());
                break;
            }
            Err(err) => {
                println!("REPL Error: {:?}", err);
                break;
            }
        }
    }

    let _ = rl.save_history("history.txt");
}

#[derive(Default)]
pub struct LispHelper;

impl Completer for LispHelper {
    type Candidate = String;
}
impl Hinter for LispHelper {
    type Hint = String;
}
impl Helper for LispHelper {}

impl Validator for LispHelper {}

impl Highlighter for LispHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        let mut colored = String::with_capacity(line.len() * 3);
        let mut in_string = false;
        let mut in_comment = false;

        let mut match_idx = None;
        let mut trigger_idx = None;

        if pos > 0 && pos <= line.len() {
            if let Some((idx, ')')) = line[..pos].char_indices().last() {
                trigger_idx = Some(idx);
                let mut depth = 0;
                let mut in_str_scan = false;
                for (i, c) in line[..idx].char_indices().rev() {
                    if c == '"' {
                        in_str_scan = !in_str_scan;
                        continue;
                    }
                    if in_str_scan {
                        continue;
                    }

                    if c == ')' {
                        depth += 1;
                    } else if c == '(' {
                        if depth == 0 {
                            match_idx = Some(i);
                            break;
                        } else {
                            depth -= 1;
                        }
                    }
                }
            }
        }

        let match_color = "\x1b[1;95m";
        let reset = "\x1b[0m";

        for (i, c) in line.char_indices() {
            if in_comment {
                colored.push_str(&format!("\x1b[90m{}\x1b[0m", c));
                continue;
            }

            let is_match = match_idx == Some(i) || trigger_idx == Some(i);

            match c {
                ';' if !in_string => {
                    in_comment = true;
                    colored.push_str(&format!("\x1b[90m{}\x1b[0m", c));
                }
                '"' => {
                    in_string = !in_string;
                    colored.push_str(&format!("\x1b[33m{}\x1b[0m", c));
                }
                '(' | ')' | '[' | ']' if !in_string => {
                    if is_match {
                        colored.push_str(&format!("{}{}{}", match_color, c, reset));
                    } else {
                        colored.push_str(&format!("\x1b[36m{}\x1b[0m", c));
                    }
                }
                _ => {
                    let text_color = if in_string { "\x1b[33m" } else { "" };
                    colored.push_str(&format!("{}{}{}", text_color, c, reset));
                }
            }
        }

        Cow::Owned(colored)
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _idk: bool) -> bool {
        true
    }
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Cow::Borrowed(prompt)
    }
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Borrowed(hint)
    }
    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        _comp: rustyline::CompletionType,
    ) -> Cow<'c, str> {
        Cow::Borrowed(candidate)
    }
}
