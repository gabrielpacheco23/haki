use serde_json::Value;

use crate::compiler::compile;
use crate::helpers::{apply_procedure, expand_macros, get_float, pairs_to_vec, vec_to_pairs};
use crate::vm::{Chunk, OpCode, Vm};
use crate::{def_cmp, def_fold, def_is, def_math};
use crate::{expr::*, run_script};
use std::cell::RefCell;
use std::collections::HashSet;
use std::time::Duration;
use std::{collections::HashMap as RustHashMap, rc::Rc};

use std::process::Command;

pub type Env = Rc<RefCell<LispEnv>>;

#[derive(Clone, Debug)]
pub struct LispEnv {
    pub data: RustHashMap<String, LispExp>,
    pub outer: Option<Env>,
    pub loaded_files: HashSet<String>,
}

impl LispEnv {
    pub fn new(outer: Option<Env>) -> Env {
        Rc::new(RefCell::new(LispEnv {
            data: RustHashMap::new(),
            loaded_files: HashSet::new(),
            outer,
        }))
    }

    pub fn get(env_rc: &Env, var: &str) -> Option<LispExp> {
        let env_ref = env_rc.borrow();

        if let Some(val) = env_ref.data.get(var) {
            Some(val.clone())
        } else if let Some(ref outer) = env_ref.outer {
            LispEnv::get(outer, var)
        } else {
            None
        }
    }
}

pub fn standard_env() -> Env {
    let lisp_env = LispEnv::new(None);

    {
        let mut env_ref = lisp_env.borrow_mut();
        let env = &mut env_ref.data;

        // Funções Matemáticas (Unárias)
        env.insert("sin".to_string(), LispExp::Native(def_math!(f64::sin)));
        env.insert("cos".to_string(), LispExp::Native(def_math!(f64::cos)));
        env.insert("tan".to_string(), LispExp::Native(def_math!(f64::tan)));
        env.insert("abs".to_string(), LispExp::Native(def_math!(f64::abs)));
        env.insert("sqrt".to_string(), LispExp::Native(def_math!(f64::sqrt)));
        env.insert("round".to_string(), LispExp::Native(def_math!(f64::round)));

        // Operadores de Comparação
        env.insert(">".to_string(), def_cmp!(>));
        env.insert("<".to_string(), def_cmp!(<));
        env.insert(">=".to_string(), def_cmp!(>=));
        env.insert("<=".to_string(), def_cmp!(<=));
        env.insert("=".to_string(), def_cmp!(==));

        // Operadores Aritméticos e Agregadores (Fold)
        // Soma: valor inicial 0.0, operação a + b
        env.insert(
            "+".to_string(),
            LispExp::Native(def_fold!(0.0, |a, b| a + b)),
        );
        // Multiplicação: valor inicial 1.0, operação a * b
        env.insert(
            "*".to_string(),
            LispExp::Native(def_fold!(1.0, |a, b| a * b)),
        );

        // Min e Max (usamos infinito como valor neutro inicial)
        env.insert(
            "max".to_string(),
            LispExp::Native(def_fold!(f64::NEG_INFINITY, f64::max)),
        );
        env.insert(
            "min".to_string(),
            LispExp::Native(def_fold!(f64::INFINITY, f64::min)),
        );

        env.insert(
            "list?".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 1 {
                    return Err("'list?' requires only 1 argument".to_string());
                }

                let mut current = &args[0];
                let is_proper_list = loop {
                    match current {
                        LispExp::Nil => break true,
                        LispExp::List(_) => break true,
                        LispExp::Pair(_, cdr) => current = cdr,
                        _ => break false,
                    }
                };
                Ok(LispExp::Bool(is_proper_list))
            })),
        );
        env.insert(
            "pair?".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 1 {
                    return Err("'pair?' requires only 1 argument".to_string());
                }

                let is_pair = match &args[0] {
                    LispExp::Pair(_, _) => true,
                    LispExp::List(vec) => !vec.is_empty(),
                    _ => false,
                };

                Ok(LispExp::Bool(is_pair))
            })),
        );
        env.insert(
            "symbol?".to_string(),
            LispExp::Native(def_is!(LispExp::Symbol(_))),
        );
        env.insert(
            "number?".to_string(),
            LispExp::Native(def_is!(LispExp::Number(_))),
        );

        env.insert(
            "string?".to_string(),
            LispExp::Native(def_is!(LispExp::Str(_))),
        );

        env.insert(
            "vector?".to_string(),
            LispExp::Native(def_is!(LispExp::Vector(_))),
        );

        env.insert(
            "hashmap?".to_string(),
            LispExp::Native(def_is!(LispExp::HashMap(_))),
        );

        env.insert(
            "procedure?".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 1 {
                    return Err("'procedure?' requires exactly 1 argument".to_string());
                }

                let res = match &args[0] {
                    LispExp::Native(_) | LispExp::Lambda(_) | LispExp::VmClosure { .. } => true,
                    _ => false,
                };
                Ok(LispExp::Bool(res))
            })),
        );

        // null? verifica se é uma Lista e se está vazia
        env.insert(
            "null?".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                let is_null = match &args[0] {
                    LispExp::Nil => true,
                    LispExp::List(vec) => vec.is_empty(),
                    _ => false,
                };
                Ok(LispExp::Bool(is_null))
            })),
        );

        // --- Operações de Lista ---
        // 'car': Pega o primeiro elemento
        env.insert(
            "car".to_string(),
            LispExp::Native(Rc::new(|args, _env| match &args[0] {
                LispExp::Pair(car, _) => Ok((**car).clone()),
                LispExp::List(list) => list
                    .first()
                    .cloned()
                    .ok_or_else(|| "car: empty list".to_string()),
                _ => Err("'car' requires a pair or list".to_string()),
            })),
        );

        env.insert(
            "cdr".to_string(),
            LispExp::Native(Rc::new(|args, _env| match &args[0] {
                LispExp::Pair(_, cdr) => Ok((**cdr).clone()),
                LispExp::List(list) => {
                    if list.is_empty() {
                        return Err("cdr: empty list".to_string());
                    }
                    Ok(LispExp::List(list[1..].to_vec()))
                }
                _ => Err("'cdr' requires a pair or list".to_string()),
            })),
        );

        env.insert(
            "cons".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                let head = args[0].clone();
                let tail_val = pairs_to_vec(&args[1]);

                match tail_val {
                    LispExp::List(tail) => {
                        let mut new = vec![head];
                        new.extend_from_slice(&tail);
                        Ok(LispExp::List(new))
                    }
                    _ => Ok(LispExp::Pair(Rc::new(head), Rc::new(args[1].clone()))),
                }
            })),
        );

        env.insert(
            "list".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                let mut result = LispExp::Nil;

                for arg in args.iter().rev() {
                    result = LispExp::Pair(Rc::new(arg.clone()), Rc::new(result));
                }
                Ok(result)
            })),
        );

        env.insert(
            "length".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let Some(LispExp::List(list)) = args.first() {
                    Ok(LispExp::Number(list.len() as f64))
                } else {
                    Err("'length' requires a list".to_string())
                }
            })),
        );

        env.insert(
            "not".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 1 {
                    return Err("'not' requires 1 argument".to_string());
                }
                match &args[0] {
                    LispExp::Bool(false) => Ok(LispExp::Bool(true)),
                    _ => Ok(LispExp::Bool(false)),
                }
            })),
        );

        // Constantes Matemáticas
        env.insert("pi".to_string(), LispExp::Number(std::f64::consts::PI));

        // Lista vazia (nil)
        env.insert("nil".to_string(), LispExp::Nil);

        // Void
        env.insert(
            "void".to_string(),
            LispExp::Native(Rc::new(|_args, _env| Ok(LispExp::Void))),
        );

        // Exemplo para subtração manual
        env.insert(
            "-".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                let floats: Result<Vec<f64>, _> = args.iter().map(get_float).collect();
                let floats = floats?;

                if floats.is_empty() {
                    return Err("Subtraction requires arguments".to_string());
                }

                if floats.len() == 1 {
                    // Negação unária: (- 5) -> -5
                    return Ok(LispExp::Number(-floats[0]));
                }

                // Subtração n-ária: (- 10 2 1) -> 10 - 2 - 1
                let mut result = floats[0];
                for x in &floats[1..] {
                    result -= x;
                }
                Ok(LispExp::Number(result))
            })),
        );

        // Divisão (/)
        env.insert(
            "/".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                // 1. Converte todos os argumentos para f64
                let floats: Result<Vec<f64>, String> = args.iter().map(get_float).collect();
                let floats = floats?; // Retorna erro se algum não for número

                if floats.is_empty() {
                    return Err("Division requires at least 1 argument".to_string());
                }

                // 2. Caso Unário: (/ x) -> 1/x
                if floats.len() == 1 {
                    if floats[0] == 0.0 {
                        return Err("Division by zero".to_string());
                    }
                    return Ok(LispExp::Number(1.0 / floats[0]));
                }

                // 3. Caso N-ário: (/ a b c...) -> (a / b) / c ...
                let mut acc = floats[0];
                for val in &floats[1..] {
                    if *val == 0.0 {
                        return Err("Division by zero".to_string());
                    }
                    acc /= val;
                }

                Ok(LispExp::Number(acc))
            })),
        );

        env.insert(
            "modulo".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 2 {
                    return Err("'modulo' requires 2 arguments".to_string());
                }
                let a = get_float(&args[0])?;
                let b = get_float(&args[1])?;
                Ok(LispExp::Number(a % b))
            })),
        );

        // Potenciação (expt)
        env.insert(
            "expt".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 2 {
                    return Err("'expt' requires 2 arguments".to_string());
                }

                let floats: Result<Vec<f64>, _> = args.iter().map(get_float).collect();
                let floats = floats?;
                Ok(LispExp::Number(floats[0].powf(floats[1])))
            })),
        );

        // 'apply': (apply + (1 2)) -> (+ 1 2)
        env.insert(
            "apply".to_string(),
            LispExp::Native(Rc::new(|args, env| {
                if args.len() != 2 {
                    return Err("'apply' requires 2 arguments".to_string());
                }

                let proc = &args[0];

                let func_args = match &args[1] {
                    LispExp::List(l) => l,
                    LispExp::Pair(_, _) => match pairs_to_vec(&args[1]) {
                        LispExp::List(l) => &l.clone(),
                        _ => return Err("'apply' must be used in a list".to_string()),
                    },
                    _ => {
                        return Err(format!(
                            "Second argument of 'apply' must be a list, but got: {}",
                            &args[1]
                        ));
                    }
                };

                apply_procedure(proc, &func_args, env)
            })),
        );

        // 'map': (map (lambda (x) (* x 2)) (1 2 3))
        env.insert(
            "map".to_string(),
            LispExp::Native(Rc::new(|args, env| {
                if args.len() != 2 {
                    return Err("'map' requires 2 arguments".to_string());
                }

                let proc = &args[0];
                let list_items = match &args[1] {
                    LispExp::List(l) => l,
                    _ => return Err("Second argument of 'map' must be a list".to_string()),
                };

                let mut res_list = vec![];
                for item in list_items {
                    let val = apply_procedure(proc, &[item.clone()], env)?;
                    res_list.push(val);
                }

                Ok(LispExp::List(res_list))
            })),
        );

        // 'begin': Retorna o último argumento
        env.insert(
            "begin".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                args.last()
                    .cloned()
                    .ok_or_else(|| "'begin' requires at least 1 argument".to_string())
            })),
        );

        env.insert(
            "equal?".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 2 {
                    return Err("equal? requires 2 arguments".to_string());
                }
                Ok(LispExp::Bool(args[0] == args[1]))
            })),
        );

        env.insert(
            "display".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                for arg in args.iter() {
                    print!("{}", arg);
                }
                Ok(LispExp::Void)
            })),
        );
        env.insert(
            "displayln".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                for arg in args.iter() {
                    print!("{}", arg);
                }
                println!();
                Ok(LispExp::Void)
            })),
        );

        env.insert(
            "string-contains?".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 2 {
                    return Err("'string-contains?' requires 2 arguments".to_string());
                }

                if let Some(LispExp::Str(s)) = args.first() {
                    if let Some(LispExp::Str(search)) = args.iter().nth(1) {
                        Ok(LispExp::Bool(s.contains(search)))
                    } else {
                        Err("'string-contains?' requires a string".to_string())
                    }
                } else {
                    Err("'string-contains?' can be only used in strings".to_string())
                }
            })),
        );

        env.insert(
            "string-split".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let (Some(LispExp::Str(s)), Some(LispExp::Str(delim))) =
                    (args.get(0), args.get(1))
                {
                    let parts: Vec<LispExp> = s
                        .split(delim)
                        .map(|s| LispExp::Str(s.to_string()))
                        .collect();

                    Ok(vec_to_pairs(&LispExp::List(parts)))
                } else {
                    Err("'string-split' requires 2 strings".to_string())
                }
            })),
        );

        env.insert(
            "string-append".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                let mut buffer = String::new();
                for arg in args {
                    if let LispExp::Str(s) = arg {
                        buffer.push_str(s);
                    } else {
                        return Err(format!("'string-append' expects strings, but got: {}", arg));
                    }
                }
                Ok(LispExp::Str(buffer))
            })),
        );

        env.insert(
            "string-length".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("'string-length' requires only 1 argument".to_string());
                }

                if let Some(LispExp::Str(s)) = args.first() {
                    Ok(LispExp::Number(s.len() as f64))
                } else {
                    return Err("'string-length' expects a string".to_string());
                }
            })),
        );

        env.insert(
            "string-empty?".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let Some(LispExp::Str(s)) = args.first() {
                    return Ok(LispExp::Bool(s.trim().is_empty()));
                }

                Ok(LispExp::Bool(false))
            })),
        );

        env.insert(
            "string-trim".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("'string-trim' requires only 1 argument".to_string());
                }

                if let Some(LispExp::Str(s)) = args.first() {
                    Ok(LispExp::Str(s.trim().to_string()))
                } else {
                    return Err("'string-trim' expects a string".to_string());
                }
            })),
        );

        env.insert(
            "to-upper".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("'to-upper' requires only 1 argument".to_string());
                }

                if let Some(LispExp::Str(s)) = args.first() {
                    Ok(LispExp::Str(s.to_uppercase()))
                } else {
                    return Err("'to-upper' expects a string".to_string());
                }
            })),
        );

        env.insert(
            "to-lower".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err("'to-lower' requires only 1 argument".to_string());
                }

                if let Some(LispExp::Str(s)) = args.first() {
                    Ok(LispExp::Str(s.to_lowercase()))
                } else {
                    return Err("'to-lower' expects a string".to_string());
                }
            })),
        );

        env.insert(
            "number->string".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let Some(LispExp::Number(n)) = args.first() {
                    Ok(LispExp::Str(n.to_string()))
                } else {
                    Err("'number->string' expects a number".to_string())
                }
            })),
        );

        env.insert(
            "string->number".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let Some(LispExp::Str(s)) = args.first() {
                    if let Ok(num) = s.parse::<f64>() {
                        Ok(LispExp::Number(num))
                    } else {
                        Err("Error converting to number".to_string())
                    }
                } else {
                    Err("'string->number' expects a string".to_string())
                }
            })),
        );

        env.insert(
            "substring".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let LispExp::Str(s) = args[0].clone() {
                    let start = get_float(&args[1])? as usize;
                    let end = get_float(&args[2])? as usize;
                    let subs: &str = &s[start..end];
                    return Ok(LispExp::Str(subs.to_string()));
                }

                Ok(LispExp::Nil)
            })),
        );

        env.insert(
            "load".to_string(),
            LispExp::Native(Rc::new(|args, env| {
                if args.len() != 1 {
                    return Err("'load' requires 1 argument (path)".to_string());
                }
                let path_str = match &args[0] {
                    LispExp::Str(s) => s,
                    _ => return Err("Source path must be a string literal".to_string()),
                };

                run_script(path_str, env)
            })),
        );

        env.insert(
            "shell".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let Some(LispExp::Str(cmd_str)) = args.first() {
                    let output = Command::new("sh")
                        .arg("-c")
                        .arg(cmd_str)
                        .output()
                        .map_err(|e| format!("Error running shell (sh): {}", e))?;

                    return Ok(LispExp::Str(
                        String::from_utf8_lossy(&output.stdout).to_string(),
                    ));
                } else {
                    Err("'shell' requires a string with the shell commands".to_string())
                }
            })),
        );

        env.insert(
            "eval".to_string(),
            LispExp::Native(Rc::new(|args, env| {
                if args.len() != 1 {
                    return Err("'eval' requires 1 argument".to_string());
                }

                let ast = pairs_to_vec(&args[0]);
                let expanded_ast = expand_macros(ast, env)?;

                if expanded_ast != LispExp::Void {
                    let mut chunk = Chunk::new();
                    compile(&expanded_ast, &mut chunk, false)?;
                    chunk.code.push(OpCode::Return);

                    let mut sub_vm = Vm::new();
                    sub_vm.execute(Rc::new(chunk), env.clone())
                } else {
                    Ok(LispExp::Void)
                }
            })),
        );

        env.insert(
            "sleep".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if args.len() != 1 {
                    return Err("'sleep' requires 1 argument".to_string());
                }

                let millis = get_float(&args[0])?;
                std::thread::sleep(Duration::from_millis(millis as i64 as u64));

                Ok(LispExp::Void)
            })),
        );

        env.insert(
            "get-env".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let Some(LispExp::Str(key)) = args.first() {
                    match std::env::var(key) {
                        Ok(val) => Ok(LispExp::Str(val)),
                        Err(_) => Ok(LispExp::Nil),
                    }
                } else {
                    Err("'get-env' expects a string".to_string())
                }
            })),
        );

        env.insert(
            "cd".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let Some(LispExp::Str(path)) = args.first() {
                    if let Err(e) = std::env::set_current_dir(path) {
                        return Err(format!("Error changing directory '{}': {}", path, e));
                    }
                    Ok(LispExp::Void)
                } else {
                    Err("cd expects a string with the directory path".to_string())
                }
            })),
        );

        env.insert(
            "slurp".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let Some(LispExp::Str(path)) = args.first() {
                    match std::fs::read_to_string(path) {
                        Ok(content) => Ok(LispExp::Str(content)),
                        Err(e) => Err(format!("Error reading file '{}': {}", path, e)),
                    }
                } else {
                    Err("'slurp / read-file' expects a string with the file path".to_string())
                }
            })),
        );

        env.insert("read-file".to_string(), env["slurp"].clone());

        env.insert(
            "spit".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err(
                        "'spit / write-file' requires 2 arguments (path and content)".to_string(),
                    );
                }

                if let (LispExp::Str(path), LispExp::Str(content)) = (&args[0], &args[1]) {
                    match std::fs::write(path, content) {
                        Ok(_) => Ok(LispExp::Void),
                        Err(e) => Err(format!("Error writing to file '{}': {}", path, e)),
                    }
                } else {
                    Err("'write-file' arguments must be strings".to_string())
                }
            })),
        );
        env.insert("write-file".to_string(), env["spit"].clone());

        env.insert(
            "not".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let Some(LispExp::Bool(b)) = args.first() {
                    return Ok(LispExp::Bool(!b));
                }
                Ok(LispExp::Bool(false))
            })),
        );

        env.insert(
            "type-of".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let Some(exp) = args.first() {
                    match exp {
                        LispExp::Symbol(_) => Ok(LispExp::Str("symbol".to_string())),
                        LispExp::Number(_) => Ok(LispExp::Str("number".to_string())),
                        LispExp::Bool(_) => Ok(LispExp::Str("bool".to_string())),
                        LispExp::Str(_) => Ok(LispExp::Str("string".to_string())),
                        LispExp::List(_) => Ok(LispExp::Str("list".to_string())),
                        LispExp::Native(_) => Ok(LispExp::Str("procedure".to_string())),
                        LispExp::Lambda(_) => Ok(LispExp::Str("procedure".to_string())),
                        LispExp::Macro(_) => Ok(LispExp::Str("macro".to_string())),
                        LispExp::Void => Ok(LispExp::Str("<void>".to_string())),
                        LispExp::Pair(_, cdr) => match &**cdr {
                            LispExp::Pair(_, _) | LispExp::List(_) | LispExp::Nil => {
                                Ok(LispExp::Str("list".to_string()))
                            }
                            _ => Ok(LispExp::Str("pair".to_string())),
                        },
                        LispExp::Nil => Ok(LispExp::Str("()".to_string())),
                        LispExp::Vector(_) => Ok(LispExp::Str("vector".to_string())),
                        LispExp::HashMap(_) => Ok(LispExp::Str("hashmap".to_string())),
                        LispExp::VmClosure { .. } => Ok(LispExp::Str("procedure".to_string())),
                    }
                } else {
                    Err("'type-of' expects a value".to_string())
                }
            })),
        );

        env.insert(
            "require".to_string(),
            LispExp::Native(Rc::new(|args, env| {
                if let Some(LispExp::Str(path)) = args.first() {
                    let abs_path = std::fs::canonicalize(path)
                        .map_err(|e| format!("File not found: {}", e))?
                        .to_string_lossy()
                        .to_string();

                    if env.borrow().loaded_files.contains(&abs_path) {
                        return Ok(LispExp::Void);
                    }
                    run_script(path, env)
                } else {
                    Err("'require' expects a string as path".to_string())
                }
            })),
        );

        env.insert(
            "vector".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                Ok(LispExp::Vector(Rc::new(RefCell::new(args.to_vec()))))
            })),
        );

        env.insert(
            "vector->list".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let Some(LispExp::Vector(vec)) = args.first() {
                    Ok(vec_to_pairs(&LispExp::List(vec.borrow().to_vec())))
                } else {
                    Ok(LispExp::Nil)
                }
            })),
        );

        env.insert(
            "vector-ref".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let (Some(LispExp::Vector(vec)), Some(LispExp::Number(idx))) =
                    (args.get(0), args.get(1))
                {
                    let index = *idx as usize;
                    let v = vec.borrow();
                    if index < v.len() {
                        Ok(v[index].clone())
                    } else {
                        Err(format!("Index {} out of bounds (len {})", index, v.len()))
                    }
                } else {
                    Err("'vector-ref' expects vector and index".to_string())
                }
            })),
        );

        env.insert(
            "vector-set!".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let (Some(LispExp::Vector(vec)), Some(LispExp::Number(idx)), Some(val)) =
                    (args.get(0), args.get(1), args.get(2))
                {
                    let index = *idx as usize;
                    let mut v = vec.borrow_mut();
                    if index < v.len() {
                        v[index] = val.clone();
                        Ok(LispExp::Void)
                    } else {
                        Err(format!("Index {} out of bounds (len {})", index, v.len()))
                    }
                } else {
                    Err("'vector-set!' expects vector, index and value".to_string())
                }
            })),
        );

        env.insert(
            "vector-push!".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let (Some(LispExp::Vector(vec)), Some(val)) = (args.get(0), args.get(1)) {
                    vec.borrow_mut().push(val.clone());
                    Ok(LispExp::Void)
                } else {
                    Err("'vector-push!' expects a vector and a value".to_string())
                }
            })),
        );
        env.insert(
            "make-hash".to_string(),
            LispExp::Native(Rc::new(|_args, _env| {
                Ok(LispExp::HashMap(Rc::new(RefCell::new(RustHashMap::new()))))
            })),
        );

        env.insert(
            "hash-set!".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let (Some(LispExp::HashMap(map)), Some(key_exp), Some(val)) =
                    (args.get(0), args.get(1), args.get(2))
                {
                    let key_str = match key_exp {
                        LispExp::Str(s) | LispExp::Symbol(s) => s.clone(),
                        _ => return Err("The hash key must be a string or symbol".to_string()),
                    };

                    map.borrow_mut().insert(key_str, val.clone());
                    Ok(LispExp::Void)
                } else {
                    Err("hash-set! expects hashmap, key and value".to_string())
                }
            })),
        );

        env.insert(
            "hash-ref".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let (Some(LispExp::HashMap(map)), Some(key_exp)) = (args.get(0), args.get(1)) {
                    let key_str = match key_exp {
                        LispExp::Str(s) | LispExp::Symbol(s) => s.clone(),
                        _ => return Err("The hash key must be a string or symbol".to_string()),
                    };

                    match map.borrow().get(&key_str) {
                        Some(val) => Ok(val.clone()),
                        None => Ok(LispExp::Nil),
                    }
                } else {
                    Err("hash-ref expects hashmap and key".to_string())
                }
            })),
        );

        env.insert(
            "hash-keys".to_string(),
            LispExp::Native(Rc::new(|args, _| {
                if let Some(LispExp::HashMap(map)) = args.first() {
                    let mut keys_list = LispExp::Nil;

                    for key in map.borrow().keys() {
                        keys_list =
                            LispExp::Pair(Rc::new(LispExp::Str(key.clone())), Rc::new(keys_list));
                    }
                    Ok(keys_list)
                } else {
                    Err("hash-keys expects a hashmap".to_string())
                }
            })),
        );

        env.insert(
            "parse-json".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                if let Some(LispExp::Str(json_str)) = args.first() {
                    match serde_json::from_str::<Value>(json_str) {
                        Ok(val) => Ok(json_to_lisp(&val)),
                        Err(e) => Err(format!("Error parsing JSON: {}", e)),
                    }
                } else {
                    Err("'parse-json' expects a string".to_string())
                }
            })),
        );

        env.insert(
            "hash".to_string(),
            LispExp::Native(Rc::new(|args, _env| {
                let mut map = RustHashMap::new();
                let mut i = 0;
                while i + 1 < args.len() {
                    let key_str = match &args[i] {
                        LispExp::Str(s) | LispExp::Symbol(s) => s.clone(),
                        _ => return Err("The hash key must be a string or symbol".to_string()),
                    };
                    map.insert(key_str, args[i + 1].clone());
                    i += 2;
                }
                Ok(LispExp::HashMap(Rc::new(RefCell::new(map))))
            })),
        );
    }

    lisp_env
}

fn json_to_lisp(value: &Value) -> LispExp {
    match value {
        Value::Null => LispExp::Nil,
        Value::Bool(b) => LispExp::Bool(*b),
        Value::Number(num) => {
            if let Some(f) = num.as_f64() {
                LispExp::Number(f)
            } else {
                LispExp::Number(0.0)
            }
        }
        Value::String(s) => LispExp::Str(s.clone()),
        Value::Array(arr) => {
            let mut vec = vec![];
            for item in arr {
                vec.push(json_to_lisp(item));
            }
            LispExp::Vector(Rc::new(RefCell::new(vec)))
        }
        Value::Object(obj) => {
            let mut map = RustHashMap::new();
            for (k, v) in obj {
                map.insert(k.clone(), json_to_lisp(v));
            }
            LispExp::HashMap(Rc::new(RefCell::new(map)))
        }
    }
}
