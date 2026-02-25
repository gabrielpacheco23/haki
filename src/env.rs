use crate::value::Value;
use crate::{arithmetic_op, def_fold1, run_code};
use rand::RngExt;
use regex::Regex;

use crate::heap::{Heap, collect_garbage};
use crate::helpers::{apply_procedure, ast_to_value, pairs_to_vec, value_to_ast};
use crate::{compare_op, def_fold, def_is, def_math};
use crate::{expr::*, run_script};
use std::cell::RefCell;
use std::collections::HashSet;
use std::time::Duration;
use std::{collections::HashMap as RustHashMap, rc::Rc};

use std::process::Command;

use std::sync::{Mutex, OnceLock, mpsc};

use libffi::middle::{Arg, Cif, Type};
use libloading::{Library, Symbol};
use std::ffi::CString;

// FFI (C)
unsafe extern "C" {
    fn malloc(size: usize) -> *mut u8;
    fn free(ptr: *mut u8);
}

static ACTOR_REGISTRY: OnceLock<Mutex<RustHashMap<usize, mpsc::Sender<String>>>> = OnceLock::new();

static NEXT_PID: OnceLock<Mutex<usize>> = OnceLock::new();

macro_rules! add_native {
    ($env:ident, $heap:ident, $name:expr, $func:expr) => {
        let func_val = $heap.alloc(LispExp::Native(Rc::new($func)));
        $env.insert($name.to_string(), func_val);
    };
}

pub type Env = Rc<RefCell<LispEnv>>;

#[derive(Clone, Debug)]
pub struct LispEnv {
    pub data: RustHashMap<String, Value>,
    pub outer: Option<Env>,
    pub loaded_files: HashSet<String>,
    pub mailbox: Option<Rc<RefCell<mpsc::Receiver<String>>>>,
}

impl LispEnv {
    pub fn new(outer: Option<Env>) -> Env {
        Rc::new(RefCell::new(LispEnv {
            data: RustHashMap::new(),
            loaded_files: HashSet::new(),
            outer,
            mailbox: None,
        }))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.data.get(name) {
            Some(val) => Some(*val),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn insert(&mut self, name: String, val: Value) {
        self.data.insert(name, val);
    }

    pub fn set(&mut self, name: &str, val: Value) -> Result<(), String> {
        if self.data.contains_key(name) {
            self.data.insert(name.to_string(), val);
            Ok(())
        } else {
            match &self.outer {
                Some(outer) => outer.borrow_mut().set(name, val),
                None => Err(format!("Variable {} not defined", name)),
            }
        }
    }
}

pub fn standard_env(heap: &mut Heap) -> Env {
    let lisp_env = LispEnv::new(None);

    {
        let mut env_ref = lisp_env.borrow_mut();
        let env = &mut env_ref.data;

        // Funções Matemáticas (Unárias)
        def_math!(env, heap, "sqrt", sqrt);
        def_math!(env, heap, "sin", sin);
        def_math!(env, heap, "cos", cos);
        def_math!(env, heap, "round", round);
        def_math!(env, heap, "floor", floor);

        // Operadores de Comparação
        compare_op!(env, heap, "=", ==);
        compare_op!(env, heap, ">", >);
        compare_op!(env, heap, "<", <);
        compare_op!(env, heap, ">=", >=);
        compare_op!(env, heap, "<=", <=);

        // Operadores Aritméticos e Agregadores (Fold)
        // Soma: valor inicial 0.0, operação a + b
        arithmetic_op!(env, heap, "+", 0.0, +);
        arithmetic_op!(env, heap, "-", 0.0, -);
        arithmetic_op!(env, heap, "*", 1.0, *);
        arithmetic_op!(env, heap, "/", 1.0, /);
        arithmetic_op!(env, heap, "modulo", 1.0, %);

        add_native!(env, heap, "eq?", |args, _, _| {
            if args.len() != 2 {
                return Err("'eq?' requires 2 arguments".to_string());
            }
            Ok(Value::boolean(args[0] == args[1]))
        });

        add_native!(env, heap, "cons", |args, _, heap| {
            if args.len() != 2 {
                return Err("'cons' requires 2 arguments".to_string());
            }

            Ok(heap.alloc(LispExp::Pair(args[0], args[1])))
        });

        add_native!(env, heap, "car", |args, _, h| {
            if args.is_empty() {
                return Err("'car' requires 1 argument".to_string());
            }

            if args[0].is_gc_ref() {
                match h.get(args[0]) {
                    Some(LispExp::Pair(car, _cdr)) => return Ok(*car),
                    Some(LispExp::List(vec, _)) => {
                        let vec_clone = vec.clone();
                        if vec_clone.is_empty() {
                            return Err("car: empty list".to_string());
                        }
                        return Ok(ast_to_value(&vec_clone[0], h));
                    }
                    _ => {}
                }
            }
            Err("'car' requires a pair or list".to_string())
        });

        add_native!(env, heap, "cdr", |args, _, heap| {
            if args.is_empty() {
                return Err("'cdr' requires 1 argument".to_string());
            }

            if args[0].is_gc_ref() {
                match heap.get(args[0]) {
                    Some(LispExp::Pair(_car, cdr)) => return Ok(*cdr),
                    Some(LispExp::List(vec, _)) => {
                        if vec.is_empty() {
                            return Err("cdr: empty list".to_string());
                        }
                        let rest = LispExp::List(vec[1..].to_vec(), 0);
                        return Ok(crate::helpers::ast_to_value(&rest, heap));
                    }
                    _ => {}
                }
            }
            Err("'cdr' requires a pair or list".to_string())
        });

        add_native!(env, heap, "expt", |args, _, _| {
            if args.len() != 2 || !args[0].is_number() || !args[1].is_number() {
                return Err("expt requires 2 numbers".to_string());
            }
            Ok(Value::number(args[0].as_number().powf(args[1].as_number())))
        });

        def_is!(env, heap, "number?", |v: &Value, _| v.is_number());
        def_is!(env, heap, "boolean?", |v: &Value, _| v.is_boolean());
        def_is!(env, heap, "null?", |v: &Value, h: &mut Heap| {
            if v.is_nil() {
                return true;
            }
            if v.is_gc_ref() {
                if let Some(LispExp::List(l, _)) = h.get(*v) {
                    return l.is_empty();
                }
            }
            false
        });
        def_is!(env, heap, "pair?", |v: &Value, h: &mut Heap| {
            if v.is_gc_ref() {
                match h.get(*v) {
                    Some(LispExp::Pair(_, _)) => return true,
                    Some(LispExp::List(l, _)) => return !l.is_empty(),
                    _ => return false,
                }
            }
            false
        });

        add_native!(env, heap, "list?", |args, _, heap| {
            if args.len() != 1 {
                return Err("'list?' requires 1 argument".to_string());
            }
            let mut current = args[0];

            if current.is_gc_ref() {
                if let Some(LispExp::List(_, _)) = heap.get(current) {
                    return Ok(Value::boolean(true));
                }
            }

            loop {
                if current.is_nil() {
                    return Ok(Value::boolean(true));
                }
                if current.is_gc_ref() {
                    if let Some(LispExp::Pair(_, cdr)) = heap.get(current) {
                        current = *cdr;
                        continue;
                    }
                }
                return Ok(Value::boolean(false));
            }
        });

        def_is!(env, heap, "vector?", |v: &Value, h: &mut Heap| {
            v.is_gc_ref() && matches!(h.get(*v), Some(LispExp::Vector(_)))
        });

        def_is!(env, heap, "hashmap?", |v: &Value, h: &mut Heap| {
            v.is_gc_ref() && matches!(h.get(*v), Some(LispExp::HashMap(_)))
        });

        def_is!(env, heap, "string?", |v: &Value, h: &mut Heap| {
            v.is_gc_ref() && matches!(h.get(*v), Some(LispExp::Str(_)))
        });
        def_is!(env, heap, "symbol?", |v: &Value, h: &mut Heap| {
            v.is_gc_ref() && matches!(h.get(*v), Some(LispExp::Symbol(_, _)))
        });

        def_is!(env, heap, "procedure?", |v: &Value, h: &mut Heap| {
            let is_func = match h.get(*v) {
                Some(LispExp::Native(_))
                | Some(LispExp::VmClosure { .. })
                | Some(LispExp::Lambda(_)) => true,
                _ => false,
            };
            v.is_gc_ref() && is_func
        });

        def_fold!(
            env,
            heap,
            "and",
            Value::boolean(true),
            |acc: Value, next: Value, _heap| {
                if acc.is_boolean() && next.is_boolean() {
                    Ok(Value::boolean(acc.as_boolean() && next.as_boolean()))
                } else {
                    Err("'and' expects boolean arguments".to_string())
                }
            }
        );

        def_fold!(
            env,
            heap,
            "or",
            Value::boolean(true),
            |acc: Value, next: Value, _heap| {
                if acc.is_boolean() && next.is_boolean() {
                    Ok(Value::boolean(acc.as_boolean() || next.as_boolean()))
                } else {
                    Err("'or' expects boolean arguments".to_string())
                }
            }
        );

        def_fold1!(env, heap, "min", |acc: Value, next: Value, _heap| {
            if !acc.is_number() || !next.is_number() {
                return Err("'min' expects only numbers".to_string());
            }
            Ok(Value::number(acc.as_number().min(next.as_number())))
        });

        def_fold1!(env, heap, "max", |acc: Value, next: Value, _heap| {
            if !acc.is_number() || !next.is_number() {
                return Err("'max' expects only numbers".to_string());
            }
            Ok(Value::number(acc.as_number().max(next.as_number())))
        });

        add_native!(env, heap, "begin", |args, _, _| {
            Ok(args.last().copied().unwrap_or_else(Value::void))
        });

        add_native!(env, heap, "list", |args, _, heap| {
            let mut result = Value::nil();
            for arg in args.iter().rev() {
                result = heap.alloc(LispExp::Pair(*arg, result));
            }
            Ok(result)
        });

        add_native!(env, heap, "length", |args, _, heap| {
            if args.len() != 1 {
                return Err("'length' requires 1 argument".to_string());
            }
            let mut current = args[0];
            let mut count = 0.0;
            loop {
                if current.is_nil() {
                    return Ok(Value::number(count));
                }
                if current.is_gc_ref() {
                    if let Some(LispExp::Pair(_, cdr)) = heap.get(current) {
                        count += 1.0;
                        current = *cdr;
                        continue;
                    }
                }
                return Err("'length' requires a valid list".to_string());
            }
        });

        env.insert("pi".to_string(), Value::number(std::f64::consts::PI));
        env.insert("nil".to_string(), Value::nil());
        env.insert("void".to_string(), Value::void());

        add_native!(env, heap, "display", |args, _, h| {
            use std::io::Write;
            for arg in args {
                if arg.is_gc_ref() {
                    if let Some(LispExp::Str(s)) = h.get(*arg) {
                        print!("{}", s);
                        continue;
                    }
                }

                print!("{}", lisp_fmt(*arg, h));
            }
            std::io::stdout().flush().unwrap();
            Ok(Value::void())
        });

        add_native!(env, heap, "displayln", |args, _, h| {
            for arg in args {
                if arg.is_gc_ref() {
                    if let Some(LispExp::Str(s)) = h.get(*arg) {
                        print!("{}", s);
                        continue;
                    }
                }
                print!("{}", lisp_fmt(*arg, h));
            }
            println!();
            Ok(Value::void())
        });

        add_native!(env, heap, "equal?", |args, _, heap| {
            if args.len() != 2 {
                return Err("equal? exige 2 argumentos".to_string());
            }
            let eh_igual = is_deep_equal(args[0], args[1], heap);
            Ok(Value::boolean(eh_igual))
        });
        add_native!(env, heap, "string-contains?", |args, _, heap| {
            if args.len() != 2 {
                return Err("'string-contains?' requires 2 strings".to_string());
            }
            if let (Some(LispExp::Str(s)), Some(LispExp::Str(sub))) =
                (heap.get(args[0]), heap.get(args[1]))
            {
                return Ok(Value::boolean(s.contains(sub)));
            }
            Err("string-contains? exige duas strings".to_string())
        });

        add_native!(env, heap, "string-split", |args, _, h| {
            if args.len() != 2 {
                return Err("'string-split' requires (string delim)".to_string());
            }

            let (s_str, delim_str) = match (h.get(args[0]), h.get(args[1])) {
                (Some(LispExp::Str(s)), Some(LispExp::Str(d))) => (s.clone(), d.clone()),
                _ => return Err("'string-split' requires 2 strings".to_string()),
            };

            let mut result = Value::nil();
            for part in s_str
                .split(&delim_str)
                .collect::<Vec<&str>>()
                .into_iter()
                .rev()
            {
                let str_val = h.alloc(LispExp::Str(part.to_string()));
                result = h.alloc(LispExp::Pair(str_val, result));
            }
            Ok(result)
        });

        add_native!(env, heap, "string-append", |args, _, heap| {
            let mut result = String::new();
            for arg in args {
                if arg.is_gc_ref() {
                    if let Some(LispExp::Str(s)) = heap.get(*arg) {
                        result.push_str(s);
                        continue;
                    }
                }
                return Err("'string-append' requires strings".to_string());
            }
            Ok(heap.alloc_string(result))
        });

        add_native!(env, heap, "string-length", |args, _, heap| {
            if args.len() != 1 {
                return Err("'string-length' requires 1 argument".to_string());
            }
            if args[0].is_gc_ref() {
                if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                    return Ok(Value::number(s.len() as f64));
                }
            }
            Err("'string-length' requires a string".to_string())
        });

        add_native!(env, heap, "string-empty?", |args, _, heap| {
            if args.len() != 1 {
                return Err("'string-empty?' requires 1 argument".to_string());
            }
            if args[0].is_gc_ref() {
                if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                    return Ok(Value::boolean(s.is_empty()));
                }
            }
            Err("'string-empty?' requires a string".to_string())
        });

        add_native!(env, heap, "to-upper", |args, _, heap| {
            if args.len() != 1 || !args[0].is_gc_ref() {
                return Err("'to-upper' requires 1 string".to_string());
            }
            if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                Ok(heap.alloc_string(s.to_uppercase()))
            } else {
                Err("'to-upper' requires a string".to_string())
            }
        });

        add_native!(env, heap, "to-lower", |args, _, heap| {
            if args.len() != 1 || !args[0].is_gc_ref() {
                return Err("'to-lower' requires 1 string".to_string());
            }
            if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                Ok(heap.alloc_string(s.to_lowercase()))
            } else {
                Err("'to-lower' requires a string".to_string())
            }
        });

        add_native!(env, heap, "string-trim", |args, _, heap| {
            if args.len() != 1 || !args[0].is_gc_ref() {
                return Err("'string-trim' requires a string".to_string());
            }
            if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                Ok(heap.alloc_string(s.trim().to_string()))
            } else {
                Err("'string-trim' requires a string".to_string())
            }
        });

        add_native!(env, heap, "number->string", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_number() {
                    let s = val.as_number().to_string();
                    return Ok(heap.alloc_string(s));
                }
            }
            Err("'number->string' expects a number".to_string())
        });

        add_native!(env, heap, "string->number", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    if let Some(LispExp::Str(s)) = heap.get(*val) {
                        if let Ok(num) = s.parse::<f64>() {
                            return Ok(Value::number(num));
                        } else {
                            return Err("Error parsing number".to_string());
                        }
                    }
                }
            }
            Err("'string->number' expects a string".to_string())
        });

        add_native!(env, heap, "substring", |args, _, heap| {
            if args.len() != 3 {
                return Err("substring requires 3 args".to_string());
            }
            if args[0].is_gc_ref() && args[1].is_number() && args[2].is_number() {
                if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                    let start = args[1].as_number() as usize;
                    let end = args[2].as_number() as usize;
                    if start <= end && end <= s.len() {
                        let subs = &s[start..end];
                        return Ok(heap.alloc_string(subs.to_string()));
                    }
                }
            }
            Err("Invalid arguments for substring".to_string())
        });

        // ==========================================
        // I/O E SISTEMA
        // ==========================================
        add_native!(env, heap, "load", |args, env, h| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    let path_str = if let Some(LispExp::Str(path)) = h.get(*val) {
                        path.clone()
                    } else {
                        return Err("'load' requires a string path".to_string());
                    };

                    let result_ast = run_script(&path_str, env, h)?;
                    return Ok(ast_to_value(&result_ast, h));
                }
            }
            Err("'load' requires a string path".to_string())
        });

        add_native!(env, heap, "require", |args, env, h| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    let path_str = if let Some(LispExp::Str(path)) = h.get(*val) {
                        path.clone()
                    } else {
                        return Err("'require' requires a string path".to_string());
                    };

                    if env.borrow().loaded_files.contains(&path_str) {
                        return Ok(Value::boolean(true));
                    }

                    let result_ast = run_script(&path_str, env, h)?;
                    env.borrow_mut().loaded_files.insert(path_str);

                    return Ok(ast_to_value(&result_ast, h));
                }
            }
            Err("'require' requires a string path".to_string())
        });

        add_native!(env, heap, "shell", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    if let Some(LispExp::Str(cmd)) = heap.get(*val) {
                        let output = Command::new("sh")
                            .arg("-c")
                            .arg(cmd)
                            .output()
                            .map_err(|e| format!("Shell error: {}", e))?;
                        let res = String::from_utf8_lossy(&output.stdout).to_string();
                        return Ok(heap.alloc_string(res));
                    }
                }
            }
            Err("'shell' requires a string".to_string())
        });

        add_native!(env, heap, "sleep", |args, _, _| {
            if let Some(val) = args.first() {
                if val.is_number() {
                    std::thread::sleep(Duration::from_millis(val.as_number() as u64));
                    return Ok(Value::void());
                }
            }
            Err("'sleep' requires a number".to_string())
        });

        add_native!(env, heap, "get-env", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    if let Some(LispExp::Str(key)) = heap.get(*val) {
                        return match std::env::var(key) {
                            Ok(res) => Ok(heap.alloc_string(res)),
                            Err(_) => Ok(Value::nil()),
                        };
                    }
                }
            }
            Err("'get-env' expects a string".to_string())
        });

        add_native!(env, heap, "cd", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    if let Some(LispExp::Str(path)) = heap.get(*val) {
                        std::env::set_current_dir(path).map_err(|e| e.to_string())?;
                        return Ok(Value::void());
                    }
                }
            }
            Err("'cd' expects a string".to_string())
        });

        add_native!(env, heap, "slurp", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    if let Some(LispExp::Str(path)) = heap.get(*val) {
                        let content = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
                        return Ok(heap.alloc_string(content));
                    }
                }
            }
            Err("'slurp' expects a string path".to_string())
        });

        let slurp_val = env["slurp"];
        env.insert("read-file".to_string(), slurp_val);

        add_native!(env, heap, "spit", |args, _, heap| {
            if args.len() == 2 && args[0].is_gc_ref() && args[1].is_gc_ref() {
                if let (Some(LispExp::Str(path)), Some(LispExp::Str(content))) =
                    (heap.get(args[0]), heap.get(args[1]))
                {
                    std::fs::write(path, content).map_err(|e| e.to_string())?;
                    return Ok(Value::void());
                }
            }
            Err("'spit' requires 2 strings".to_string())
        });

        let spit_val = env["spit"];
        env.insert("write-file".to_string(), spit_val);

        add_native!(env, heap, "type-of", |args, _, heap| {
            if let Some(val) = args.first() {
                let type_str = if val.is_number() {
                    "number"
                } else if val.is_boolean() {
                    "bool"
                } else if val.is_nil() {
                    "()"
                } else if val.is_void() {
                    "<void>"
                } else if val.is_gc_ref() {
                    match heap.get(*val) {
                        Some(LispExp::Symbol(_, _)) => "symbol",
                        Some(LispExp::Str(_)) => "string",
                        Some(LispExp::List(_, _)) => "list",
                        Some(LispExp::Pair(_, _)) => "pair",
                        Some(LispExp::Native(_))
                        | Some(LispExp::Lambda(_))
                        | Some(LispExp::VmClosure { .. }) => "procedure",
                        Some(LispExp::Macro(_)) => "macro",
                        Some(LispExp::Vector(_)) => "vector",
                        Some(LispExp::HashMap(_)) => "hashmap",
                        _ => "unknown",
                    }
                } else {
                    "unknown"
                };
                return Ok(heap.alloc_string(type_str.to_string()));
            }
            Err("'type-of' expects a value".to_string())
        });

        // ==========================================
        // VECTORS
        // ==========================================
        add_native!(env, heap, "vector", |args, _, heap| {
            Ok(heap.alloc(LispExp::Vector(args.to_vec())))
        });

        add_native!(env, heap, "vector->list", |args, _, h| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    let vec_clone = if let Some(LispExp::Vector(vec)) = h.get(*val) {
                        vec.clone()
                    } else {
                        return Ok(Value::nil());
                    };

                    let mut result = Value::nil();
                    for item in vec_clone.iter().rev() {
                        result = h.alloc(LispExp::Pair(*item, result));
                    }
                    return Ok(result);
                }
            }
            Ok(Value::nil())
        });

        add_native!(env, heap, "vector-ref", |args, _, heap| {
            if args.len() == 2 && args[0].is_gc_ref() && args[1].is_number() {
                let idx = args[1].as_number() as usize;
                if let Some(LispExp::Vector(vec)) = heap.get(args[0]) {
                    if idx < vec.len() {
                        return Ok(vec[idx]);
                    } else {
                        return Err("Index out of bounds".to_string());
                    }
                }
            }
            Err("'vector-ref' requires vector and index".to_string())
        });

        add_native!(env, heap, "vector-set!", |args, _, heap| {
            if args.len() == 3 && args[0].is_gc_ref() && args[1].is_number() {
                let idx = args[1].as_number() as usize;
                let new_val = args[2];
                if let Some(LispExp::Vector(vec)) = heap.get_mut(args[0]) {
                    if idx < vec.len() {
                        vec[idx] = new_val;
                        return Ok(Value::void());
                    } else {
                        return Err("Index out of bounds".to_string());
                    }
                }
            }
            Err("'vector-set!' requires vector, index and value".to_string())
        });

        add_native!(env, heap, "vector-push!", |args, _, heap| {
            if args.len() == 2 && args[0].is_gc_ref() {
                let new_val = args[1];
                if let Some(LispExp::Vector(vec)) = heap.get_mut(args[0]) {
                    vec.push(new_val);
                    return Ok(Value::void());
                }
            }
            Err("'vector-push!' requires vector and value".to_string())
        });

        // ==========================================
        // HASHMAPS
        // ==========================================
        add_native!(env, heap, "make-hash", |_, _, heap| {
            Ok(heap.alloc(LispExp::HashMap(RustHashMap::new())))
        });

        add_native!(env, heap, "hash-set!", |args, _, heap| {
            if args.len() == 3 && args[0].is_gc_ref() && args[1].is_gc_ref() {
                let key_str =
                    if let Some(LispExp::Str(s) | LispExp::Symbol(s, _)) = heap.get(args[1]) {
                        s.clone()
                    } else {
                        return Err("Invalid key".to_string());
                    };
                let val = args[2];
                if let Some(LispExp::HashMap(map)) = heap.get_mut(args[0]) {
                    map.insert(key_str, val);
                    return Ok(Value::void());
                }
            }
            Err("'hash-set!' requires map, key and value".to_string())
        });

        add_native!(env, heap, "hash-ref", |args, _, heap| {
            if args.len() == 2 && args[0].is_gc_ref() && args[1].is_gc_ref() {
                let key_str =
                    if let Some(LispExp::Str(s) | LispExp::Symbol(s, _)) = heap.get(args[1]) {
                        s.clone()
                    } else {
                        return Err("Invalid key".to_string());
                    };
                if let Some(LispExp::HashMap(map)) = heap.get(args[0]) {
                    return Ok(map.get(&key_str).copied().unwrap_or(Value::nil()));
                }
            }
            Err("'hash-ref' requires map and key".to_string())
        });

        add_native!(env, heap, "hash-keys", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    let keys: Vec<String> = if let Some(LispExp::HashMap(map)) = heap.get(*val) {
                        map.keys().cloned().collect()
                    } else {
                        return Err("Not a hashmap".to_string());
                    };

                    let mut result = Value::nil();
                    for key in keys {
                        let key_val = heap.alloc_string(key);
                        result = heap.alloc(LispExp::Pair(key_val, result));
                    }
                    return Ok(result);
                }
            }
            Err("'hash-keys' requires a map".to_string())
        });

        add_native!(env, heap, "hash", |args, _, heap| {
            let mut map = RustHashMap::new();
            let mut i = 0;
            while i + 1 < args.len() {
                let key_str =
                    if let Some(LispExp::Str(s) | LispExp::Symbol(s, _)) = heap.get(args[i]) {
                        s.clone()
                    } else {
                        return Err("Invalid key".to_string());
                    };
                map.insert(key_str, args[i + 1]);
                i += 2;
            }
            Ok(heap.alloc(LispExp::HashMap(map)))
        });

        // ==========================================
        // UTILITÁRIOS FINAIS
        // ==========================================
        add_native!(env, heap, "parse-json", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    if let Some(LispExp::Str(s)) = heap.get(*val) {
                        match serde_json::from_str::<serde_json::Value>(s) {
                            Ok(json_val) => return Ok(json_to_lisp(&json_val, heap)),
                            Err(e) => return Err(e.to_string()),
                        }
                    }
                }
            }
            Err("'parse-json' requires a string".to_string())
        });

        add_native!(env, heap, "random", |args, _, _| {
            let mut rng = rand::rng();
            if args.len() == 2 && args[0].is_number() && args[1].is_number() {
                let min = args[0].as_number() as i64;
                let max = args[1].as_number() as i64;
                let num = rng.random_range(min..max);
                return Ok(Value::number(num as f64));
            }
            Err("'random' expects 2 numbers".to_string())
        });

        add_native!(env, heap, "random->float", |args, _, _| {
            let mut rng = rand::rng();
            if args.len() == 2 && args[0].is_number() && args[1].is_number() {
                let min = args[0].as_number();
                let max = args[1].as_number();
                let num = rng.random_range(min..max);
                return Ok(Value::number(num));
            }
            Err("'random' expects 2 numbers".to_string())
        });

        add_native!(env, heap, "gc", |_, env_ref, heap| {
            let before = heap.memory.len() - heap.free_list.len();
            collect_garbage(heap, env_ref, Value::void(), &[], true);
            let after = heap.memory.len() - heap.free_list.len();
            println!("[GC] Cleaned {} object(s).", before - after);
            Ok(Value::void())
        });

        add_native!(env, heap, "time-ms", |_, _, _| {
            use std::time::{SystemTime, UNIX_EPOCH};
            let start = SystemTime::now();
            let since_the_epoch = start.duration_since(UNIX_EPOCH).expect("Time is broken");
            Ok(Value::number(since_the_epoch.as_millis() as f64))
        });

        add_native!(env, heap, "error", |args, _, h| {
            let mut err_msg = String::new();
            for arg in args {
                err_msg.push_str(&lisp_fmt(*arg, h).to_string());
                err_msg.push(' ');
            }
            Err(err_msg.trim().to_string())
        });

        add_native!(env, heap, "regex-match?", |args, _, h| {
            if args.len() != 2 {
                return Err("'regex-match?' requires (pattern string)".to_string());
            }
            if let (Some(LispExp::Str(pat)), Some(LispExp::Str(text))) =
                (h.get(args[0]), h.get(args[1]))
            {
                let re = Regex::new(pat).map_err(|e| e.to_string())?;
                return Ok(Value::boolean(re.is_match(text)));
            }

            Err("Expected 2 strings".to_string())
        });

        add_native!(env, heap, "regex-replace", |args, _, h| {
            if args.len() != 3 {
                return Err("'regex-replace' requires (pattern string replacement)".to_string());
            }
            if let (Some(LispExp::Str(pat)), Some(LispExp::Str(text)), Some(LispExp::Str(rep))) =
                (h.get(args[0]), h.get(args[1]), h.get(args[2]))
            {
                let re = regex::Regex::new(pat).map_err(|e| e.to_string())?;
                let result = re.replace_all(text, rep).to_string();
                return Ok(h.alloc_string(result));
            }
            Err("Expected 3 strings".to_string())
        });

        add_native!(env, heap, "http-get", |args, _, h| {
            if args.len() != 1 {
                return Err("'http-get' requires a URL".to_string());
            }
            if let Some(LispExp::Str(url)) = h.get(args[0]) {
                let body = ureq::get(url)
                    .call()
                    .map_err(|e| e.to_string())?
                    .body_mut()
                    .read_to_string()
                    .map_err(|e| e.to_string())?;
                return Ok(h.alloc_string(body));
            }
            Err("Expected a string URL".to_string())
        });

        add_native!(env, heap, "exit", |args, _, _| {
            let code = if args.len() == 1 && args[0].is_number() {
                args[0].as_number() as i32
            } else {
                0
            };
            std::process::exit(code);
        });

        add_native!(env, heap, "file-exists?", |args, _, heap| {
            if args.len() != 1 {
                return Err("requires 1 path".to_string());
            }
            if let Some(LispExp::Str(path)) = heap.get(args[0]) {
                return Ok(Value::boolean(std::path::Path::new(path).exists()));
            }
            Err("Expected string path".to_string())
        });

        add_native!(env, heap, "delete-file", |args, _, h| {
            if args.len() != 1 {
                return Err("'delete-file' requires 1 path".to_string());
            }
            if let Some(LispExp::Str(path)) = h.get(args[0]) {
                match std::fs::remove_file(path) {
                    Ok(_) => return Ok(Value::boolean(true)),
                    Err(e) => return Err(e.to_string()),
                }
            }
            Err("Expected string path".to_string())
        });

        add_native!(env, heap, "list-dir", |args, _, h| {
            if args.len() != 1 {
                return Err("'list-dir' requires 1 path".to_string());
            }
            if let Some(LispExp::Str(path)) = h.get(args[0]) {
                match std::fs::read_dir(path) {
                    Ok(entries) => {
                        let mut result = Value::nil();
                        let mut paths = vec![];
                        for entry in entries.flatten() {
                            if let Ok(name) = entry.file_name().into_string() {
                                paths.push(name);
                            }
                        }
                        for p in paths.into_iter().rev() {
                            let str_val = h.alloc_string(p);
                            result = h.alloc(LispExp::Pair(str_val, result));
                        }
                        return Ok(result);
                    }
                    Err(e) => return Err(e.to_string()),
                }
            }
            Err("Expected string path".to_string())
        });

        add_native!(env, heap, "try-catch", |args, env_ref, heap| {
            if args.len() != 2 {
                return Err("'try-catch' requires (try-thunk catch-fn)".to_string());
            }

            let try_thunk = value_to_ast(args[0], heap);
            let catch_fn = value_to_ast(args[1], heap);

            match apply_procedure(&try_thunk, &[], env_ref, heap) {
                Ok(res) => Ok(crate::helpers::ast_to_value(&res, heap)),
                Err(err_msg) => {
                    let err_exp = LispExp::Str(err_msg);

                    match apply_procedure(&catch_fn, &[err_exp], env_ref, heap) {
                        Ok(res) => Ok(crate::helpers::ast_to_value(&res, heap)),
                        Err(e) => Err(e),
                    }
                }
            }
        });

        // TODO: REMOVE THIS DUPLICATE
        add_native!(env, heap, "vector->list", |args, _, h| {
            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    let vec_clone = if let Some(LispExp::Vector(vec)) = h.get(*val) {
                        vec.clone()
                    } else {
                        return Ok(Value::nil());
                    };

                    let mut result = Value::nil();
                    for item in vec_clone.iter().rev() {
                        result = h.alloc(LispExp::Pair(*item, result));
                    }
                    return Ok(result);
                }
            }
            Ok(Value::nil())
        });

        // Transforma "Haki" em '("H" "a" "k" "i")
        add_native!(env, heap, "string->list", |args, _, h| {
            if args.len() != 1 {
                return Err("requires 1 string".to_string());
            }

            if let Some(val) = args.first() {
                if val.is_gc_ref() {
                    let str_clone = if let Some(LispExp::Str(s)) = h.get(*val) {
                        s.clone()
                    } else {
                        return Ok(Value::nil());
                    };

                    let mut result = Value::nil();

                    for c in str_clone.chars().rev() {
                        let char_str = h.alloc_string(c.to_string());
                        result = h.alloc(LispExp::Pair(char_str, result));
                    }
                    return Ok(result);
                }
            }

            Err("Expected string".to_string())
        });

        add_native!(env, heap, "char->ascii", |args, _, heap| {
            if args.len() != 1 {
                return Err("requires 1 char string".to_string());
            }
            if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                if let Some(c) = s.chars().next() {
                    return Ok(Value::number(c as u32 as f64));
                }
            }
            Err("Expected char string".to_string())
        });

        add_native!(env, heap, "ascii->char", |args, _, heap| {
            if args.len() != 1 {
                return Err("requires 1 number".to_string());
            }
            let code = args[0].as_number() as u32;
            if let Some(c) = char::from_u32(code) {
                return Ok(heap.alloc_string(c.to_string()));
            }
            Err("Invalid ASCII code".to_string())
        });

        add_native!(env, heap, "start-server", |args, env_ref, heap_| {
            if args.len() != 2 {
                return Err("'start-server' requires (port handler-fn)".to_string());
            }

            let port = if args[0].is_number() {
                args[0].as_number() as u16
            } else {
                return Err("Port must be a number".to_string());
            };

            // Transforma a closure recebida em AST para podermos executá-la depois
            let handler_ast = value_to_ast(args[1], heap_);

            use std::io::{Read, Write};
            use std::net::TcpListener;

            let address = format!("127.0.0.1:{}", port);
            let listener = TcpListener::bind(&address).map_err(|e| e.to_string())?;
            println!("[Haki Server] Rodando e ouvindo em http://{}", address);

            // O Loop Infinito do Servidor (Trava a VM aqui)
            for stream in listener.incoming() {
                match stream {
                    Ok(mut stream) => {
                        let mut buffer = [0; 2048];
                        if let Ok(bytes_read) = stream.read(&mut buffer) {
                            let request_str =
                                String::from_utf8_lossy(&buffer[..bytes_read]).to_string();

                            // Ignora requisições vazias (como health-checks ou pings fantasmas)
                            if request_str.trim().is_empty() {
                                continue;
                            }

                            // 1. Passa a requisição do navegador para o Lisp!
                            // let arg_exp = crate::expr::LispExp::Str(request_str);

                            // Lê a primeira linha: "GET /caminho HTTP/1.1"
                            let mut parts = request_str.split_whitespace();
                            let method = parts.next().unwrap_or("GET");
                            let path = parts.next().unwrap_or("/");

                            // Constrói um Dicionário (HashMap) para o Lisp!
                            let mut req_map = RustHashMap::new();
                            req_map.insert(
                                "method".to_string(),
                                heap_.alloc_string(method.to_string()),
                            );
                            req_map
                                .insert("path".to_string(), heap_.alloc_string(path.to_string()));
                            req_map.insert("raw".to_string(), heap_.alloc_string(request_str));

                            let arg_exp = LispExp::HashMap(req_map);

                            match apply_procedure(&handler_ast, &[arg_exp], env_ref, heap_) {
                                Ok(res_ast) => {
                                    // 2. Pega o que o Lisp retornou
                                    let res_val = crate::helpers::ast_to_value(&res_ast, heap_);
                                    let response_body = if let Some(crate::expr::LispExp::Str(s)) =
                                        heap_.get(res_val)
                                    {
                                        s.clone()
                                    } else {
                                        crate::expr::lisp_fmt(res_val, heap_).to_string()
                                    };

                                    // 3. Empacota como uma resposta HTTP real e devolve ao navegador
                                    let response = format!(
                                        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n{}",
                                        response_body.len(),
                                        response_body
                                    );
                                    let _ = stream.write_all(response.as_bytes());
                                }
                                Err(e) => {
                                    println!("[Server Error] {}", e);
                                    let error_res = format!(
                                        "HTTP/1.1 500 Internal Server Error\r\n\r\nErro Lisp: {}",
                                        e
                                    );
                                    let _ = stream.write_all(error_res.as_bytes());
                                }
                            }
                        }
                    }
                    Err(e) => println!("Falha na conexão: {}", e),
                }
            }
            Ok(Value::void())
        });

        // ==========================================
        // CONCORRÊNCIA (ERLANG ACTOR MODEL)
        // ==========================================

        add_native!(env, heap, "spawn-native", |args, _, h| {
            if args.len() != 1 {
                return Err("'spawn-native' requires 1 argument".to_string());
            }

            let lambda_str = lisp_fmt(args[0], h).to_string();
            let (tx, rx) = mpsc::channel::<String>();
            let pid = {
                let mutex = NEXT_PID.get_or_init(|| Mutex::new(1));
                let mut guard = mutex.lock().unwrap();
                let p = *guard;
                *guard += 1;
                p
            };

            let reg = ACTOR_REGISTRY.get_or_init(|| Mutex::new(RustHashMap::new()));

            reg.lock().unwrap().insert(pid, tx);

            std::thread::spawn(move || {
                let mut new_heap = Heap::new();
                let new_env = standard_env(&mut new_heap);
                new_env.borrow_mut().mailbox = Some(Rc::new(RefCell::new(rx)));

                let _ = run_script(
                    "/home/pachequinho/Documents/rustprojs/haki/std/macros.lsp",
                    &mut new_env.clone(),
                    &mut new_heap,
                );
                let _ = run_script(
                    "/home/pachequinho/Documents/rustprojs/haki/std/lib.lsp",
                    &mut new_env.clone(),
                    &mut new_heap,
                );

                let code_to_run = format!("({})", lambda_str);

                if let Err(e) = run_code(&code_to_run, new_env, &mut new_heap) {
                    eprintln!("[Actor {} Failed] {}", pid, e);
                }
            });
            Ok(Value::number(pid as f64))
        });

        add_native!(env, heap, "send", |args, _, h| {
            if args.len() != 2 {
                return Err("'send' requires (pid msg)".to_string());
            }
            if !args[0].is_number() {
                return Err("'send' PID must be a number".to_string());
            }

            let pid = args[0].as_number() as usize;

            // Aceita qualquer tipo como mensagem, mas converte para String para a viagem pelas threads
            let msg = if let Some(LispExp::Str(s)) = h.get(args[1]) {
                s.clone()
            } else {
                lisp_fmt(args[1], h).to_string()
            };

            let reg = ACTOR_REGISTRY.get_or_init(|| Mutex::new(RustHashMap::new()));
            if let Some(tx) = reg.lock().unwrap().get(&pid) {
                let _ = tx.send(msg); // Despacha a mensagem
                Ok(Value::boolean(true))
            } else {
                Ok(Value::boolean(false)) // O PID não existe ou o ator já morreu
            }
        });

        add_native!(env, heap, "receive", |_, env_ref, h| {
            // Recurso genial: Procura a caixa de correio subindo pelo escopo
            fn get_rx(env: &Env) -> Option<Rc<RefCell<mpsc::Receiver<String>>>> {
                let e = env.borrow();
                if let Some(ref rx) = e.mailbox {
                    Some(rx.clone())
                } else if let Some(ref outer) = e.outer {
                    get_rx(outer)
                } else {
                    None
                }
            }

            if let Some(rx) = get_rx(env_ref) {
                // TRAVA o ator atual. Ele não faz nada até uma mensagem cair na caixa!
                match rx.borrow().recv() {
                    Ok(msg) => Ok(h.alloc_string(msg)),
                    Err(_) => Err("Mailbox closed".to_string()),
                }
            } else {
                Err("This process has no mailbox!".to_string())
            }
        });

        // add_native!(env, heap, "malloc", |args, e, h| native_malloc(args, e, h));

        add_native!(env, heap, "malloc", |args, _, h| {
            if args.len() != 1 {
                return Err("'malloc' requires 1 argument (size in bytes)".to_string());
            }

            let size = args[0].as_number() as usize;
            let raw_address = unsafe { malloc(size) } as usize;

            if raw_address == 0 {
                return Err("Out of memory (malloc failed)".to_string());
            }

            let ptr_obj = h.alloc(LispExp::RawPtr(raw_address));
            Ok(ptr_obj)
        });

        add_native!(env, heap, "free", |args, e, h| {
            if args.len() != 1 {
                return Err("'free' requires 1 argument (raw pointer)".to_string());
            }

            let ptr_val = args[0];
            if let Some(LispExp::RawPtr(addr)) = h.get(ptr_val) {
                // Entregamos o endereço de volta ao C para libertar a RAM
                unsafe { free(*addr as *mut u8) };
                Ok(Value::void())
            } else {
                Err("'free' requires a valid raw pointer".to_string())
            }
        });

        add_native!(env, heap, "char->int", |args, e, h| {
            if args.len() != 1 {
                return Err("char->int expects 1 argument (string)".to_string());
            }

            if args[0].is_gc_ref() {
                if let Some(LispExp::Str(s)) = h.get(args[0]) {
                    if let Some(c) = s.chars().next() {
                        return Ok(Value::number(c as u32 as f64));
                    }
                }
            }
            Err("char->int expects a valid string".to_string())
        });
        add_native!(env, heap, "int->char", |args, e, h| {
            if args.len() != 1 {
                return Err("int->char expects 1 argument (number)".to_string());
            }

            let code = args[0].as_number() as u32;
            if let Some(c) = std::char::from_u32(code) {
                let s = c.to_string();
                return Ok(h.alloc_string(s));
            }

            Err("int->char received an invalid char code".to_string())
        });

        add_native!(env, heap, "hex", |args, _, h| {
            if args.len() != 1 {
                return Err("'hex' requires 1 argument".to_string());
            }

            let bits: u64 = unsafe { std::mem::transmute(args[0]) };
            let hex_str = format!("0x{:x}", bits);
            Ok(h.alloc_string(hex_str))
        });

        add_native!(env, heap, "bits->int", |args, _, _| {
            if args.len() != 1 {
                return Err("bits->int requires 1 argument".to_string());
            }

            let bits: u64 = unsafe { std::mem::transmute(args[0]) };
            Ok(Value::number(bits as f64))
        });

        add_native!(env, heap, "zero-mem", |args, _, h| {
            if args.len() != 2 {
                return Err("zero-mem requires 2 args (ptr tamanho_em_bytes)".to_string());
            }

            let ptr_val = args[0];
            let size = args[1].as_number() as usize;

            if let Some(LispExp::RawPtr(addr)) = h.get(ptr_val) {
                // write_bytes é a ponte do Rust para o 'memset'
                unsafe {
                    std::ptr::write_bytes(*addr as *mut u8, 0, size);
                }

                Ok(Value::void())
            } else {
                Err("zero-mem requires a valid raw pointer".to_string())
            }
        });

        add_native!(env, heap, "ffi-open", |args, _, h| {
            if let Some(LispExp::Str(path)) = h.get(args[0]) {
                let lib = unsafe { Library::new(path).map_err(|e| e.to_string())? };
                // Escondemos o Library num ponteiro bruto para o Haki segurar
                let ptr = Box::into_raw(Box::new(lib)) as usize;
                return Ok(h.alloc(LispExp::RawPtr(ptr)));
            }
            Err("ffi-open expects the library path (ex: 'raylib.a')".to_string())
        });

        add_native!(env, heap, "ffi-sym", |args, _, h| {
            if let (Some(LispExp::RawPtr(lib_ptr)), Some(LispExp::Str(sym_name))) =
                (h.get(args[0]), h.get(args[1]))
            {
                let lib = unsafe { &*(*lib_ptr as *const Library) };

                let sym: Symbol<unsafe extern "C" fn()> =
                    unsafe { lib.get(sym_name.as_bytes()).map_err(|e| e.to_string())? };

                // Pegamos o endereço de memória real da função C
                let func_ptr = *sym as usize;
                return Ok(h.alloc(LispExp::RawPtr(func_ptr)));
            }
            Err("ffi-sym expects a raw ptr (library) and a String (function name)".to_string())
        });

        add_native!(env, heap, "ffi-call", |args, e, h| {
            fn parse_ffi_type(t_str: &str) -> Type {
                if t_str.starts_with("struct:") {
                    // Separa "struct:uint,int" -> ["uint", "int"] e converte recursivamente
                    let fields: Vec<Type> =
                        t_str[7..].split(',').map(|s| parse_ffi_type(s)).collect();
                    Type::structure(fields)
                } else {
                    match t_str {
                        "int" => Type::i32(),
                        "uint" => Type::u32(),
                        "float" => Type::f32(),
                        "bool" => Type::u8(),
                        "pointer" => Type::pointer(),
                        _ => Type::void(),
                    }
                }
            }

            if args.len() != 4 {
                return Err(
                    "ffi-call expects 4 args: (ptr ret-type arg-types arg-values)".to_string(),
                );
            }

            // 1. O Ponteiro da Função C e o Tipo de Retorno esperado
            let func_ptr = if let Some(LispExp::RawPtr(p)) = h.get(args[0]) {
                *p
            } else {
                return Err("Invalid pointer".to_string());
            };
            let ret_type_str = if let Some(LispExp::Str(s)) = h.get(args[1]) {
                s.clone()
            } else {
                "void".to_string()
            };

            // 2. Extrair a lista de Tipos (Ex: "int", "float", "string") do Heap
            let mut type_strs = vec![];
            if let Some(exp) = h.get(args[2]) {
                let t_list = if let LispExp::Pair(_, _) = exp {
                    pairs_to_vec(exp, h)
                } else {
                    exp.clone()
                };
                if let LispExp::List(items, _) = t_list {
                    for item in items {
                        if let LispExp::Str(s) = item {
                            type_strs.push(s);
                        }
                    }
                }
            }

            // 3. Extrair a lista de Valores reais do Haki
            let mut arg_vals = vec![];
            if let Some(exp) = h.get(args[3]) {
                let v_list = if let LispExp::Pair(_, _) = exp {
                    pairs_to_vec(exp, h)
                } else {
                    exp.clone()
                };
                if let LispExp::List(items, _) = v_list {
                    for item in items {
                        // Força a avaliação das sub-expressões para obtermos os Values finais
                        arg_vals.push(ast_to_value(&item, h));
                    }
                }
            }

            if type_strs.len() != arg_vals.len() {
                return Err("ffi-call: The number of types does not match the values".to_string());
            }

            // ========================================================================
            // O COFRE DE MEMÓRIA (Impede que o Rust destrua os dados antes do C rodar!)
            // ========================================================================
            let mut i32_store: Vec<Box<i32>> = vec![];
            let mut u32_store: Vec<Box<u32>> = vec![];
            let mut f32_store: Vec<Box<f32>> = vec![];
            let mut cstring_store: Vec<CString> = vec![];
            let mut ptr_store: Vec<Box<*const std::ffi::c_char>> = vec![];
            let mut raw_ptr_store: Vec<Box<usize>> = vec![];
            let mut struct_16_store: Vec<Box<[u8; 16]>> = vec![];
            let mut struct_20_store: Vec<Box<[u8; 20]>> = vec![];
            let mut struct_40_store: Vec<Box<[u8; 40]>> = vec![];

            let mut ffi_types = vec![];

            // 4. Mapear os tipos e fixar os valores no Cofre
            for (t_str, val) in type_strs.iter().zip(arg_vals.iter()) {
                if t_str.starts_with("struct:") {
                    let p = if let Some(LispExp::RawPtr(ptr_val)) = h.get(*val) {
                        *ptr_val
                    } else {
                        0
                    };

                    // Calculamos o tamanho simplificado (4 bytes por campo)
                    let field_count = t_str[7..].split(',').count();
                    let size = field_count * 4;

                    // Lemos a memória bruta do ponteiro dinamicamente
                    unsafe {
                        match size {
                            16 => {
                                let mut buf = Box::new([0u8; 16]);
                                std::ptr::copy_nonoverlapping(p as *const u8, buf.as_mut_ptr(), 16);
                                struct_16_store.push(buf);
                            }
                            20 => {
                                // Tamanho exato de uma Texture2D genérica
                                let mut buf = Box::new([0u8; 20]);
                                std::ptr::copy_nonoverlapping(p as *const u8, buf.as_mut_ptr(), 20);
                                struct_20_store.push(buf);
                            }
                            40 => {
                                let mut buf = Box::new([0u8; 40]);
                                std::ptr::copy_nonoverlapping(p as *const u8, buf.as_mut_ptr(), 40);
                                struct_40_store.push(buf);
                            }
                            _ => {
                                return Err(format!("Struct size {} not supported yet", size));
                            }
                        }
                    }
                    ffi_types.push(parse_ffi_type(t_str));
                    continue;
                }

                match t_str.as_str() {
                    "int" => {
                        let v = val.as_number() as i32;
                        i32_store.push(Box::new(v));
                        ffi_types.push(Type::i32());
                    }
                    "uint" => {
                        let v = val.as_number() as u32;
                        u32_store.push(Box::new(v));
                        ffi_types.push(Type::u32());
                    }
                    "float" => {
                        let v = val.as_number() as f32;
                        f32_store.push(Box::new(v));
                        ffi_types.push(Type::f32());
                    }
                    "string" => {
                        let s = if let Some(LispExp::Str(str_val)) = h.get(*val) {
                            str_val.clone()
                        } else {
                            "".to_string()
                        };
                        let cstr = CString::new(s).unwrap();
                        ptr_store.push(Box::new(cstr.as_ptr()));
                        cstring_store.push(cstr); // Mantém a string viva na RAM
                        ffi_types.push(Type::pointer());
                    }
                    "pointer" => {
                        let p = if let Some(LispExp::RawPtr(ptr_val)) = h.get(*val) {
                            *ptr_val
                        } else {
                            0
                        };
                        raw_ptr_store.push(Box::new(p));
                        ffi_types.push(Type::pointer());
                    }
                    _ => return Err(format!("Unknown FFI type in Haki: {}", t_str)),
                }
            }

            // Construir os Argumentos (Agarrando as referências protegidas do Cofre)
            let mut ffi_args = vec![];
            let mut i32_idx = 0;
            let mut u32_idx = 0;
            let mut f32_idx = 0;
            let mut ptr_idx = 0;
            let mut raw_idx = 0;
            let mut s16_idx = 0;
            let mut s20_idx = 0;
            let mut s40_idx = 0;

            for t_str in &type_strs {
                if t_str.starts_with("struct:") {
                    let field_count = t_str[7..].split(',').count();
                    match field_count * 4 {
                        16 => {
                            ffi_args.push(Arg::new(&*struct_16_store[s16_idx]));
                            s16_idx += 1;
                        }
                        20 => {
                            ffi_args.push(Arg::new(&*struct_20_store[s20_idx]));
                            s20_idx += 1;
                        }
                        40 => {
                            ffi_args.push(Arg::new(&*struct_40_store[s40_idx]));
                            s40_idx += 1;
                        }
                        _ => {}
                    }
                    continue;
                }

                match t_str.as_str() {
                    "int" => {
                        ffi_args.push(Arg::new(&*i32_store[i32_idx]));
                        i32_idx += 1;
                    }
                    "uint" => {
                        ffi_args.push(Arg::new(&*u32_store[u32_idx]));
                        u32_idx += 1;
                    }
                    "float" => {
                        ffi_args.push(Arg::new(&*f32_store[f32_idx]));
                        f32_idx += 1;
                    }
                    "string" => {
                        ffi_args.push(Arg::new(&*ptr_store[ptr_idx]));
                        ptr_idx += 1;
                    }
                    "pointer" => {
                        ffi_args.push(Arg::new(&*raw_ptr_store[raw_idx]));
                        raw_idx += 1;
                    }
                    _ => {}
                }
            }

            // Configurar o Tipo de Retorno esperado pelo Lisp
            // let ret_type = match ret_type_str.as_str() {
            //     "int" => Type::i32(),
            //     "float" => Type::f32(),
            //     "pointer" => Type::pointer(),
            //     "bool" => Type::u8(),
            //     _ => Type::void(),
            // };
            let ret_type = parse_ffi_type(&ret_type_str);

            // Compilar a Assinatura (CIF) e Executar no Processador
            let cif = Cif::new(ffi_types.into_iter(), ret_type);
            let code_ptr = libffi::middle::CodePtr::from_ptr(func_ptr as *mut _);

            unsafe {
                if ret_type_str.starts_with("struct:") {
                    let field_count = ret_type_str[7..].split(',').count();
                    let size = field_count * 4;

                    // O C devolve a Struct Genérica, nós copiamos os bytes para um RawPtr novo
                    let ptr =
                        std::alloc::alloc(std::alloc::Layout::from_size_align(size, 4).unwrap());

                    match size {
                        16 => {
                            let res: [u8; 16] = cif.call(code_ptr, ffi_args.as_slice());
                            std::ptr::copy_nonoverlapping(res.as_ptr(), ptr, 16);
                        }
                        20 => {
                            let res: [u8; 20] = cif.call(code_ptr, ffi_args.as_slice());
                            std::ptr::copy_nonoverlapping(res.as_ptr(), ptr, 20);
                        }
                        40 => {
                            let res: [u8; 40] = cif.call(code_ptr, ffi_args.as_slice());
                            std::ptr::copy_nonoverlapping(res.as_ptr(), ptr, 40);
                        }
                        _ => return Err("Generic struct return failed".to_string()),
                    }
                    return Ok(h.alloc(LispExp::RawPtr(ptr as usize)));
                }
                match ret_type_str.as_str() {
                    "int" => {
                        let res: i32 = cif.call(code_ptr, ffi_args.as_slice());
                        Ok(Value::number(res as f64))
                    }
                    "uint" => {
                        let res: u32 = cif.call(code_ptr, ffi_args.as_slice());
                        Ok(Value::number(res as f64))
                    }
                    "float" => {
                        let res: f32 = cif.call(code_ptr, ffi_args.as_slice());
                        Ok(Value::number(res as f64))
                    }
                    "pointer" => {
                        let res: usize = cif.call(code_ptr, ffi_args.as_slice());
                        // Alocamos o ponteiro retornado pelo C no nosso heap e devolvemos ao Lisp
                        Ok(h.alloc(LispExp::RawPtr(res)))
                    }
                    "bool" => {
                        let res: u8 = cif.call(code_ptr, ffi_args.as_slice());
                        Ok(Value::number(res as f64))
                    }
                    _ => {
                        cif.call::<()>(code_ptr, ffi_args.as_slice());
                        Ok(Value::void())
                    }
                }
            }
        });

        add_native!(env, heap, "struct->c", |args, e, h| {
            if args.len() != 2 {
                return Err("struct->c expects a struct and a list of types".to_string());
            }

            let struct_val = args[0];

            // Pegamos os VALORES da LispExp::Struct
            let campos = if let Some(LispExp::Struct(_, _, vals)) = h.get(struct_val) {
                vals.clone()
            } else {
                return Err("The first argument should be a struct".to_string());
            };

            // Extraímos a "Assinatura de Memória" (ex: '("float" "float" "int"))
            let mut type_strs = vec![];
            if let Some(exp) = h.get(args[1]) {
                let t_list = if let LispExp::Pair(_, _) = exp {
                    crate::helpers::pairs_to_vec(exp, h)
                } else {
                    exp.clone()
                };
                if let LispExp::List(items, _) = t_list {
                    for item in items {
                        if let LispExp::Str(s) = item {
                            type_strs.push(s);
                        }
                    }
                }
            }

            // Calculamos o tamanho total necessário na memória (em bytes)
            let mut offset = 0;
            for t in &type_strs {
                match t.as_str() {
                    "int" | "uint" | "float" => offset += 4,
                    "double" | "pointer" => offset += 8,
                    "byte" => offset += 1,
                    _ => offset += 4, // Alinhamento padrão simplificado
                }
            }

            // Alocamos a memória física real!
            let raw_ptr = unsafe {
                std::alloc::alloc(std::alloc::Layout::from_size_align(offset, 4).unwrap())
            };

            // Escrevemos os campos nativos do Haki diretamente nos bytes do C
            let mut current_offset = 0;
            for (i, t) in type_strs.iter().enumerate() {
                let physical_addr = raw_ptr as usize + current_offset;
                let val = campos[i];

                unsafe {
                    match t.as_str() {
                        "int" => {
                            *(physical_addr as *mut i32) = val.as_number() as i32;
                            current_offset += 4;
                        }
                        "uint" => {
                            *(physical_addr as *mut u32) = val.as_number() as u32;
                            current_offset += 4;
                        }
                        "float" => {
                            *(physical_addr as *mut f32) = val.as_number() as f32;
                            current_offset += 4;
                        }
                        "byte" => {
                            *(physical_addr as *mut u8) = val.as_number() as u8;
                            current_offset += 1;
                        }
                        "pointer" => {
                            *(physical_addr as *mut u8) = val.as_number() as u8;
                            current_offset += 8;
                        }
                        // Adicione ponteiros se necessário
                        _ => {}
                    }
                }
            }

            // Devolvemos o raw pointer
            Ok(h.alloc(LispExp::RawPtr(raw_ptr as usize)))
        });
    }

    lisp_env
}

fn json_to_lisp(value: &serde_json::Value, heap: &mut Heap) -> Value {
    match value {
        serde_json::Value::Null => Value::nil(),
        serde_json::Value::Bool(b) => Value::boolean(*b),
        serde_json::Value::Number(num) => Value::number(num.as_f64().unwrap_or(0.0)),
        serde_json::Value::String(s) => heap.alloc_string(s.clone()),
        serde_json::Value::Array(arr) => {
            let vec: Vec<Value> = arr.iter().map(|item| json_to_lisp(item, heap)).collect();
            heap.alloc(LispExp::Vector(vec))
        }
        serde_json::Value::Object(obj) => {
            let mut map = RustHashMap::new();
            for (k, v) in obj {
                map.insert(k.clone(), json_to_lisp(v, heap));
            }
            heap.alloc(LispExp::HashMap(map))
        }
    }
}
