use crate::value::Value;
use crate::{arithmetic_op, def_fold1};
use rand::RngExt;
use serde_json::Value as jsonValue;

use crate::compiler::compile;
use crate::heap::{Heap, collect_garbage};
use crate::helpers::{apply_procedure, ast_to_value, expand_macros, pairs_to_vec, vec_to_pairs};
use crate::vm::{Chunk, OpCode, Vm};
use crate::{compare_op, def_fold, def_is, def_math};
use crate::{expr::*, run_script};
use std::cell::RefCell;
use std::collections::HashSet;
use std::time::Duration;
use std::{collections::HashMap as RustHashMap, rc::Rc};

use std::process::Command;

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
}

impl LispEnv {
    pub fn new(outer: Option<Env>) -> Env {
        Rc::new(RefCell::new(LispEnv {
            data: RustHashMap::new(),
            loaded_files: HashSet::new(),
            outer,
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

        add_native!(env, heap, "car", |args, _, heap| {
            if args.is_empty() {
                return Err("'car' requires 1 argument".to_string());
            }

            if args[0].is_gc_ref() {
                if let Some(LispExp::Pair(car, _cdr)) = heap.get(args[0]) {
                    return Ok(*car);
                }
            }
            Err("'car' requires a pair or list".to_string())
        });

        add_native!(env, heap, "cdr", |args, _, heap| {
            if args.is_empty() {
                return Err("'cdr' requires 1 argument".to_string());
            }

            if args[0].is_gc_ref() {
                if let Some(LispExp::Pair(_car, cdr)) = heap.get(args[0]) {
                    return Ok(*cdr);
                }
            }
            Err("'cdr' requires a pair or list".to_string())
        });

        // def_fold!(env, heap, "max", |acc: Value, next: Value, _heap| {});

        add_native!(env, heap, "expt", |args, _, _| {
            if args.len() != 2 || !args[0].is_number() || !args[1].is_number() {
                return Err("expt requires 2 numbers".to_string());
            }
            Ok(Value::number(args[0].as_number().powf(args[1].as_number())))
        });

        def_is!(env, heap, "number?", |v: &Value, _| v.is_number());
        def_is!(env, heap, "boolean?", |v: &Value, _| v.is_boolean());
        def_is!(env, heap, "null?", |v: &Value, _| v.is_nil());
        def_is!(env, heap, "pair?", |v: &Value, h: &mut Heap| {
            v.is_gc_ref() && matches!(h.get(*v), Some(LispExp::Pair(_, _)))
        });

        add_native!(env, heap, "list?", |args, _, heap| {
            if args.len() != 1 {
                return Err("'list?' requires 1 argument".to_string());
            }
            let mut current = args[0];
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
            v.is_gc_ref() && matches!(h.get(*v), Some(LispExp::Symbol(_)))
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

        add_native!(env, heap, "display", |args, _, heap| {
            use std::io::Write;
            for arg in args {
                print!("{}", lisp_fmt(*arg, heap));
            }
            std::io::stdout().flush().unwrap();
            Ok(Value::void())
        });

        add_native!(env, heap, "displayln", |args, _, heap| {
            for arg in args {
                print!("{}", lisp_fmt(*arg, heap));
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
            Ok(heap.alloc(LispExp::Str(result)))
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
                Ok(heap.alloc(LispExp::Str(s.to_uppercase())))
            } else {
                Err("'to-upper' requires a string".to_string())
            }
        });

        add_native!(env, heap, "to-lower", |args, _, heap| {
            if args.len() != 1 || !args[0].is_gc_ref() {
                return Err("'to-lower' requires 1 string".to_string());
            }
            if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                Ok(heap.alloc(LispExp::Str(s.to_lowercase())))
            } else {
                Err("'to-lower' requires a string".to_string())
            }
        });

        add_native!(env, heap, "string-trim", |args, _, heap| {
            if args.len() != 1 || !args[0].is_gc_ref() {
                return Err("'string-trim' requires a string".to_string());
            }
            if let Some(LispExp::Str(s)) = heap.get(args[0]) {
                Ok(heap.alloc(LispExp::Str(s.trim().to_string())))
            } else {
                Err("'string-trim' requires a string".to_string())
            }
        });

        add_native!(env, heap, "number->string", |args, _, heap| {
            if let Some(val) = args.first() {
                if val.is_number() {
                    let s = val.as_number().to_string();
                    return Ok(heap.alloc(LispExp::Str(s)));
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
                        return Ok(heap.alloc(LispExp::Str(subs.to_string())));
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
                        return Ok(heap.alloc(LispExp::Str(res)));
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
                            Ok(res) => Ok(heap.alloc(LispExp::Str(res))),
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
                        return Ok(heap.alloc(LispExp::Str(content)));
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
                        Some(LispExp::Symbol(_)) => "symbol",
                        Some(LispExp::Str(_)) => "string",
                        Some(LispExp::List(_)) => "list",
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
                return Ok(heap.alloc(LispExp::Str(type_str.to_string())));
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
                let key_str = if let Some(LispExp::Str(s) | LispExp::Symbol(s)) = heap.get(args[1])
                {
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
                let key_str = if let Some(LispExp::Str(s) | LispExp::Symbol(s)) = heap.get(args[1])
                {
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
                        let key_val = heap.alloc(LispExp::Str(key));
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
                let key_str = if let Some(LispExp::Str(s) | LispExp::Symbol(s)) = heap.get(args[i])
                {
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
                let num = rng.random_range(args[0].as_number()..args[1].as_number());
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
    }

    lisp_env
}

fn json_to_lisp(value: &serde_json::Value, heap: &mut Heap) -> Value {
    match value {
        serde_json::Value::Null => Value::nil(),
        serde_json::Value::Bool(b) => Value::boolean(*b),
        serde_json::Value::Number(num) => Value::number(num.as_f64().unwrap_or(0.0)),
        serde_json::Value::String(s) => heap.alloc(LispExp::Str(s.clone())),
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
