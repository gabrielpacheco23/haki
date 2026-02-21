use crate::eval;
use crate::heap::Heap;
use crate::value::Value;
use crate::vm::CallFrame;
use crate::{
    env::{Env, LispEnv},
    expr::*,
};

#[macro_export]
macro_rules! arithmetic_op {
    ($env:ident, $heap:ident, $name:expr, $init:expr, $op:tt) => {
        add_native!($env, $heap, $name, |args, _env, _heap| {
            if args.is_empty() { return Ok(Value::number($init)); }
            if !args[0].is_number() { return Err(format!("'{}' requires numbers", $name)); }
            let mut result = args[0].as_number();

            if args.len() == 1 {
                if $name == "-" { result = -result; }
                else if $name == "/" { result = 1.0 / result; }
            } else {
                for arg in args.iter().skip(1) {
                    if !arg.is_number() { return Err(format!("'{}' requires numbers", $name)); }
                    result = result $op arg.as_number();
                }
            }
            Ok(Value::number(result))
        });
    };
}

#[macro_export]
macro_rules! compare_op {
    ($env:ident, $heap:ident, $name:expr, $op:tt) => {
        add_native!($env, $heap, $name, |args, _env, _heap| {
            if args.len() < 2 { return Err(format!("'{}' requires at least 2 numbers", $name)); }

            for window in args.windows(2) {
                if !window[0].is_number() || !window[1].is_number() {
                    return Err(format!("'{}' requires numbers", $name));
                }
                if !(window[0].as_number() $op window[1].as_number()) {
                    return Ok(Value::boolean(false));
                }
            }
            Ok(Value::boolean(true))
        });
    };
}

#[macro_export]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[LispExp]| -> Result<LispExp, String> {
            let floats = parse_list_of_floats(args)?;
            let first = floats
                .first()
                .ok_or(LispErr::Reason("expected at least one number".to_string()))?;
            let rest = &floats[1..];
            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            };
            Ok(LispExp::Bool(f(first, rest)))
        }
    }};
}

// Macro para funções matemáticas de 1 argumento (sin, cos, sqrt)
#[macro_export]
macro_rules! def_math {
    ($env:ident, $heap:ident, $name:expr, $method:ident) => {
        add_native!($env, $heap, $name, |args, _env, _heap| {
            if args.len() != 1 {
                return Err(format!("'{}' exige exatamente 1 argumento.", $name));
            }
            if !args[0].is_number() {
                return Err(format!("'{}' exige um número.", $name));
            }
            // Chama o método f64 do Rust (ex: args[0].as_number().sqrt()) e empacota de volta
            let result = args[0].as_number().$method();
            Ok(Value::number(result))
        });
    };
}

// Macro para operações de "Fold" (Redução) (+, *, min, max)
// Recebe um valor inicial e uma função de acumulação
#[macro_export]
macro_rules! def_fold {
    ($env:ident, $heap:ident, $name:expr, $init:expr, $fold_op:expr) => {
        add_native!($env, $heap, $name, |args, _env, h| {
            if args.is_empty() {
                return Ok($init);
            }

            let mut acc = args[0];

            for arg in args.iter().skip(1) {
                acc = $fold_op(acc, *arg, &mut *h)?;
            }

            Ok(acc)
        });
    };
}

// MACRO: Redutor que exige pelo menos 1 argumento (ideal para min/max)
#[macro_export]
macro_rules! def_fold1 {
    ($env:ident, $heap:ident, $name:expr, $fold_op:expr) => {
        add_native!($env, $heap, $name, |args, _env, h| {
            // A diferença está aqui: barramos listas vazias!
            if args.is_empty() {
                return Err(format!("'{}' exige pelo menos 1 argumento.", $name));
            }

            let mut acc = args[0];

            for arg in args.iter().skip(1) {
                acc = $fold_op(acc, *arg, &mut *h)?;
            }

            Ok(acc)
        });
    };
}

#[macro_export]
macro_rules! def_is {
    ($env:ident, $heap:ident, $name:expr, $check:expr) => {
        add_native!($env, $heap, $name, |args, _env, heap| {
            if args.len() != 1 {
                return Err(format!("'{}' exige exatamente 1 argumento.", $name));
            }
            // Executa a lógica do closure passado na macro
            let is_true = $check(&args[0], heap);
            Ok(Value::boolean(is_true))
        });
    };
}

pub fn apply_macro(
    macro_def: &LispLambda,
    args: &[LispExp],
    _env: &mut Env,
    heap: &mut Heap,
) -> Result<LispExp, String> {
    let mut expansion_env = LispEnv::new(Some(macro_def.env.clone()));

    if let LispExp::List(params, _) = &*macro_def.params {
        let has_varargs = params.len() >= 2
            && matches!(&params[params.len() - 2], LispExp::Symbol(s, _) if s == "&rest");

        if has_varargs {
            let fixed_params_count = params.len() - 2;
            if args.len() < fixed_params_count {
                return Err("Insufficient arguments in variadic macro".to_string());
            }

            for i in 0..fixed_params_count {
                if let LispExp::Symbol(name, _) = &params[i] {
                    let arg_val = ast_to_value(&args[i], heap);
                    expansion_env.borrow_mut().insert(name.clone(), arg_val);
                }
            }

            let vararg_name_exp = &params[params.len() - 1];
            if let LispExp::Symbol(name, _) = vararg_name_exp {
                let rest_args = args[fixed_params_count..].to_vec();
                let rest_list = LispExp::List(rest_args, 0);
                let rest_val = ast_to_value(&rest_list, heap);
                expansion_env.borrow_mut().insert(name.clone(), rest_val);
            }
        } else {
            if params.len() != args.len() {
                return Err(format!(
                    "Macro expects {} arguments, got {}",
                    params.len(),
                    args.len()
                ));
            }
            for (param, arg) in params.iter().zip(args.iter()) {
                if let LispExp::Symbol(name, _) = param {
                    let arg_val = ast_to_value(arg, heap);
                    expansion_env.borrow_mut().insert(name.clone(), arg_val);
                }
            }
        }
    } else {
        return Err("Invalid parameters".to_string());
    }

    eval((*macro_def.body).clone(), &mut expansion_env, heap)
}

pub fn apply_procedure(
    proc: &LispExp,
    args: &[LispExp],
    env: &mut Env,
    heap: &mut Heap,
) -> Result<LispExp, String> {
    match proc {
        LispExp::Native(func) => {
            let mut value_args = Vec::with_capacity(args.len());
            for arg in args {
                value_args.push(ast_to_value(arg, heap));
            }
            let mut temp_env = env.clone();
            let result_val = func(&value_args, &mut temp_env, heap)?;
            Ok(value_to_ast(result_val, heap))
        }
        LispExp::Lambda(lambda) => {
            let mut new_env = LispEnv::new(Some(lambda.env.clone()));

            let params_flat = pairs_to_vec(&*lambda.params, heap);

            if let LispExp::List(params, _) = params_flat {
                for (param, arg) in params.iter().zip(args.iter()) {
                    if let LispExp::Symbol(name, _) = param {
                        new_env
                            .borrow_mut()
                            .insert(name.clone(), ast_to_value(arg, heap));
                    }
                }
            }
            eval((*lambda.body).clone(), &mut new_env, heap)
        }
        LispExp::VmClosure {
            params,
            chunk,
            upvalues,
        } => {
            if args.len() != params.len() {
                return Err(format!(
                    "Incorrect arity. Expected {}, got {}",
                    params.len(),
                    args.len()
                ));
            }

            let mut sub_vm = crate::vm::Vm::new();

            sub_vm.stack.push(Value::void());
            for arg in args {
                sub_vm.stack.push(ast_to_value(arg, heap));
            }

            sub_vm.frames.push(CallFrame {
                chunk: chunk.clone(),
                ip: 0,
                stack_offset: 1, // Os argumentos começam no índice 1
                closure_upvalues: upvalues.clone(),
            });

            let result_val = sub_vm.execute(chunk.clone(), env.clone(), heap, false)?;
            Ok(value_to_ast(result_val, heap))
        }
        _ => Err(format!(
            "Object'{}' is not callable in eval",
            lisp_fmt(ast_to_value(proc, heap), heap)
        )),
    }
}

pub fn expand_quasiquote(exp: &LispExp, env: &mut Env, heap: &mut Heap) -> Result<LispExp, String> {
    match exp {
        // Se encontrar (,algo), avalia o 'algo'
        LispExp::List(l, _) if l.len() == 2 && is_symbol(&l[0], "unquote") => {
            eval(l[1].clone(), env, heap)
        }
        // Recursão em listas
        LispExp::List(l, line) => {
            let mut new_list = Vec::with_capacity(l.len());
            for item in l {
                new_list.push(expand_quasiquote(item, env, heap)?);
            }
            Ok(LispExp::List(new_list, *line))
        }
        _ => Ok(exp.clone()),
    }
}

// pub fn expand_quasiquote(exp: &LispExp, env: &mut Env, heap: &mut Heap) -> Result<LispExp, String> {
//     match exp {
//         LispExp::List(l, _) if l.len() == 2 && is_symbol(&l[0], "unquote") => {
//             eval(l[1].clone(), env, heap)
//         }
//         LispExp::List(l, line) => {
//             let mut new_list = vec![];
//             for item in l {
//                 new_list.push(expand_quasiquote(item, env, heap)?);
//             }
//             Ok(LispExp::List(new_list, *line))
//         }
//         _ => Ok(exp.clone()),
//     }
// }

fn is_symbol(exp: &LispExp, name: &str) -> bool {
    matches!(exp, LispExp::Symbol(s, _) if s == name)
}

pub fn expand_macros(ast: LispExp, env: &mut Env, heap: &mut Heap) -> Result<LispExp, String> {
    // Primeiro, garantimos que a árvore está em formato de Listas nativas
    let ast = pairs_to_vec(&ast, heap);

    match ast {
        LispExp::List(list, line) => {
            if list.is_empty() {
                return Ok(LispExp::List(list, line));
            }

            if let LispExp::Symbol(s, _) = &list[0] {
                // Comandos especiais que não devem expandir seus cabeçalhos
                match s.as_str() {
                    "defmacro" => {
                        eval(LispExp::List(list, line), env, heap)?;
                        return Ok(LispExp::Void);
                    }
                    "quote" => return Ok(LispExp::List(list, line)),
                    "lambda" | "λ" | "define" => {
                        if list.len() < 3 {
                            return Err(format!("Malformed {}", s));
                        }
                        let mut new_list = vec![list[0].clone(), list[1].clone()];
                        for item in &list[2..] {
                            new_list.push(expand_macros(item.clone(), env, heap)?);
                        }
                        return Ok(LispExp::List(new_list, line));
                    }
                    _ => {}
                }

                // Tenta expandir se for uma Macro
                if let Some(macro_def) = find_macro(env, s, heap) {
                    let expanded_ast = apply_macro(&macro_def, &list[1..], env, heap)?;
                    // Recursão: a macro pode retornar outra macro!
                    return expand_macros(expanded_ast, env, heap);
                }
            }

            // Se não for macro, expande os itens internos
            let mut new_list = Vec::with_capacity(list.len());
            for item in list {
                new_list.push(expand_macros(item, env, heap)?);
            }
            Ok(LispExp::List(new_list, line))
        }
        _ => Ok(ast),
    }
}

// pub fn expand_macros(ast: LispExp, env: &mut Env, heap: &mut Heap) -> Result<LispExp, String> {
//     let ast = pairs_to_vec(&ast, heap);

//     match ast {
//         LispExp::List(list, line) => {
//             if list.is_empty() {
//                 return Ok(LispExp::List(list, line));
//             }

//             if let LispExp::Symbol(s, _) = &list[0] {
//                 if s == "defmacro" {
//                     eval(LispExp::List(list.clone(), line), env, heap)?;
//                     return Ok(LispExp::Void);
//                 }
//                 if s == "quote" {
//                     return Ok(LispExp::List(list.clone(), line));
//                 }
//                 if s == "lambda" {
//                     if list.len() < 3 {
//                         return Err("Malformed lambda".to_string());
//                     }
//                     let mut new_list = vec![list[0].clone(), list[1].clone()];
//                     for item in &list[2..] {
//                         new_list.push(expand_macros(item.clone(), env, heap)?);
//                     }
//                     return Ok(LispExp::List(new_list, line));
//                 }
//                 if s == "define" {
//                     if list.len() < 3 {
//                         return Err("Malformed define".to_string());
//                     }
//                     let mut new_list = vec![list[0].clone(), list[1].clone()];
//                     for item in &list[2..] {
//                         new_list.push(expand_macros(item.clone(), env, heap)?);
//                     }
//                     return Ok(LispExp::List(new_list, line));
//                 }

//                 if let Some(macro_def) = find_macro(env, s, heap) {
//                     let expanded_ast = apply_macro(&macro_def, &list[1..], env, heap)?;
//                     let mut fully_expanded = expand_macros(expanded_ast, env, heap)?;

//                     if let LispExp::List(_, ref mut expanded_line) = fully_expanded {
//                         if *expanded_line == 0 {
//                             *expanded_line = line;
//                         }
//                     }
//                     return Ok(fully_expanded);
//                 }
//             }

//             let mut new_list = vec![];
//             for item in list {
//                 new_list.push(expand_macros(item, env, heap)?);
//             }
//             Ok(LispExp::List(new_list, line))
//         }
//         _ => Ok(ast),
//     }
// }

fn find_macro(env: &Env, name: &str, heap: &Heap) -> Option<LispLambda> {
    let env_ref = env.borrow();

    if let Some(val) = env_ref.get(name) {
        if val.is_gc_ref() {
            if let Some(LispExp::Macro(m)) = heap.get(val) {
                return Some(m.clone());
            }
        }
    }

    if let Some(outer) = &env_ref.outer {
        return find_macro(outer, name, heap);
    }
    None
}

pub fn pairs_to_vec(exp: &LispExp, heap: &Heap) -> LispExp {
    match exp {
        LispExp::Pair(car_val, cdr_val) => {
            let mut vec = vec![];

            let car_ast = value_to_ast(*car_val, heap);
            vec.push(pairs_to_vec(&car_ast, heap));

            let mut current = *cdr_val;
            while !current.is_nil() {
                if current.is_gc_ref() {
                    match heap.get(current) {
                        Some(LispExp::Pair(next_car, next_cdr)) => {
                            let next_car_ast = value_to_ast(*next_car, heap);
                            vec.push(pairs_to_vec(&next_car_ast, heap));
                            current = *next_cdr;
                            continue;
                        }
                        Some(LispExp::List(l, _)) => {
                            for item in l {
                                vec.push(pairs_to_vec(item, heap));
                            }
                            break;
                        }
                        _ => {}
                    }
                }
                let curr_ast = value_to_ast(current, heap);
                vec.push(pairs_to_vec(&curr_ast, heap));
                break;
            }
            LispExp::List(vec, 0)
        }
        LispExp::List(vec, line) => {
            let new_vec = vec.iter().map(|item| pairs_to_vec(item, heap)).collect();
            LispExp::List(new_vec, *line)
        }
        _ => exp.clone(),
    }
}

pub fn vec_to_pairs(exp: &LispExp, heap: &mut Heap) -> Value {
    match exp {
        LispExp::List(vec, _) => {
            let mut result = Value::nil();
            for item in vec.iter().rev() {
                let item_val = if let LispExp::List(_, _) = item {
                    vec_to_pairs(item, heap)
                } else {
                    ast_to_value(item, heap)
                };
                result = heap.alloc(LispExp::Pair(item_val, result));
            }
            result
        }
        _ => ast_to_value(exp, heap),
    }
}

pub fn value_to_ast(val: Value, heap: &Heap) -> LispExp {
    if val.is_number() {
        LispExp::Number(val.as_number())
    } else if val.is_boolean() {
        LispExp::Bool(val.as_boolean())
    } else if val.is_nil() {
        LispExp::Nil
    } else if val.is_void() {
        LispExp::Void
    } else if val.is_gc_ref() {
        heap.get(val).cloned().unwrap_or(LispExp::Void)
    } else {
        LispExp::Void
    }
}

pub fn ast_to_value(exp: &LispExp, heap: &mut Heap) -> Value {
    match exp {
        LispExp::Number(n) => Value::number(*n),
        LispExp::Bool(b) => Value::boolean(*b),
        LispExp::Nil => Value::nil(),
        LispExp::Void => Value::void(),
        LispExp::HeapPtr(val) => *val,
        _ => heap.alloc(exp.clone()),
    }
}
