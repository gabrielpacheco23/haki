use std::rc::Rc;

use crate::eval;
use crate::vm::Vm;
use crate::{
    env::{Env, LispEnv},
    expr::*,
};

// Helper para converter qualquer número Lisp para f64
pub fn get_float(exp: &LispExp) -> Result<f64, String> {
    match exp {
        LispExp::Number(f) => Ok(*f),
        _ => Err(format!("Expected a number, got: {}", exp)),
    }
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
    ($func:expr) => {
        Rc::new(move |args: &[LispExp], _env| -> Result<LispExp, String> {
            if args.len() != 1 {
                return Err("This procedure requires exactly 1 argument".to_string());
            }
            let val = get_float(&args[0])?;
            Ok(LispExp::Number($func(val)))
        })
    };
}

#[macro_export]
macro_rules! def_cmp {
    ($op:tt) => {
        LispExp::Native(Rc::new(move |args: &[LispExp], _env: &mut Env| -> Result<LispExp, String> {
            let floats: Result<Vec<f64>, String> = args.iter().map(get_float).collect();
            let floats = floats?;

            if floats.len() < 2 {
                return Ok(LispExp::Bool(true));
            }

            for pair in floats.windows(2) {
                let prev = pair[0];
                let curr = pair[1];

                if !(prev $op curr) {
                    return Ok(LispExp::Bool(false));
                }
            }

            Ok(LispExp::Bool(true))
        }))
    };
}

// Macro para operações de "Fold" (Redução) (+, *, min, max)
// Recebe um valor inicial e uma função de acumulação
#[macro_export]
macro_rules! def_fold {
    ($initial:expr, $op:expr) => {
        Rc::new(move |args: &[LispExp], _env| -> Result<LispExp, String> {
            let mut acc = $initial;
            for (i, arg) in args.iter().enumerate() {
                let val = get_float(arg)?;
                if i == 0 && args.len() == 1 && $initial == 0.0 {
                    // Caso especial: (+ 5) ou (min 5) -> retorna 5
                    acc = val;
                } else if i == 0 {
                    // Se for o primeiro item, inicializamos o acumulador com ele
                    // (para min/max isso é importante, para + nem tanto)
                    acc = match $op(acc, val) {
                        // Truque para min/max funcionarem com o primeiro elemento
                        _ if $initial == f64::INFINITY || $initial == f64::NEG_INFINITY => val,
                        res => res,
                    };
                } else {
                    acc = $op(acc, val);
                }
            }
            Ok(LispExp::Number(acc))
        })
    };
}

#[macro_export]
macro_rules! def_is {
    // $pattern: O padrão do match (ex: LispExp::List(_))
    ($pattern:pat) => {
        Rc::new(|args: &[LispExp], _env| -> Result<LispExp, String> {
            if args.len() != 1 {
                return Err("Type check requires exactly 1 argument".to_string());
            }
            let res = matches!(&args[0], $pattern);
            Ok(LispExp::Bool(res))
        })
    };
    // Variante especial para validações mais complexas (ex: null?)
    ($pattern:pat if $guard:expr) => {
        Rc::new(|args: &[LispExp], _env| -> Result<LispExp, String> {
            if args.len() != 1 {
                return Err("Type check requires exactly 1 argument".to_string());
            }
            let res = match &args[0] {
                $pattern if $guard => true,
                _ => false,
            };
            Ok(LispExp::Bool(res))
        })
    };
}

pub fn apply_macro(
    macro_def: &LispLambda,
    args: &[LispExp],
    _env: &mut Env,
) -> Result<LispExp, String> {
    let mut expansion_env = LispEnv::new(Some(macro_def.env.clone()));

    if let LispExp::List(params) = &*macro_def.params {
        // Verifica se o último parâmetro é "&rest"
        let has_varargs = params.len() >= 2
            && matches!(&params[params.len() - 2], LispExp::Symbol(s) if s == "&rest");

        if has_varargs {
            // Verifica se tem o mínimo de argumentos necessários (antes do &rest)
            let fixed_params_count = params.len() - 2;
            if args.len() < fixed_params_count {
                return Err("Insufficient arguments to variadic macro".to_string());
            }

            // Bind dos argumentos fixos
            for i in 0..fixed_params_count {
                if let LispExp::Symbol(name) = &params[i] {
                    expansion_env
                        .borrow_mut()
                        .data
                        .insert(name.clone(), args[i].clone());
                }
            }

            // Bind do resto! Pega todos os args que sobraram e coloca numa lista
            let vararg_name_exp = &params[params.len() - 1];
            if let LispExp::Symbol(name) = vararg_name_exp {
                let rest_args = args[fixed_params_count..].to_vec();
                expansion_env
                    .borrow_mut()
                    .data
                    .insert(name.clone(), LispExp::List(rest_args));
            }
        } else {
            if params.len() != args.len() {
                return Err(format!(
                    "Macro expects {} arguments, but got {}",
                    params.len(),
                    args.len()
                ));
            }
            for (param, arg) in params.iter().zip(args.iter()) {
                if let LispExp::Symbol(name) = param {
                    expansion_env
                        .borrow_mut()
                        .data
                        .insert(name.clone(), arg.clone());
                }
            }
        }
    } else {
        return Err("Invalid parameters".to_string());
    }

    eval((*macro_def.body).clone(), &mut expansion_env)
}

pub fn apply_procedure(proc: &LispExp, args: &[LispExp], env: &mut Env) -> Result<LispExp, String> {
    match proc {
        LispExp::Native(f) => f(args, env),
        LispExp::Lambda(lambda) => {
            let new_env = LispEnv::new(Some(lambda.env.clone()));

            if let LispExp::List(params) = &*lambda.params {
                if params.len() != args.len() {
                    return Err(format!(
                        "Expected {} arguments, but got {}",
                        params.len(),
                        args.len()
                    ));
                }
                for (param, arg) in params.iter().zip(args.iter()) {
                    if let LispExp::Symbol(name) = param {
                        new_env.borrow_mut().data.insert(name.clone(), arg.clone());
                    }
                }
            } else {
                return Err("Invalid lambda parameters".to_string());
            }

            eval((*lambda.body).clone(), &mut new_env.clone())
        }
        LispExp::VmClosure {
            params,
            chunk,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err("Incorrect arity".to_string());
            }

            let new_env = LispEnv::new(Some(closure_env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                new_env.borrow_mut().data.insert(param.clone(), arg.clone());
            }

            let mut sub_vm = Vm::new();
            sub_vm.execute(chunk.clone(), new_env)
        }
        _ => Err(format!("Object not callable using eval: {}", proc)),
    }
}

pub fn expand_quasiquote(exp: &LispExp, env: &mut Env) -> Result<LispExp, String> {
    match exp {
        LispExp::List(l) if l.len() == 2 && is_symbol(&l[0], "unquote") => eval(l[1].clone(), env),
        LispExp::List(l) => {
            let mut new_list = vec![];
            for item in l {
                new_list.push(expand_quasiquote(item, env)?);
            }

            Ok(LispExp::List(new_list))
        }
        _ => Ok(exp.clone()),
    }
}

fn is_symbol(exp: &LispExp, name: &str) -> bool {
    matches!(exp, LispExp::Symbol(s) if s == name)
}

pub fn env_set(env: &Env, var: &str, val: LispExp) -> Result<(), String> {
    // 1. Tenta encontrar e atualizar no escopo ATUAL
    {
        let mut env_ref = env.borrow_mut();
        if env_ref.data.contains_key(var) {
            env_ref.data.insert(var.to_string(), val);
            return Ok(());
        }
    }

    // 2. Se não achou, pega o ponteiro para o pai (se existir)
    let outer = env.borrow().outer.clone();

    // 3. Recursão no pai
    match outer {
        Some(parent_env) => env_set(&parent_env, var, val),
        None => Err(format!(
            "Error: Variable '{}' not defined. Use 'define' before 'set!'",
            var
        )),
    }
}

pub fn expand_macros(ast: LispExp, env: &mut Env) -> Result<LispExp, String> {
    let ast = pairs_to_vec(&ast);

    match ast {
        LispExp::List(list) => {
            if list.is_empty() {
                return Ok(LispExp::List(list));
            }

            if let LispExp::Symbol(s) = &list[0] {
                // Regra 0: defmacros
                if s == "defmacro" {
                    eval(LispExp::List(list.clone()), env)?;
                    return Ok(LispExp::Void);
                }

                // Regra 1: Não expandir o que está dentro de quote
                if s == "quote" {
                    return Ok(LispExp::List(list.clone()));
                }

                // Regra 2: Proteger parametros do lambda
                if s == "lambda" {
                    if list.len() < 3 {
                        return Err("Malformed lambda".to_string());
                    }
                    let mut new_list = vec![list[0].clone(), list[1].clone()];
                    for item in &list[2..] {
                        new_list.push(expand_macros(item.clone(), env)?);
                    }
                    return Ok(LispExp::List(new_list));
                }

                // Regra 3: Proteger nome da variavel no define
                if s == "define" {
                    if list.len() < 3 {
                        return Err("Malformed define".to_string());
                    }

                    let mut new_list = vec![list[0].clone(), list[1].clone()];
                    for item in &list[2..] {
                        new_list.push(expand_macros(item.clone(), env)?);
                    }
                    return Ok(LispExp::List(new_list));
                }

                // Regra 4: É uma macro?
                if let Some(macro_def) = find_macro(env, s) {
                    let expanded_ast = apply_macro(&macro_def, &list[1..], env)?;

                    return expand_macros(expanded_ast, env);
                }
            }

            // Regra 5: Chamada de função normal
            let mut new_list = vec![];
            for item in list {
                new_list.push(expand_macros(item, env)?);
            }
            Ok(LispExp::List(new_list))
        }
        _ => Ok(ast),
    }
}

fn find_macro(env: &Env, name: &str) -> Option<LispLambda> {
    let env_ref = env.borrow();

    if let Some(LispExp::Macro(m)) = env_ref.data.get(name) {
        return Some(m.clone());
    }

    if let Some(outer) = &env_ref.outer {
        return find_macro(outer, name);
    }

    None
}

pub fn pairs_to_vec(exp: &LispExp) -> LispExp {
    match exp {
        LispExp::Pair(car, cdr) => {
            let mut vec = vec![];

            vec.push(pairs_to_vec(car));

            let mut current = cdr.clone();
            while let LispExp::Pair(next_car, next_cdr) = &*current {
                vec.push(pairs_to_vec(next_car));
                current = next_cdr.clone();
            }

            LispExp::List(vec)
        }

        LispExp::Nil => LispExp::List(vec![]),
        LispExp::List(vec) => LispExp::List(vec.iter().map(pairs_to_vec).collect()),
        _ => exp.clone(),
    }
}

pub fn vec_to_pairs(exp: &LispExp) -> LispExp {
    match exp {
        LispExp::List(vec) => {
            if vec.is_empty() {
                return LispExp::Nil;
            }

            let mut current = LispExp::Nil;
            for item in vec.iter().rev() {
                let item_eval = vec_to_pairs(item);
                current = LispExp::Pair(Rc::new(item_eval), Rc::new(current));
            }
            current
        }
        _ => exp.clone(),
    }
}
