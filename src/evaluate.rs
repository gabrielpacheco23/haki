use crate::{
    apply_macro, apply_procedure,
    env::{Env, LispEnv},
    expand_quasiquote,
    expr::{LispExp, LispLambda},
    heap::Heap,
    helpers::{ast_to_value, expand_macros, pairs_to_vec, value_to_ast},
};
use std::rc::Rc;

pub fn eval(exp: LispExp, env: &mut Env, heap: &mut Heap) -> Result<LispExp, String> {
    match exp {
        LispExp::Symbol(s, _) => {
            let val = env
                .borrow()
                .get(&s)
                .ok_or_else(|| format!("Variable not found: {}", s))?;
            Ok(value_to_ast(val, heap))
        }
        LispExp::Number(_) | LispExp::Bool(_) | LispExp::Str(_) => Ok(exp),
        LispExp::Native(_) => Ok(exp),
        LispExp::Lambda(_) => Ok(exp),
        LispExp::Macro(_) => Ok(exp),
        LispExp::List(list, pos) => {
            if list.is_empty() {
                return Ok(LispExp::List(vec![], pos));
            }
            let (head, tail) = list.split_first().ok_or("Cannot evaluate empty list '()")?;

            match head {
                LispExp::Symbol(s, _) if s == "if" => {
                    if tail.len() != 3 {
                        return Err(
                            "'if' requires 3 arguments: (if condition then else)".to_string()
                        );
                    }

                    let test_result = eval(tail[0].clone(), env, heap)?;
                    match test_result {
                        LispExp::Bool(false) => eval(tail[2].clone(), env, heap),
                        _ => eval(tail[1].clone(), env, heap),
                    }
                }
                LispExp::Symbol(s, line) if s == "begin" => {
                    let mut last_val = LispExp::List(vec![], *line);

                    for exp in tail {
                        last_val = eval(exp.clone(), env, heap)?;
                    }

                    Ok(last_val)
                }
                LispExp::Symbol(s, line) if s == "define" => {
                    if tail.len() < 2 {
                        return Err("'define' requires arguments".to_string());
                    }

                    let (name, val) = match &tail[0] {
                        LispExp::Symbol(s, _) => (s.clone(), eval(tail[1].clone(), env, heap)?),
                        LispExp::List(def_header, _) => {
                            if def_header.is_empty() {
                                return Err("Invalid procedure definition".to_string());
                            }
                            let func_name = match &def_header[0] {
                                LispExp::Symbol(s, _) => s.clone(),
                                _ => return Err("Procedure name must be a symbol".to_string()),
                            };

                            let params = LispExp::List(def_header[1..].to_vec(), *line);

                            let body_exps = &tail[1..];
                            let body = if body_exps.len() > 1 {
                                let mut begin_vec =
                                    vec![LispExp::Symbol("begin".to_string(), *line)];
                                begin_vec.extend_from_slice(body_exps);
                                LispExp::List(begin_vec, 0)
                            } else if body_exps.len() == 1 {
                                body_exps[0].clone()
                            } else {
                                return Err("Procedure without body".to_string());
                            };

                            let lambda = LispExp::Lambda(LispLambda {
                                params: Rc::new(params),
                                body: Rc::new(body),
                                env: env.clone(),
                            });

                            (func_name, lambda)
                        }
                        _ => return Err("Invalid first argument of 'define'".to_string()),
                    };

                    let value_tag = ast_to_value(&val, heap);
                    env.borrow_mut().insert(name, value_tag);
                    Ok(LispExp::Void)
                }
                LispExp::Symbol(s, line) if s == "lambda" => {
                    if tail.len() != 2 {
                        return Err(
                            "lambda requires 2 arguments: (lambda (params) body)".to_string()
                        );
                    }

                    let params = tail[0].clone();

                    let body_exps = &tail[1..];
                    let body = if body_exps.len() > 1 {
                        let mut begin_vec = vec![LispExp::Symbol("begin".to_string(), *line)];
                        begin_vec.extend_from_slice(body_exps);
                        LispExp::List(begin_vec, 0)
                    } else {
                        body_exps[0].clone()
                    };

                    Ok(LispExp::Lambda(LispLambda {
                        params: Rc::new(params),
                        body: Rc::new(body),
                        env: env.clone(),
                    }))
                }

                LispExp::Symbol(s, _) if s == "let" => {
                    if tail.len() < 2 {
                        return Err("'let' requires bindings and body".to_string());
                    }

                    // Cria um novo escopo temporário
                    let mut new_env = LispEnv::new(Some(env.clone()));

                    //  Resolve as variáveis (Parallel Binding)
                    if let LispExp::List(bindings, _) = &tail[0] {
                        for binding in bindings {
                            if let LispExp::List(kv, _) = binding {
                                if kv.len() == 2 {
                                    if let LispExp::Symbol(name, _) = &kv[0] {
                                        let val = eval(kv[1].clone(), env, heap)?;
                                        new_env
                                            .borrow_mut()
                                            .insert(name.clone(), ast_to_value(&val, heap));
                                    }
                                }
                            }
                        }
                    } else {
                        return Err("Invalid let bindings".to_string());
                    }

                    // Executa o corpo sequencialmente no novo escopo
                    let mut last_val = LispExp::Void;
                    for exp in &tail[1..] {
                        last_val = eval(exp.clone(), &mut new_env, heap)?;
                    }

                    return Ok(last_val);
                }
                LispExp::Symbol(s, line) if s == "defmacro" => {
                    let head_def = match &tail[0] {
                        LispExp::List(l, _) if !l.is_empty() => l,
                        _ => return Err("defmacro requires (name params...)".to_string()),
                    };

                    let macro_name = match &head_def[0] {
                        LispExp::Symbol(s, _) => s.clone(),
                        _ => {
                            return Err("Macro name must be a symbol".to_string());
                        }
                    };

                    let params = Rc::new(LispExp::List(head_def[1..].to_vec(), *line));
                    let body = Rc::new(tail[1].clone());

                    let macro_exp = LispExp::Macro(LispLambda {
                        params,
                        body,
                        env: env.clone(),
                    });

                    env.borrow_mut()
                        .insert(macro_name, ast_to_value(&macro_exp, heap));
                    Ok(LispExp::Void)
                }
                LispExp::Symbol(s, _) if s == "set!" => {
                    if tail.len() != 2 {
                        return Err("set! requires 2 arguments: (set! var val)".to_string());
                    }

                    let var_name = match &tail[0] {
                        LispExp::Symbol(s, _) => s,
                        _ => {
                            return Err("First argument of 'set!' must be a symbol".to_string());
                        }
                    };

                    let val_ast = eval(tail[1].clone(), env, heap)?;
                    let val_tag = ast_to_value(&val_ast, heap);
                    let _ = env.borrow_mut().set(var_name, val_tag);

                    Ok(LispExp::Void)
                }

                LispExp::Symbol(s, _) if s == "quote" => {
                    if tail.len() != 1 {
                        return Err("'quote' requires 1 argument".to_string());
                    }

                    Ok(tail[0].clone())
                }
                LispExp::Symbol(s, _) if s == "quasiquote" => {
                    if tail.len() != 1 {
                        return Err("'quasiquote' requires 1 argument".to_string());
                    }

                    expand_quasiquote(&tail[0], env, heap)
                }
                _ => {
                    let proc = eval(head.clone(), env, heap)?;

                    match proc {
                        LispExp::Macro(macro_def) => {
                            let expansion = apply_macro(&macro_def, tail, env, heap)?;
                            eval(expansion, env, heap)
                        }

                        _ => {
                            let args: Result<Vec<LispExp>, String> = tail
                                .iter()
                                .map(|arg| eval(arg.clone(), env, heap))
                                .collect();

                            apply_procedure(&proc, &args?, env, heap)
                        }
                    }
                }
            }
        }
        LispExp::Void => Ok(LispExp::Void),
        // TODO: check this
        LispExp::Nil => Ok(LispExp::List(vec![], 0)),
        LispExp::Pair(_, _) => {
            let ast_list = pairs_to_vec(&exp, heap);
            let expanded_ast = expand_macros(ast_list, env, heap)?;
            eval(expanded_ast, env, heap)
        }
        val @ LispExp::VmClosure { .. } => Ok(val),
        LispExp::Vector(v_ref) => Ok(LispExp::Vector(v_ref)),
        LispExp::HashMap(map_ref) => Ok(LispExp::HashMap(map_ref)),
        LispExp::HeapPtr(_) => todo!(),
    }
}
