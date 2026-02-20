use std::rc::Rc;

use crate::{
    expr::LispExp,
    heap::Heap,
    helpers::{pairs_to_vec, vec_to_pairs},
    value::Value,
    vm::{Chunk, OpCode},
};

pub fn compile(
    exp: &LispExp,
    chunk: &mut Chunk,
    is_tail: bool,
    heap: &mut Heap,
) -> Result<(), String> {
    let aritmethic_operators = ["+", "-", "*", "/"];
    let comparison_operators = ["<", ">", "=", "<=", ">="];

    match exp {
        LispExp::Symbol(s) => {
            chunk.code.push(OpCode::GetVar(s.clone()));
            if is_tail {
                chunk.code.push(OpCode::Return);
            }
        }
        LispExp::Number(n) => {
            let val = Value::number(*n);
            let idx = chunk.add_constant(val);
            chunk.code.push(OpCode::Constant(idx));
            if is_tail {
                chunk.code.push(OpCode::Return);
            }
        }
        LispExp::Bool(b) => {
            let val = Value::boolean(*b);
            let idx = chunk.add_constant(val);
            chunk.code.push(OpCode::Constant(idx));
            if is_tail {
                chunk.code.push(OpCode::Return);
            }
        }
        LispExp::Str(s) => {
            // let val = heap.alloc(LispExp::Str(s.clone()));
            let val = heap.alloc_string(s.clone());
            let idx = chunk.add_constant(val);
            chunk.code.push(OpCode::Constant(idx));

            if is_tail {
                chunk.code.push(OpCode::Return);
            }
        }
        LispExp::Void => {
            let idx = chunk.add_constant(Value::void());
            chunk.code.push(OpCode::Constant(idx));

            if is_tail {
                chunk.code.push(OpCode::Return);
            }
        }
        LispExp::Pair(_, _) => {
            let ast_vec = pairs_to_vec(exp, heap);
            compile(&LispExp::List(vec![ast_vec]), chunk, is_tail, heap)?
        }
        LispExp::List(list) => {
            if list.is_empty() {
                return Ok(());
            }
            let head = &list[0];
            match head {
                LispExp::Symbol(s) if s == "quote" => {
                    if list.len() != 2 {
                        return Err("quote requires 1 argument".to_string());
                    }
                    let runtime_list = vec_to_pairs(&list[1], heap);

                    let idx = chunk.add_constant(runtime_list);
                    chunk.code.push(OpCode::Constant(idx));
                    if is_tail {
                        chunk.code.push(OpCode::Return);
                    }
                }
                LispExp::Symbol(s) if s == "set!" => {
                    if list.len() != 3 {
                        return Err("'set!' requires a variable and value".to_string());
                    }
                    match &list[1] {
                        LispExp::Symbol(name) => {
                            compile(&list[2], chunk, false, heap)?;
                            chunk.code.push(OpCode::SetVar(name.clone()));
                            if is_tail {
                                chunk.code.push(OpCode::Return);
                            }
                        }
                        _ => return Err("Invalid set!".to_string()),
                    }
                }
                LispExp::Symbol(s) if s == "define" => match &list[1] {
                    LispExp::Symbol(name) => {
                        compile(&list[2], chunk, false, heap)?;
                        chunk.code.push(OpCode::DefVar(name.clone()));
                        if is_tail {
                            chunk.code.push(OpCode::Return);
                        }
                    }
                    LispExp::List(header) if !header.is_empty() => {
                        // syntatic sugar
                        if let LispExp::Symbol(name) = &header[0] {
                            let mut params = vec![];
                            for p in &header[1..] {
                                if let LispExp::Symbol(p_name) = p {
                                    params.push(p_name.clone());
                                }
                            }
                            let mut closure_chunk = Chunk::new();
                            for (i, body_exp) in list[2..].iter().enumerate() {
                                let last = i == (list.len() - 3);
                                compile(body_exp, &mut closure_chunk, last, heap)?;
                                if !last {
                                    closure_chunk.code.push(OpCode::Pop);
                                }
                            }

                            chunk
                                .code
                                .push(OpCode::MakeClosure(params, Rc::new(closure_chunk)));
                            chunk.code.push(OpCode::DefVar(name.clone()));
                            if is_tail {
                                chunk.code.push(OpCode::Return);
                            }
                        } else {
                            return Err("Invalid procedure name".to_string());
                        }
                    }
                    _ => return Err("Invalid define".to_string()),
                },
                LispExp::Symbol(s) if aritmethic_operators.contains(&s.as_str()) => {
                    if list.len() < 3 {
                        return Err(format!("{} requires arguments", s));
                    }
                    compile(&list[1], chunk, false, heap)?;

                    for arg in &list[2..] {
                        compile(arg, chunk, false, heap)?;

                        match s.as_str() {
                            "+" => chunk.code.push(OpCode::Add),
                            "-" => chunk.code.push(OpCode::Sub),
                            "*" => chunk.code.push(OpCode::Mul),
                            "/" => chunk.code.push(OpCode::Div),
                            _ => {}
                        }
                    }
                    if is_tail {
                        chunk.code.push(OpCode::Return);
                    }
                }
                LispExp::Symbol(s) if comparison_operators.contains(&s.as_str()) => {
                    let arg_count = list.len() - 1;
                    if arg_count < 2 {
                        return Err(format!("{} requires at least 2 arguments", s));
                    }
                    for arg in &list[1..] {
                        compile(arg, chunk, false, heap)?;
                    }

                    match s.as_str() {
                        "=" => chunk.code.push(OpCode::Eq(arg_count)),
                        "<" => chunk.code.push(OpCode::Lt(arg_count)),
                        ">" => chunk.code.push(OpCode::Gt(arg_count)),
                        "<=" => chunk.code.push(OpCode::Le(arg_count)),
                        ">=" => chunk.code.push(OpCode::Ge(arg_count)),
                        _ => {}
                    }

                    if is_tail {
                        chunk.code.push(OpCode::Return);
                    }
                }
                LispExp::Symbol(s) if ["car", "cdr", "cons"].contains(&s.as_str()) => {
                    if (s == "car" || s == "cdr") && list.len() != 2 {
                        return Err(format!("{} requires 1 argument", s));
                    }
                    if s == "cons" && list.len() != 3 {
                        return Err("'cons' requires 2 arguments".to_string());
                    }

                    for arg in &list[1..] {
                        compile(arg, chunk, false, heap)?;
                    }

                    match s.as_str() {
                        "cons" => chunk.code.push(OpCode::Cons),
                        "car" => chunk.code.push(OpCode::Car),
                        "cdr" => chunk.code.push(OpCode::Cdr),
                        _ => {}
                    }

                    if is_tail {
                        chunk.code.push(OpCode::Return);
                    }
                }
                LispExp::Symbol(s) if s == "if" => {
                    compile(&list[1], chunk, false, heap)?; // condition

                    let jump_if_false_idx = chunk.code.len();
                    chunk.code.push(OpCode::JumpIfFalse(0));

                    compile(&list[2], chunk, is_tail, heap)?; // then

                    let jump_idx = chunk.code.len();
                    if !is_tail {
                        chunk.code.push(OpCode::Jump(0));
                    }

                    // patch jumps
                    chunk.code[jump_if_false_idx] = OpCode::JumpIfFalse(chunk.code.len());

                    if list.len() > 3 {
                        compile(&list[3], chunk, is_tail, heap)?;
                    } else if !is_tail {
                        let idx = chunk.add_constant(Value::void());
                        chunk.code.push(OpCode::Constant(idx));
                    }
                    if !is_tail {
                        chunk.code[jump_idx] = OpCode::Jump(chunk.code.len());
                    }
                }
                LispExp::Symbol(s) if s == "lambda" || s == "λ" => {
                    let mut params = vec![];
                    if let LispExp::List(p_list) = &list[1] {
                        for p in p_list {
                            if let LispExp::Symbol(name) = p {
                                params.push(name.clone());
                            }
                        }
                    }
                    let mut closure_chunk = Chunk::new();
                    // Compila body. Apenas ultima exp é is_tail
                    for (i, body_exp) in list[2..].iter().enumerate() {
                        let last = i == (list.len() - 3);
                        compile(body_exp, &mut closure_chunk, last, heap)?;
                        if !last {
                            closure_chunk.code.push(OpCode::Pop);
                        }
                    }

                    chunk
                        .code
                        .push(OpCode::MakeClosure(params, Rc::new(closure_chunk)));

                    if is_tail {
                        chunk.code.push(OpCode::Return);
                    }
                }
                _ => {
                    compile(head, chunk, false, heap)?;

                    for arg in &list[1..] {
                        compile(arg, chunk, false, heap)?;
                    }

                    if is_tail {
                        chunk.code.push(OpCode::TailCall(list.len() - 1));
                    } else {
                        chunk.code.push(OpCode::Call(list.len() - 1));
                    }
                }
            }
        }
        _ => {}
    };

    Ok(())
}

pub fn optimize_ast(ast: LispExp) -> LispExp {
    match ast {
        LispExp::List(vec) => {
            if vec.is_empty() {
                return LispExp::List(vec);
            }

            if let LispExp::Symbol(s) = &vec[0] {
                if s == "quote" {
                    return LispExp::List(vec);
                }
            }

            //  Otimiza os argumentos internos primeiro (Recursão Bottom-Up)
            let optimized_vec: Vec<LispExp> = vec.into_iter().map(optimize_ast).collect();

            //  Tenta fazer o "Folding"
            if let LispExp::Symbol(op) = &optimized_vec[0] {
                // Checa se é um operador matemático puro
                if ["+", "-", "*", "/"].contains(&op.as_str()) {
                    // Verifica se TODOS os argumentos depois do operador são números
                    let all_numbers = optimized_vec
                        .iter()
                        .skip(1)
                        .all(|n| matches!(n, LispExp::Number(_)));

                    if all_numbers && optimized_vec.len() > 1 {
                        let mut args = optimized_vec.iter().skip(1).map(|n| {
                            if let LispExp::Number(val) = n {
                                *val
                            } else {
                                0.0
                            }
                        });

                        let mut result = args.next().unwrap();

                        match op.as_str() {
                            "+" => {
                                for n in args {
                                    result += n;
                                }
                            }
                            "-" => {
                                // Suporte para número negativo ex: (- 5) -> -5
                                if optimized_vec.len() == 2 {
                                    result = -result;
                                } else {
                                    for n in args {
                                        result -= n;
                                    }
                                }
                            }
                            "*" => {
                                for n in args {
                                    result *= n;
                                }
                            }
                            "/" => {
                                for n in args {
                                    result /= n;
                                }
                            }
                            _ => {}
                        }

                        return LispExp::Number(result);
                    }
                }
            }

            LispExp::List(optimized_vec)
        }

        _ => ast,
    }
}
