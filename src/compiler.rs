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
    previous_line: usize,
) -> Result<(), String> {
    let aritmethic_operators = ["+", "-", "*", "/"];
    let comparison_operators = ["<", ">", "=", "<=", ">="];

    let current_line = match exp {
        LispExp::List(_, l) if *l != 0 => *l,
        LispExp::Symbol(_, l) if *l != 0 => *l,
        _ => previous_line,
    };

    match exp {
        LispExp::Symbol(s, _) => {
            chunk.write(OpCode::GetVar(s.clone()), current_line);
            if is_tail {
                chunk.write(OpCode::Return, current_line);
            }
        }
        LispExp::Number(n) => {
            let val = Value::number(*n);
            let idx = chunk.add_constant(val);
            chunk.write(OpCode::Constant(idx), current_line);
            if is_tail {
                chunk.write(OpCode::Return, current_line);
            }
        }
        LispExp::Bool(b) => {
            let val = Value::boolean(*b);
            let idx = chunk.add_constant(val);
            chunk.write(OpCode::Constant(idx), current_line);
            if is_tail {
                chunk.write(OpCode::Return, current_line);
            }
        }
        LispExp::Str(s) => {
            let val = heap.alloc_string(s.clone());
            let idx = chunk.add_constant(val);
            chunk.write(OpCode::Constant(idx), current_line);

            if is_tail {
                chunk.write(OpCode::Return, current_line);
            }
        }
        LispExp::Void => {
            let idx = chunk.add_constant(Value::void());
            chunk.write(OpCode::Constant(idx), current_line);

            if is_tail {
                chunk.write(OpCode::Return, current_line);
            }
        }
        LispExp::Pair(_, _) => {
            let ast_vec = pairs_to_vec(exp, heap);
            compile(
                &LispExp::List(vec![ast_vec], current_line),
                chunk,
                is_tail,
                heap,
                current_line,
            )?
        }
        LispExp::List(list, _) => {
            if list.is_empty() {
                return Ok(());
            }
            let head = &list[0];
            match head {
                LispExp::Symbol(s, _) if s == "quote" => {
                    if list.len() != 2 {
                        return Err("quote requires 1 argument".to_string());
                    }
                    let runtime_list = vec_to_pairs(&list[1], heap);

                    let idx = chunk.add_constant(runtime_list);
                    chunk.write(OpCode::Constant(idx), current_line);
                    if is_tail {
                        chunk.write(OpCode::Return, current_line);
                    }
                }
                LispExp::Symbol(s, _) if s == "set!" => {
                    if list.len() != 3 {
                        return Err("'set!' requires a variable and value".to_string());
                    }
                    match &list[1] {
                        LispExp::Symbol(name, _) => {
                            compile(&list[2], chunk, false, heap, current_line)?;
                            chunk.write(OpCode::SetVar(name.clone()), current_line);
                            if is_tail {
                                chunk.write(OpCode::Return, current_line);
                            }
                        }
                        _ => return Err("Invalid set!".to_string()),
                    }
                }
                LispExp::Symbol(s, _) if s == "define" => match &list[1] {
                    LispExp::Symbol(name, _) => {
                        compile(&list[2], chunk, false, heap, current_line)?;
                        chunk.write(OpCode::DefVar(name.clone()), current_line);
                        if is_tail {
                            chunk.write(OpCode::Return, current_line);
                        }
                    }
                    LispExp::List(header, _) if !header.is_empty() => {
                        // syntatic sugar
                        if let LispExp::Symbol(name, _) = &header[0] {
                            let mut params = vec![];
                            for p in &header[1..] {
                                if let LispExp::Symbol(p_name, _) = p {
                                    params.push(p_name.clone());
                                }
                            }
                            let mut closure_chunk = Chunk::new();
                            for (i, body_exp) in list[2..].iter().enumerate() {
                                let last = i == (list.len() - 3);
                                compile(body_exp, &mut closure_chunk, last, heap, current_line)?;
                                if !last {
                                    closure_chunk.write(OpCode::Pop, current_line);
                                }
                            }

                            chunk
                                .code
                                .push(OpCode::MakeClosure(params, Rc::new(closure_chunk)));
                            chunk.write(OpCode::DefVar(name.clone()), current_line);
                            if is_tail {
                                chunk.write(OpCode::Return, current_line);
                            }
                        } else {
                            return Err("Invalid procedure name".to_string());
                        }
                    }
                    _ => return Err("Invalid define".to_string()),
                },
                LispExp::Symbol(s, _) if aritmethic_operators.contains(&s.as_str()) => {
                    if list.len() == 2 {
                        if s == "-" {
                            let idx = chunk.add_constant(Value::number(0.0));
                            chunk.write(OpCode::Constant(idx), current_line);
                            compile(&list[1], chunk, false, heap, current_line)?;
                            chunk.write(OpCode::Sub, current_line);
                        } else if s == "/" {
                            let idx = chunk.add_constant(Value::number(1.0));
                            chunk.write(OpCode::Constant(idx), current_line);
                            compile(&list[1], chunk, false, heap, current_line)?;
                            chunk.write(OpCode::Div, current_line);
                        } else {
                            return Err(format!("'{}' requires at least 2 arguments", s));
                        }

                        if is_tail {
                            chunk.write(OpCode::Return, current_line);
                        }
                        return Ok(());
                    }

                    if list.len() < 3 {
                        return Err(format!("{} requires arguments", s));
                    }
                    compile(&list[1], chunk, false, heap, current_line)?;

                    for arg in &list[2..] {
                        compile(arg, chunk, false, heap, current_line)?;

                        match s.as_str() {
                            "+" => chunk.write(OpCode::Add, current_line),
                            "-" => chunk.write(OpCode::Sub, current_line),
                            "*" => chunk.write(OpCode::Mul, current_line),
                            "/" => chunk.write(OpCode::Div, current_line),
                            _ => {}
                        }
                    }
                    if is_tail {
                        chunk.write(OpCode::Return, current_line);
                    }
                }
                LispExp::Symbol(s, _) if comparison_operators.contains(&s.as_str()) => {
                    let arg_count = list.len() - 1;
                    if arg_count < 2 {
                        return Err(format!("{} requires at least 2 arguments", s));
                    }
                    for arg in &list[1..] {
                        compile(arg, chunk, false, heap, current_line)?;
                    }

                    match s.as_str() {
                        "=" => chunk.write(OpCode::Eq(arg_count), current_line),
                        "<" => chunk.write(OpCode::Lt(arg_count), current_line),
                        ">" => chunk.write(OpCode::Gt(arg_count), current_line),
                        "<=" => chunk.write(OpCode::Le(arg_count), current_line),
                        ">=" => chunk.write(OpCode::Ge(arg_count), current_line),
                        _ => {}
                    }

                    if is_tail {
                        chunk.write(OpCode::Return, current_line);
                    }
                }
                LispExp::Symbol(s, _) if ["car", "cdr", "cons"].contains(&s.as_str()) => {
                    if (s == "car" || s == "cdr") && list.len() != 2 {
                        return Err(format!("{} requires 1 argument", s));
                    }
                    if s == "cons" && list.len() != 3 {
                        return Err("'cons' requires 2 arguments".to_string());
                    }

                    for arg in &list[1..] {
                        compile(arg, chunk, false, heap, current_line)?;
                    }

                    match s.as_str() {
                        "cons" => chunk.write(OpCode::Cons, current_line),
                        "car" => chunk.write(OpCode::Car, current_line),
                        "cdr" => chunk.write(OpCode::Cdr, current_line),
                        _ => {}
                    }

                    if is_tail {
                        chunk.write(OpCode::Return, current_line);
                    }
                }
                LispExp::Symbol(s, _) if s == "if" => {
                    compile(&list[1], chunk, false, heap, current_line)?; // condition

                    let jump_if_false_idx = chunk.code.len();
                    chunk.write(OpCode::JumpIfFalse(0), current_line);

                    compile(&list[2], chunk, is_tail, heap, current_line)?; // then

                    let jump_idx = chunk.code.len();
                    if !is_tail {
                        chunk.write(OpCode::Jump(0), current_line);
                    }

                    // patch jumps
                    chunk.code[jump_if_false_idx] = OpCode::JumpIfFalse(chunk.code.len());

                    if list.len() > 3 {
                        compile(&list[3], chunk, is_tail, heap, current_line)?;
                    } else if !is_tail {
                        let idx = chunk.add_constant(Value::void());
                        chunk.write(OpCode::Constant(idx), current_line);
                    }
                    if !is_tail {
                        chunk.code[jump_idx] = OpCode::Jump(chunk.code.len());
                    }
                }
                LispExp::Symbol(s, _) if s == "lambda" || s == "λ" => {
                    let mut params = vec![];
                    if let LispExp::List(p_list, _) = &list[1] {
                        for p in p_list {
                            if let LispExp::Symbol(name, _) = p {
                                params.push(name.clone());
                            }
                        }
                    }
                    let mut closure_chunk = Chunk::new();
                    // Compila body. Apenas ultima exp é is_tail
                    for (i, body_exp) in list[2..].iter().enumerate() {
                        let last = i == (list.len() - 3);
                        compile(body_exp, &mut closure_chunk, last, heap, current_line)?;
                        if !last {
                            closure_chunk.write(OpCode::Pop, current_line);
                        }
                    }

                    chunk
                        .code
                        .push(OpCode::MakeClosure(params, Rc::new(closure_chunk)));

                    if is_tail {
                        chunk.write(OpCode::Return, current_line);
                    }
                }
                _ => {
                    compile(head, chunk, false, heap, current_line)?;

                    for arg in &list[1..] {
                        compile(arg, chunk, false, heap, current_line)?;
                    }

                    if is_tail {
                        chunk.write(OpCode::TailCall(list.len() - 1), current_line);
                    } else {
                        chunk.write(OpCode::Call(list.len() - 1), current_line);
                    }
                }
            }
        }
        _ => {}
    };

    Ok(())
}

pub fn optimize_ast(ast: LispExp) -> LispExp {
    let line = match &ast {
        LispExp::List(_, l) => *l,
        LispExp::Symbol(_, l) => *l,
        _ => 0,
    };

    match ast {
        LispExp::List(vec, _) => {
            if vec.is_empty() {
                return LispExp::List(vec, line);
            }

            if let LispExp::Symbol(s, _) = &vec[0] {
                if s == "quote" {
                    return LispExp::List(vec, line);
                }
            }

            //  Otimiza os argumentos internos primeiro (Recursão Bottom-Up)
            let optimized_vec: Vec<LispExp> = vec.into_iter().map(optimize_ast).collect();

            //  Tenta fazer o "Folding"
            if let LispExp::Symbol(op, _) = &optimized_vec[0] {
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

            LispExp::List(optimized_vec, line)
        }

        _ => ast,
    }
}
