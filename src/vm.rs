use crate::compiler::CompilerUpvalue;
use crate::upvalue::{Upvalue, UpvalueState};
use crate::{
    evaluate::eval,
    helpers::{ast_to_value, pairs_to_vec},
    value::Value,
};
use std::{fmt::Display, rc::Rc};

use crate::{
    env::{Env, LispEnv},
    expr::LispExp,
    heap::Heap,
};

#[derive(Clone, Debug)]
pub enum OpCode {
    Constant(usize),
    Pop,
    GetGlobal(String),
    SetGlobal(String),
    DefGlobal(String),
    GetLocal(usize),
    SetLocal(usize),
    GetUpvalue(usize),
    SetUpvalue(usize),
    CloseUpvalue,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq(usize),
    Lt(usize),
    Le(usize),
    Gt(usize),
    Ge(usize),
    Cons,
    Car,
    Cdr,
    JumpIfFalse(usize),
    Jump(usize),
    Call(usize),
    TailCall(usize),
    Return,
    MakeClosure(Vec<String>, Rc<Chunk>, Vec<CompilerUpvalue>),
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, opcode: OpCode, line: usize) {
        self.code.push(opcode);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn dump(&self) {
        println!("--- PROGRAM DUMP ---");
        for (i, opcode) in self.code.iter().enumerate() {
            println!("{:04}: {}", i, opcode);
        }
        println!();
    }
}

pub struct Vm {
    pub stack: Vec<Value>,
    pub frames: Vec<CallFrame>,
    pub open_upvalues: Vec<Upvalue>,
}

pub struct CallFrame {
    pub chunk: Rc<Chunk>,
    pub ip: usize,
    pub stack_offset: usize,
    pub closure_upvalues: Vec<Upvalue>,
}

macro_rules! vm_math {
    ($op:tt, $self:ident) => {{
        let b = $self.stack.pop().unwrap_or(Value::void());
        let a = $self.stack.pop().unwrap_or(Value::void());

        if a.is_number() && b.is_number() {
            let result = a.as_number() $op b.as_number();
            $self.stack.push(Value::number(result));
        } else {
            return Err("Arithmetic operations require numbers".to_string());
        }
    }};
}

macro_rules! vm_cmp_n {
    ($op:tt, $arity:expr, $self:expr) => {{
        let mut args = Vec::with_capacity($arity);
        for _ in 0..$arity {
            args.push($self.stack.pop().unwrap_or(Value::void()));
        }
        args.reverse();

        let mut is_true = true;
        for i in 0..($arity - 1) {
            let a = args[i];
            let b = args[i+1];

            if a.is_number() && b.is_number() {
                if !(a.as_number() $op b.as_number()) {
                    is_true = false;
                    break;
                }
            } else {
                return Err("Comparações exigem números".to_string());
            }
        }

        $self.stack.push(Value::boolean(is_true));
    }};
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            stack: Vec::with_capacity(2048),
            frames: vec![],
            open_upvalues: vec![],
        }
    }

    pub fn capture_upvalue(&mut self, local_idx: usize) -> Upvalue {
        for upvalue in &self.open_upvalues {
            if let UpvalueState::Open(idx) = *upvalue.state.borrow() {
                if idx == local_idx {
                    return upvalue.clone();
                }
            }
        }

        let created_upvalue = Upvalue::new_open(local_idx);
        self.open_upvalues.push(created_upvalue.clone());
        created_upvalue
    }

    pub fn close_upvalues(&mut self, last_stack_idx: usize) {
        self.open_upvalues.retain(|upvalue| {
            let mut state = upvalue.state.borrow_mut();
            if let UpvalueState::Open(idx) = *state {
                if idx >= last_stack_idx {
                    *state = UpvalueState::Closed(self.stack[idx]);
                    return false; // fecha a variável e tira da lista
                }
                return true; // mantém na lista
            }
            false // se já estiver fechado, tira da lista
        });
    }

    // pub fn close_upvalues(&mut self, last_stack_idx: usize) {
    //     self.open_upvalues.retain(|upvalue| {
    //         let mut state = upvalue.state.borrow_mut();
    //         if let UpvalueState::Open(idx) = *state {
    //             if idx >= last_stack_idx {
    //                 *state = UpvalueState::Closed(self.stack[idx]);
    //                 return false;
    //             }
    //         }
    //         true
    //     });
    // }

    pub fn execute(
        &mut self,
        chunk: Rc<Chunk>,
        env: Env,
        heap: &mut Heap,
        is_repl: bool,
    ) -> Result<Value, String> {
        match self.execute_inner(chunk, env, heap) {
            Ok(val) => Ok(val),
            Err(err) => {
                let mut trace = String::from("");
                if !is_repl {
                    trace.push_str(&format!("\n[Error] {}\n", err));
                } else {
                    trace.push_str(&format!("{}\n", err));
                }
                trace.push_str("Stack trace:\n");

                for frame in self.frames.iter().rev() {
                    let ip = if frame.ip > 0 { frame.ip - 1 } else { 0 };
                    let line = frame.chunk.lines.get(ip).unwrap_or(&0);

                    if *line == 0 {
                        trace.push_str(&format!(" in <internal> (ip: {})\n", ip));
                    } else {
                        trace.push_str(&format!(" in line {} (ip: {})\n", line, ip));
                    }
                }
                self.frames.clear();
                Err(trace)
            }
        }
    }

    fn execute_inner(
        &mut self,
        chunk: Rc<Chunk>,
        env: Env,
        heap: &mut Heap,
    ) -> Result<Value, String> {
        if self.frames.is_empty() {
            self.frames.push(CallFrame {
                chunk,
                ip: 0,
                stack_offset: self.stack.len(),
                closure_upvalues: vec![],
            });
        }

        loop {
            if self.frames.is_empty() {
                return Ok(self.stack.pop().unwrap_or(Value::void()));
            }

            let frame = self.frames.last_mut().unwrap();
            let op = &frame.chunk.code[frame.ip].clone();
            frame.ip += 1;

            match op {
                OpCode::Constant(idx) => {
                    let val = frame.chunk.constants[*idx];
                    self.stack.push(val);
                }
                OpCode::Pop => _ = self.stack.pop(),
                OpCode::GetGlobal(name) => {
                    if let Some(val) = env.borrow().get(name) {
                        self.stack.push(val);
                    } else {
                        return Err(format!("Variable not found: {}", name));
                    }
                }
                OpCode::SetGlobal(name) => {
                    let val = *self.stack.last().unwrap();
                    let _ = env.borrow_mut().set(&name, val);
                }
                OpCode::DefGlobal(name) => {
                    let val = self.stack.pop().unwrap();
                    env.borrow_mut().insert(name.clone(), val);
                    self.stack.push(Value::void());
                }
                OpCode::GetLocal(idx) => {
                    let val = self.stack[frame.stack_offset + idx];
                    self.stack.push(val);
                    // unsafe {
                    //     let val = self.stack.get_unchecked(frame.stack_offset + idx);
                    //     self.stack.push(*val);
                    // }
                }
                OpCode::SetLocal(idx) => {
                    let val = self.stack[frame.stack_offset + idx];
                    self.stack[frame.stack_offset + idx] = val;
                }
                OpCode::GetUpvalue(idx) => {
                    let val = frame.closure_upvalues[*idx].get_value(&self.stack);
                    self.stack.push(val);
                }
                OpCode::SetUpvalue(idx) => {
                    let val = *self.stack.last().unwrap();
                    frame.closure_upvalues[*idx].set_value(val, &mut self.stack);
                }
                OpCode::Add => vm_math!(+, self),
                OpCode::Sub => vm_math!(-, self),
                OpCode::Mul => vm_math!(*, self),
                OpCode::Div => vm_math!(/, self),
                OpCode::Mod => vm_math!(%, self),
                OpCode::Eq(n) => vm_cmp_n!(==, *n, self),
                OpCode::Lt(n) => vm_cmp_n!(<, *n, self),
                OpCode::Le(n) => vm_cmp_n!(<=, *n, self),
                OpCode::Gt(n) => vm_cmp_n!(>, *n, self),
                OpCode::Ge(n) => vm_cmp_n!(>=, *n, self),
                OpCode::JumpIfFalse(address) => {
                    let cond = self.stack.pop().unwrap();
                    if !cond.as_boolean() {
                        self.frames.last_mut().unwrap().ip = *address;
                    }
                }
                OpCode::Jump(address) => {
                    self.frames.last_mut().unwrap().ip = *address;
                }
                OpCode::Call(arg_count) | OpCode::TailCall(arg_count) => {
                    let is_tail = matches!(op, OpCode::TailCall(_));

                    // Os argumentos já estão no topo da pilha. A função está logo abaixo deles
                    let stack_offset = self.stack.len() - arg_count;
                    let func_val = self.stack[stack_offset - 1];

                    if !func_val.is_gc_ref() {
                        return Err("Object is not callable".to_string());
                    }

                    let func_exp = heap.get(func_val).unwrap().clone();
                    match func_exp {
                        LispExp::Native(f) => {
                            // Fecha os Upvalues de qualquer closure que esteja
                            // sendo enviada para o mundo nativo (como o try-catch).
                            for arg in &self.stack[stack_offset..] {
                                if arg.is_gc_ref() {
                                    if let Some(LispExp::VmClosure { upvalues, .. }) =
                                        heap.get(*arg)
                                    {
                                        for upval in upvalues {
                                            let mut state = upval.state.borrow_mut();
                                            if let UpvalueState::Open(idx) = *state {
                                                *state = UpvalueState::Closed(self.stack[idx]);
                                            }
                                        }
                                    }
                                }
                            }

                            let args_slice = &self.stack[stack_offset..].to_vec();
                            let mut temp_env = env.clone();
                            let result = f(args_slice, &mut temp_env, heap)?;

                            if is_tail {
                                let frame = self.frames.pop().unwrap();
                                self.close_upvalues(frame.stack_offset);

                                // TCO CORRETO: Apaga a pilha até o início do frame chamador!
                                // Isso limpa o frame morto E a função nativa de uma vez só.
                                if frame.stack_offset > 0 {
                                    self.stack.truncate(frame.stack_offset - 1);
                                } else {
                                    self.stack.clear();
                                }

                                if self.frames.is_empty() {
                                    return Ok(result);
                                }
                            } else {
                                // Chamada normal: Limpa apenas os argumentos e a função nativa
                                self.stack.truncate(stack_offset - 1);
                            }
                            self.stack.push(result);
                        }
                        // LispExp::Native(f) => {
                        //     // Funções nativas do Rust recebem os argumentos por fatia (slice)
                        //     let args_slice = &self.stack[stack_offset..].to_vec();
                        //     let mut temp_env = env.clone();
                        //     let result = f(args_slice, &mut temp_env, heap)?;

                        //     // Limpa os argumentos e a função nativa da pilha
                        //     self.stack.truncate(stack_offset - 1);

                        //     if is_tail {
                        //         let frame = self.frames.pop().unwrap();
                        //         self.close_upvalues(frame.stack_offset);
                        //         if self.frames.is_empty() {
                        //             return Ok(result);
                        //         }
                        //     }
                        //     self.stack.push(result);
                        // }
                        LispExp::VmClosure {
                            params,
                            chunk,
                            upvalues,
                        } => {
                            if *arg_count != params.len() {
                                return Err(format!(
                                    "Incorrect arity. Expected {}, got {}",
                                    params.len(),
                                    arg_count
                                ));
                            }

                            if is_tail {
                                let old_frame = self.frames.pop().unwrap();
                                self.close_upvalues(old_frame.stack_offset);

                                // TCO MÁGICO: Empurramos os argumentos novos para cima dos velhos!
                                let new_args_start = stack_offset;
                                let target_start = old_frame.stack_offset;

                                for i in 0..*arg_count {
                                    self.stack[target_start + i] = self.stack[new_args_start + i];
                                }
                                self.stack.truncate(target_start + *arg_count);

                                // O novo frame reaproveita o espaço do antigo
                                self.frames.push(CallFrame {
                                    chunk: chunk.clone(),
                                    ip: 0,
                                    stack_offset: target_start,
                                    closure_upvalues: upvalues.clone(),
                                });
                            } else {
                                // Chamada normal, cria um novo frame em cima da pilha atual
                                self.frames.push(CallFrame {
                                    chunk: chunk.clone(),
                                    ip: 0,
                                    stack_offset,
                                    closure_upvalues: upvalues.clone(),
                                });
                            }
                        }
                        LispExp::Lambda(lambda) => {
                            let mut new_env = LispEnv::new(Some(lambda.env.clone()));
                            let params_flat = pairs_to_vec(&*lambda.params, heap);

                            let args_slice = &self.stack[stack_offset..];
                            if let LispExp::List(params, _) = params_flat {
                                if args_slice.len() != params.len() {
                                    return Err("Incorrect arity".to_string());
                                }
                                for (param, arg) in params.iter().zip(args_slice.iter()) {
                                    if let LispExp::Symbol(name, _) = param {
                                        new_env.borrow_mut().insert(name.clone(), *arg);
                                    }
                                }
                            }

                            let result_ast = eval((*lambda.body).clone(), &mut new_env, heap)?;
                            let result_val = ast_to_value(&result_ast, heap);

                            if is_tail {
                                let frame = self.frames.pop().unwrap();
                                self.close_upvalues(frame.stack_offset);

                                if frame.stack_offset > 0 {
                                    self.stack.truncate(frame.stack_offset - 1);
                                } else {
                                    self.stack.clear();
                                }

                                if self.frames.is_empty() {
                                    return Ok(result_val);
                                }
                            } else {
                                self.stack.truncate(stack_offset - 1);
                            }
                            self.stack.push(result_val);
                        } //     LispExp::Lambda(lambda) => {
                        //         // Interoperabilidade com funções antigas não-compiladas (Avaliador de Árvore)
                        //         let mut new_env = LispEnv::new(Some(lambda.env.clone()));
                        //         let params_flat = pairs_to_vec(&*lambda.params, heap);

                        //         let args_slice = &self.stack[stack_offset..];
                        //         if let LispExp::List(params, _) = params_flat {
                        //             if args_slice.len() != params.len() {
                        //                 return Err("Incorrect arity".to_string());
                        //             }
                        //             for (param, arg) in params.iter().zip(args_slice.iter()) {
                        //                 if let LispExp::Symbol(name, _) = param {
                        //                     new_env.borrow_mut().insert(name.clone(), *arg);
                        //                 }
                        //             }
                        //         }

                        //         let result_ast = eval((*lambda.body).clone(), &mut new_env, heap)?;
                        //         let result_val = ast_to_value(&result_ast, heap);

                        //         self.stack.truncate(stack_offset - 1);

                        //         if is_tail {
                        //             let frame = self.frames.pop().unwrap();
                        //             self.close_upvalues(frame.stack_offset);
                        //             if self.frames.is_empty() {
                        //                 return Ok(result_val);
                        //             }
                        //         }
                        //         self.stack.push(result_val);
                        //     }
                        _ => return Err("Object is not callable".to_string()),
                    }
                }
                OpCode::Return => {
                    let result = self.stack.pop().unwrap_or(Value::void());
                    let frame = self.frames.pop().unwrap();

                    // O RESGATE: A função vai morrer, salvem os Post-its abertos!
                    self.close_upvalues(frame.stack_offset);

                    // Apaga a função e os argumentos locais da Pilha de forma ultra-rápida
                    if frame.stack_offset > 0 {
                        self.stack.truncate(frame.stack_offset - 1);
                    }

                    self.stack.push(result);
                    // let result = self.stack.pop().unwrap_or(Value::void());
                    // self.frames.pop();
                    // self.stack.push(result);
                }
                OpCode::MakeClosure(params, chunk, compiler_upvalues) => {
                    let stack_offset = frame.stack_offset;
                    let closure_upvalues = frame.closure_upvalues.clone();

                    let mut runtime_upvalues = vec![];

                    for cup in compiler_upvalues {
                        if cup.is_local {
                            let stack_idx = stack_offset + cup.index;
                            runtime_upvalues.push(self.capture_upvalue(stack_idx));
                        } else {
                            runtime_upvalues.push(closure_upvalues[cup.index].clone());
                        }
                    }

                    let closure = LispExp::VmClosure {
                        params: params.clone(),
                        chunk: chunk.clone(),
                        upvalues: runtime_upvalues,
                    };

                    let clos_ptr = heap.alloc(closure);
                    self.stack.push(clos_ptr);
                }
                OpCode::Cons => {
                    let b = self.stack.pop().unwrap_or(Value::void());
                    let a = self.stack.pop().unwrap_or(Value::void());
                    let pair_ptr = heap.alloc(LispExp::Pair(a, b));
                    self.stack.push(pair_ptr);
                }
                OpCode::Car => {
                    let obj = self.stack.pop().unwrap_or(Value::void());

                    if obj.is_gc_ref() {
                        match heap.get(obj) {
                            Some(LispExp::Pair(car, _cdr)) => {
                                self.stack.push(*car);
                                continue;
                            }
                            Some(LispExp::List(vec, _)) => {
                                let vec = vec.clone();
                                if vec.is_empty() {
                                    return Err("car: empty list".to_string());
                                }
                                self.stack.push(ast_to_value(&vec[0], heap));
                                continue;
                            }
                            _ => {}
                        }
                    }
                    return Err("'car' requires a pair or list".to_string());
                }
                OpCode::Cdr => {
                    let obj = self.stack.pop().unwrap_or(Value::void());

                    if obj.is_gc_ref() {
                        match heap.get(obj) {
                            Some(LispExp::Pair(_car, cdr)) => {
                                self.stack.push(*cdr);
                                continue;
                            }
                            Some(LispExp::List(vec, _)) => {
                                if vec.is_empty() {
                                    return Err("cdr: empty list".to_string());
                                }
                                let rest = LispExp::List(vec[1..].to_vec(), 0);
                                self.stack.push(ast_to_value(&rest, heap));
                                continue;
                            }
                            _ => {}
                        }
                    }
                    return Err("'cdr' requires a pair or list".to_string());
                }
                OpCode::CloseUpvalue => todo!(),
            }
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Constant(_) => write!(f, "PUSH_CONST"),
            OpCode::Pop => write!(f, "POP"),
            OpCode::GetGlobal(_) => write!(f, "GET_GLOBAL"),
            OpCode::SetGlobal(_) => write!(f, "SET_GLOBAL"),
            OpCode::DefGlobal(_) => write!(f, "DEF_GLOBAL"),
            OpCode::GetLocal(_) => write!(f, "GET_LOCAL"),
            OpCode::SetLocal(_) => write!(f, "SET_LOCAL"),
            OpCode::GetUpvalue(_) => write!(f, "GET_UPVALUE"),
            OpCode::SetUpvalue(_) => write!(f, "SET_UPVALUE"),
            OpCode::CloseUpvalue => write!(f, "CLOSE_UPVALUE"),
            OpCode::Add => write!(f, "ADD"),
            OpCode::Sub => write!(f, "SUB"),
            OpCode::Mul => write!(f, "MUL"),
            OpCode::Div => write!(f, "DIV"),
            OpCode::Mod => write!(f, "MOD"),
            OpCode::Eq(_) => write!(f, "EQ"),
            OpCode::Lt(_) => write!(f, "LT"),
            OpCode::Le(_) => write!(f, "LE"),
            OpCode::Gt(_) => write!(f, "GT"),
            OpCode::Ge(_) => write!(f, "GE"),
            OpCode::JumpIfFalse(_) => write!(f, "JUMP_IF_FALSE"),
            OpCode::Jump(_) => write!(f, "JUMP"),
            OpCode::Call(_) => write!(f, "CALL"),
            OpCode::TailCall(_) => write!(f, "TAIL_CALL"),
            OpCode::Return => write!(f, "RETURN"),
            OpCode::MakeClosure(_, _, _) => write!(f, "MAKE_CLOSURE"),
            OpCode::Cons => write!(f, "CONS"),
            OpCode::Car => write!(f, "CAR"),
            OpCode::Cdr => write!(f, "CDR"),
        }
    }
}

// Função para exibir o bytecode de forma legível
pub fn disassemble_chunk(chunk: &Chunk, name: &str, heap: &Heap) {
    println!("=== Bytecode: {} ===", name);

    for (offset, instruction) in chunk.code.iter().enumerate() {
        print!("{:04}  ", offset);

        match instruction {
            OpCode::Constant(idx) => println!(
                "{:<18} {:?}",
                instruction,
                chunk.constants[*idx],
                // lisp_fmt(chunk.constants[*idx], heap)
            ),
            OpCode::JumpIfFalse(t) => println!("{:<18} {:04}", instruction, t),
            OpCode::Jump(t) => println!("{:<18} {:04}", instruction, t),
            OpCode::Call(n) => println!("{:<18} ({} args)", instruction, n),
            OpCode::TailCall(n) => println!("{:<18} ({} args)", instruction, n),
            OpCode::Add => println!("{}", instruction),
            OpCode::Sub => println!("{}", instruction),
            OpCode::Mul => println!("{}", instruction),
            OpCode::Div => println!("{}", instruction),
            OpCode::Mod => println!("{}", instruction),
            OpCode::Eq(n) => println!("{:<18} ({} args)", instruction, n),
            OpCode::Lt(n) => println!("{:<18} ({} args)", instruction, n),
            OpCode::Gt(n) => println!("{:<18} ({} args)", instruction, n),
            OpCode::Le(n) => println!("{:<18} ({} args)", instruction, n),
            OpCode::Ge(n) => println!("{:<18} ({} args)", instruction, n),
            OpCode::Pop => println!("{}", instruction),
            OpCode::Return => println!("{}", instruction),
            OpCode::MakeClosure(params, closure_chunk, upvalues) => {
                println!("{:<18} {:?}", instruction, params);
                println!("\n  [Closure Start {:?}]", params);
                disassemble_chunk(closure_chunk, "Closure Body", heap);
                println!("  [Closure End]\n");
            }
            OpCode::Cons => println!("{}", instruction),
            OpCode::Car => println!("{}", instruction),
            OpCode::Cdr => println!("{}", instruction),
            OpCode::GetGlobal(name) => println!("{} '{}'", instruction, name),
            OpCode::SetGlobal(name) => println!("{} '{}'", instruction, name),
            OpCode::DefGlobal(name) => println!("{} '{}'", instruction, name),
            OpCode::GetLocal(idx) => println!("{}({})", instruction, idx),
            OpCode::SetLocal(idx) => println!("{}({})", instruction, idx),
            OpCode::GetUpvalue(idx) => println!("{}({})", instruction, idx),
            OpCode::SetUpvalue(idx) => println!("{}({})", instruction, idx),
            OpCode::CloseUpvalue => println!("{}", instruction),
        }
    }
    println!("======================\n");
}
