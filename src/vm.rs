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
    GetVar(String),
    DefVar(String),
    SetVar(String),
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
    MakeClosure(Vec<String>, Rc<Chunk>),
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
}

pub struct CallFrame {
    pub chunk: Rc<Chunk>,
    pub ip: usize,
    pub env: Env,
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
            stack: vec![],
            frames: vec![],
        }
    }

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
        self.frames.push(CallFrame { chunk, ip: 0, env });

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
                OpCode::GetVar(name) => {
                    let val = LispEnv::get(&frame.env.borrow(), name)
                        .ok_or_else(|| format!("Variable not found: {}", name))?;
                    self.stack.push(val);
                }
                OpCode::DefVar(name) => {
                    let val = self.stack.pop().unwrap_or(Value::void());
                    frame.env.borrow_mut().data.insert(name.clone(), val);
                    self.stack.push(Value::void());
                }
                OpCode::SetVar(name) => {
                    let val = self.stack.pop().unwrap_or(Value::void());
                    // env_set(&frame.env, &name, val)?;
                    frame.env.borrow_mut().set(&name, val)?;
                    self.stack.push(Value::void());
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

                    let mut args = vec![];
                    for _ in 0..*arg_count {
                        args.push(self.stack.pop().unwrap());
                    }
                    args.reverse();

                    let func_val = self.stack.pop().unwrap_or(Value::void());
                    if !func_val.is_gc_ref() {
                        return Err("Object is not callable".to_string());
                    }

                    let func_exp = heap.get(func_val).unwrap().clone();
                    match func_exp {
                        LispExp::Native(f) => {
                            let curr_env = &mut self.frames.last_mut().unwrap().env;
                            let result = f(&args, curr_env, heap)?;

                            if is_tail {
                                self.frames.pop();
                                if self.frames.is_empty() {
                                    return Ok(result);
                                }
                            }
                            self.stack.push(result);
                        }

                        LispExp::VmClosure {
                            params,
                            chunk,
                            env: closure_env,
                        } => {
                            if args.len() != params.len() {
                                return Err("Incorrect arity".to_string());
                            }
                            let new_env = LispEnv::new(Some(closure_env));
                            for (p, a) in params.iter().zip(args.iter()) {
                                new_env.borrow_mut().data.insert(p.clone(), *a);
                            }
                            if is_tail {
                                let frame = self.frames.last_mut().unwrap();
                                frame.ip = 0;
                                frame.env = new_env;
                                frame.chunk = chunk.clone();
                            } else {
                                self.frames.push(CallFrame {
                                    chunk: chunk.clone(),
                                    ip: 0,
                                    env: new_env,
                                });
                            }
                        }
                        LispExp::Lambda(lambda) => {
                            let mut new_env = LispEnv::new(Some(lambda.env.clone()));
                            let params_flat = pairs_to_vec(&*lambda.params, heap);
                            if let LispExp::List(params, _) = params_flat {
                                if args.len() != params.len() {
                                    return Err("Incorrect arity".to_string());
                                }
                                for (param, arg) in params.iter().zip(args.iter()) {
                                    if let LispExp::Symbol(name, _) = param {
                                        new_env.borrow_mut().insert(name.clone(), *arg);
                                    }
                                }
                            }

                            let result_ast = eval((*lambda.body).clone(), &mut new_env, heap)?;
                            let resul_val = ast_to_value(&result_ast, heap);

                            if is_tail {
                                self.frames.pop();
                                if self.frames.is_empty() {
                                    return Ok(resul_val);
                                }
                            }
                            self.stack.push(resul_val);
                        }
                        _ => return Err("Object is not callable".to_string()),
                    }
                }
                OpCode::Return => {
                    let result = self.stack.pop().unwrap_or(Value::void());
                    self.frames.pop();
                    self.stack.push(result);
                }
                OpCode::MakeClosure(params, clos_chunk) => {
                    let closure = LispExp::VmClosure {
                        params: params.clone(),
                        chunk: clos_chunk.clone(),
                        env: frame.env.clone(),
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
                // OpCode::Car => {
                //     let obj = self.stack.pop().unwrap_or(Value::void());
                //     if obj.is_gc_ref() {
                //         if let Some(LispExp::Pair(car, _cdr)) = heap.get(obj) {
                //             self.stack.push(*car);
                //             continue;
                //         }
                //     }
                //     return Err("'car' requires a pair or list".to_string());
                // }
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
                } // OpCode::Cdr => {
                  //     let obj = self.stack.pop().unwrap_or(Value::void());
                  //     if obj.is_gc_ref() {
                  //         if let Some(LispExp::Pair(_car, cdr)) = heap.get(obj) {
                  //             self.stack.push(*cdr);
                  //             continue;
                  //         }
                  //     }
                  //     return Err("'cdr' requires a pair or list".to_string());
                  // }
            }
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Constant(_) => write!(f, "PUSH_CONST"),
            OpCode::Pop => write!(f, "POP"),
            OpCode::GetVar(_) => write!(f, "GET_VAR"),
            OpCode::DefVar(_) => write!(f, "DEF_VAR"),
            OpCode::SetVar(_) => write!(f, "SET_VAR"),
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
            OpCode::MakeClosure(_, _) => write!(f, "MAKE_CLOSURE"),
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
            OpCode::GetVar(name) => println!("{:<18} '{}'", instruction, name),
            OpCode::DefVar(name) => println!("{:<18} '{}'", instruction, name),
            OpCode::SetVar(name) => println!("{:<18} '{}'", instruction, name),
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
            OpCode::MakeClosure(params, closure_chunk) => {
                println!("{:<18} {:?}", instruction, params);
                println!("\n  [Closure Start {:?}]", params);
                disassemble_chunk(closure_chunk, "Closure Body", heap);
                println!("  [Closure End]\n");
            }
            OpCode::Cons => println!("{}", instruction),
            OpCode::Car => println!("{}", instruction),
            OpCode::Cdr => println!("{}", instruction),
        }
    }
    println!("======================\n");
}
