use std::{fmt::Display, rc::Rc};

use crate::{
    env::{Env, LispEnv},
    evaluate::eval,
    expr::{LispExp, lisp_fmt},
    heap::Heap,
    helpers::{env_set, get_float},
};

#[derive(Clone, Debug)]
pub enum OpCode {
    PushConst(LispExp),
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
}

impl Chunk {
    pub fn new() -> Self {
        Chunk { code: vec![] }
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
    pub stack: Vec<LispExp>,
    pub frames: Vec<CallFrame>,
}

pub struct CallFrame {
    pub chunk: Rc<Chunk>,
    pub ip: usize,
    pub env: Env,
}

fn pop_float(stack: &mut Vec<LispExp>, heap: &Heap) -> Result<f64, String> {
    let val = stack.pop().ok_or("Empty stack")?;
    get_float(&val, heap)
}

macro_rules! vm_math {
    ($op:tt, $self:ident, $heap:expr) => {{
          let b = pop_float(&mut $self.stack, $heap)?;
          let a = pop_float(&mut $self.stack, $heap)?;
          $self.stack.push(LispExp::Number(a $op b));
      }};
}

macro_rules! vm_cmp_n {
    ($op:tt, $arity:expr, $self:expr, $heap:expr) => {{
        let mut args = Vec::with_capacity($arity);
        for _ in 0..$arity {
            args.push($self.stack.pop().unwrap_or(LispExp::Void));
        }
        args.reverse();

        let mut is_true = true;
        for i in 0..($arity - 1) {
            let a = get_float(&args[i], $heap)?;
            let b = get_float(&args[i+1], $heap)?;
            if !(a $op b) {
                is_true = false;
                break;
            }
        }

        $self.stack.push(LispExp::Bool(is_true));
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
    ) -> Result<LispExp, String> {
        self.frames.push(CallFrame { chunk, ip: 0, env });

        loop {
            if self.frames.is_empty() {
                return Ok(self.stack.pop().unwrap_or(LispExp::Void));
            }

            let frame = self.frames.last_mut().unwrap();
            let op = &frame.chunk.code[frame.ip].clone();
            frame.ip += 1;

            match op {
                OpCode::PushConst(val) => self.stack.push(val.clone()),
                OpCode::Pop => _ = self.stack.pop(),
                OpCode::GetVar(name) => {
                    let val = LispEnv::get(&frame.env, name)
                        .ok_or_else(|| format!("Variable not found: {}", name))?;
                    self.stack.push(val);
                }
                OpCode::DefVar(name) => {
                    let val = self.stack.pop().unwrap_or(LispExp::Void);
                    frame.env.borrow_mut().data.insert(name.clone(), val);
                    self.stack.push(LispExp::Void);
                }
                OpCode::SetVar(name) => {
                    let val = self.stack.pop().unwrap_or(LispExp::Void);
                    env_set(&frame.env, &name, val)?;
                    self.stack.push(LispExp::Void);
                }
                OpCode::Add => vm_math!(+, self, &heap),
                OpCode::Sub => vm_math!(-, self, &heap),
                OpCode::Mul => vm_math!(*, self, &heap),
                OpCode::Div => vm_math!(/, self, &heap),
                OpCode::Mod => vm_math!(%, self, &heap),
                OpCode::Eq(n) => vm_cmp_n!(==, *n, self, &heap),
                OpCode::Lt(n) => vm_cmp_n!(<, *n, self, &heap),
                OpCode::Le(n) => vm_cmp_n!(<=, *n, self, &heap),
                OpCode::Gt(n) => vm_cmp_n!(>, *n, self, &heap),
                OpCode::Ge(n) => vm_cmp_n!(>=, *n, self, &heap),
                OpCode::JumpIfFalse(address) => {
                    let cond = self.stack.pop().unwrap();
                    let is_false = matches!(cond, LispExp::Bool(false));
                    if is_false {
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
                    args.reverse(); // saem da pilha de trás pra frente

                    let func = self.stack.pop().unwrap_or(LispExp::Void);
                    match func {
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
                                new_env.borrow_mut().data.insert(p.clone(), a.clone());
                            }

                            // TCO: Substitui frame atual em vez de empilhar
                            if is_tail {
                                let frame = self.frames.last_mut().unwrap();
                                frame.chunk = chunk;
                                frame.ip = 0;
                                frame.env = new_env;
                            } else {
                                self.frames.push(CallFrame {
                                    chunk,
                                    ip: 0,
                                    env: new_env,
                                });
                            }
                        }
                        LispExp::Lambda(lambda) => {
                            if let LispExp::List(params) = &*lambda.params {
                                if args.len() != params.len() {
                                    return Err("Incorrect arity".to_string());
                                }

                                let mut new_env = LispEnv::new(Some(lambda.env.clone()));
                                for (param, arg) in params.iter().zip(args.iter()) {
                                    if let LispExp::Symbol(name) = param {
                                        new_env.borrow_mut().data.insert(name.clone(), arg.clone());
                                    }
                                }

                                let result = eval((*lambda.body).clone(), &mut new_env, heap)?;

                                if is_tail {
                                    self.frames.pop();
                                    if self.frames.is_empty() {
                                        return Ok(result);
                                    }
                                }
                                self.stack.push(result);
                            } else {
                                return Err("Invalid lambda parameters".to_string());
                            }
                        }
                        _ => {
                            return Err(format!(
                                "Object '{}' is not callable",
                                lisp_fmt(&func, &heap)
                            ));
                        }
                    }
                }
                OpCode::Return => {
                    let result = self.stack.pop().unwrap_or(LispExp::Void);
                    self.frames.pop();
                    self.stack.push(result);
                }
                OpCode::MakeClosure(params, clos_chunk) => {
                    let closure = LispExp::VmClosure {
                        params: params.clone(),
                        chunk: clos_chunk.clone(),
                        env: frame.env.clone(),
                    };

                    self.stack.push(closure);
                }
                OpCode::Cons => {
                    let b = self.stack.pop().unwrap_or(LispExp::Void);
                    let a = self.stack.pop().unwrap_or(LispExp::Void);

                    match b {
                        LispExp::List(mut vec) => {
                            vec.insert(0, a);
                            self.stack.push(LispExp::List(vec));
                        }
                        _ => self.stack.push(LispExp::Pair(heap.alloc(a), heap.alloc(b))),
                    }
                }
                OpCode::Car => {
                    let obj = self.stack.pop().unwrap_or(LispExp::Void);
                    match obj {
                        LispExp::Pair(car, _) => match heap.get(car) {
                            Some(val) => self.stack.push(val.clone()),
                            None => return Err("'car' expects a pair or list".to_string()),
                        },
                        LispExp::List(vec) => {
                            if vec.is_empty() {
                                return Err("car: empty list".to_string());
                            }
                            self.stack.push(vec[0].clone());
                        }
                        _ => {
                            return Err(format!(
                                "'car' expects a pair or list, but got: {}",
                                lisp_fmt(&obj, &heap)
                            ));
                        }
                    }
                }
                OpCode::Cdr => {
                    let obj = self.stack.pop().unwrap_or(LispExp::Void);
                    match obj {
                        LispExp::Pair(_, cdr) => match heap.get(cdr) {
                            Some(val) => self.stack.push(val.clone()),
                            None => return Err("'cdr' expects a pair or list".to_string()),
                        },
                        LispExp::List(vec) => {
                            if vec.is_empty() {
                                return Err("cdr: empty list".to_string());
                            }
                            self.stack.push(LispExp::List(vec[1..].to_vec()));
                        }
                        _ => {
                            return Err(format!(
                                "'cdr' expects a pair or list, but got: {}",
                                lisp_fmt(&obj, &heap)
                            ));
                        }
                    }
                }
            }
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::PushConst(_) => write!(f, "PUSH_CONST"),
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
            OpCode::PushConst(val) => println!("{:<18} {}", instruction, lisp_fmt(&val, heap)),
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
