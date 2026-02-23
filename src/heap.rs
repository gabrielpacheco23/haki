use crate::upvalue::UpvalueState;
use crate::value::Value;
use crate::vm::{Chunk, OpCode};
use crate::{env::Env, expr::LispExp};
use std::collections::HashMap as RustHashMap;

pub struct Heap {
    pub memory: Vec<LispExp>,
    pub free_list: Vec<usize>,
    pub marked: Vec<bool>,
    pub string_pool: RustHashMap<String, Value>,

    pub worklist: Vec<Value>,

    pub threshold: usize,
    pub needs_gc: bool,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            memory: vec![],
            free_list: vec![],
            marked: vec![],
            string_pool: RustHashMap::new(),
            worklist: Vec::with_capacity(1024), // Pré-aloca espaço para o GC
            threshold: 100_000,
            needs_gc: false,
        }
    }

    pub fn alloc_string(&mut self, s: String) -> Value {
        if let Some(val) = self.string_pool.get(&s) {
            return *val;
        }

        let val = self.alloc(LispExp::Str(s.clone()));
        self.string_pool.insert(s, val);
        val
    }

    pub fn alloc(&mut self, exp: LispExp) -> Value {
        // (Nota: Lembre-se de reativar a chamada automática do GC aqui depois!)
        if self.memory.len() > self.threshold {
            self.needs_gc = true;
        }

        if let Some(index) = self.free_list.pop() {
            self.memory[index] = exp;
            self.marked[index] = false;
            Value::gc_ref(index)
        } else {
            let index = self.memory.len();
            self.memory.push(exp);
            self.marked.push(false);
            Value::gc_ref(index)
        }
    }

    pub fn get(&self, val: Value) -> Option<&LispExp> {
        if val.is_gc_ref() {
            self.memory.get(val.as_gc_ref())
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, val: Value) -> Option<&mut LispExp> {
        if val.is_gc_ref() {
            self.memory.get_mut(val.as_gc_ref())
        } else {
            None
        }
    }

    pub fn clear_marks(&mut self) {
        for m in self.marked.iter_mut() {
            *m = false;
        }
    }

    // ==========================================
    // NOVO MOTOR DO GARBAGE COLLECTOR
    // ==========================================

    pub fn mark_val(&mut self, val: Value) {
        if val.is_gc_ref() {
            let index = val.as_gc_ref();
            if index < self.memory.len() {
                self.worklist.push(val);
            }
        }
    }

    /// Esvazia o "Carrinho de Mão" processando todos os objetos pendentes
    pub fn process_worklist(&mut self) {
        while let Some(val) = self.worklist.pop() {
            if !val.is_gc_ref() {
                continue;
            }
            let index = val.as_gc_ref();

            // Se já foi marcado, pula (evita loops infinitos em referências circulares Lisp!)
            if self.marked[index] {
                continue;
            }

            self.marked[index] = true;

            let exp = self.memory[index].clone();

            match exp {
                LispExp::Pair(car, cdr) => {
                    // Empurra os filhos para a fila! Sem usar a Pilha do Processador!
                    self.worklist.push(car);
                    self.worklist.push(cdr);
                }
                LispExp::Vector(vec) => {
                    for v in vec {
                        self.worklist.push(v);
                    }
                }
                LispExp::HashMap(map) => {
                    for v in map.values() {
                        self.worklist.push(*v);
                    }
                }
                LispExp::VmClosure {
                    params: _,
                    chunk,
                    upvalues,
                } => {
                    for upvalue in upvalues {
                        if let UpvalueState::Closed(v) = *upvalue.state.borrow() {
                            self.worklist.push(v);
                        }
                    }
                    self.mark_chunk(&chunk);
                }
                LispExp::List(l, _) => {
                    for item in l {
                        self.mark_lisp_exp(&item);
                    }
                }
                _ => {}
            }
        }
    }

    pub fn mark_chunk(&mut self, chunk: &Chunk) {
        for const_val in &chunk.constants {
            self.mark_val(*const_val);
        }

        for op in &chunk.code {
            if let OpCode::MakeClosure(_, inner_chunk, _upvals) = op {
                self.mark_chunk(inner_chunk);
            }
        }
    }

    pub fn mark_lisp_exp(&mut self, exp: &LispExp) {
        match exp {
            LispExp::HeapPtr(val) => self.mark_val(*val),
            LispExp::Pair(car, cdr) => {
                self.mark_val(*car);
                self.mark_val(*cdr);
            }
            LispExp::List(l, _) => {
                for item in l {
                    self.mark_lisp_exp(item);
                }
            }
            _ => {}
        }
    }

    pub fn sweep(&mut self, debug_gc: bool) {
        let mut bytes_freed = 0;

        self.string_pool
            .retain(|_, val| self.marked[val.as_gc_ref()]);

        for i in 0..self.memory.len() {
            if !self.marked[i] && !matches!(self.memory[i], LispExp::Void) {
                self.memory[i] = LispExp::Void;

                self.free_list.push(i);
                bytes_freed += 1;
            }
        }

        if bytes_freed > 0 && debug_gc {
            println!("[GC] Cleaned {} inactive object(s).", bytes_freed);
        }
    }
}

pub fn collect_garbage(
    heap: &mut Heap,
    env: &Env,
    protected_value: Value,
    vm_stack: &[Value],
    debug_gc: bool,
) {
    heap.clear_marks();

    let mut curr_env = Some(env.clone());
    while let Some(env_ref) = curr_env {
        let env_borrowed = env_ref.borrow();

        for (_name, value) in env_borrowed.data.iter() {
            // Joga as globais no carrinho...
            heap.mark_val(*value);
        }

        curr_env = env_borrowed.outer.clone();
    }

    // Joga a pilha da VM no carrinho...
    heap.mark_val(protected_value);
    for value in vm_stack {
        heap.mark_val(*value);
    }

    // O GRANDE MOMENTO: Aciona o motor iterativo para marcar tudo com segurança!
    heap.process_worklist();

    heap.sweep(debug_gc);
}
