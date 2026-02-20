use crate::value::Value;
use crate::{env::Env, expr::LispExp};

pub struct Heap {
    pub memory: Vec<LispExp>,
    pub free_list: Vec<usize>,
    pub marked: Vec<bool>,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            memory: vec![],
            free_list: vec![],
            marked: vec![],
        }
    }

    pub fn alloc(&mut self, exp: LispExp) -> Value {
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

    pub fn mark_val(&mut self, val: Value) {
        if val.is_gc_ref() {
            self.mark(val.as_gc_ref());
        }
    }

    pub fn mark(&mut self, index: usize) {
        if self.marked[index] {
            return;
        }

        self.marked[index] = true;

        let exp = self.memory[index].clone();

        match exp {
            LispExp::Pair(car, cdr) => {
                self.mark_val(car);
                self.mark_val(cdr);
            }
            LispExp::Vector(vec) => {
                for v in vec {
                    self.mark_val(v);
                }
            }
            LispExp::HashMap(map) => {
                for v in map.values() {
                    self.mark_val(*v);
                }
            }
            LispExp::VmClosure {
                params: _,
                chunk,
                env,
            } => {
                let mut curr_env = Some(env.clone());
                while let Some(env_ref) = curr_env {
                    let env_borrowed = env_ref.borrow();

                    for value in env_borrowed.data.values() {
                        self.mark_val(*value);
                    }

                    curr_env = env_borrowed.outer.clone();
                }

                for const_val in &chunk.constants {
                    self.mark_val(*const_val);
                }
            }
            _ => {}
        }
    }

    pub fn mark_lisp_exp(&mut self, exp: &LispExp) {
        match exp {
            LispExp::HeapPtr(val) => self.mark_val(*val),
            LispExp::Pair(car, cdr) => {
                self.mark_val(*car);
                self.mark_val(*cdr);
            }
            LispExp::List(l) => {
                for item in l {
                    self.mark_lisp_exp(item);
                }
            }
            _ => {}
        }
    }

    pub fn sweep(&mut self, debug_gc: bool) {
        let mut bytes_freed = 0;

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
            heap.mark_val(*value);
        }

        curr_env = env_borrowed.outer.clone();
    }

    heap.mark_val(protected_value);

    for value in vm_stack {
        heap.mark_val(*value);
    }

    heap.sweep(debug_gc);
}
