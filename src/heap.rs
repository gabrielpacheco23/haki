use crate::{
    env::Env,
    expr::{GcRef, LispExp},
};

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

    pub fn alloc(&mut self, exp: LispExp) -> GcRef {
        if let Some(index) = self.free_list.pop() {
            self.memory[index] = exp;
            self.marked[index] = false;
            GcRef(index)
        } else {
            let index = self.memory.len();
            self.memory.push(exp);
            self.marked.push(false);
            GcRef(index)
        }
    }

    pub fn get(&self, gc_rec: GcRef) -> Option<&LispExp> {
        self.memory.get(gc_rec.0)
    }

    pub fn get_mut(&mut self, gc_ref: GcRef) -> Option<&mut LispExp> {
        self.memory.get_mut(gc_ref.0)
    }

    pub fn clear_marks(&mut self) {
        for m in self.marked.iter_mut() {
            *m = false;
        }
    }

    pub fn mark(&mut self, gc_ref: GcRef) {
        let index = gc_ref.0;

        if self.marked[index] {
            return;
        }

        self.marked[index] = true;

        let exp = self.memory[index].clone();

        match exp {
            LispExp::Pair(car, cdr) => {
                self.mark(car);
                self.mark(cdr);
            }
            LispExp::Vector(vec_ref) => self.mark(vec_ref),
            LispExp::HashMap(map_ref) => self.mark(map_ref),
            LispExp::VectorData(data) => {
                for item in data {
                    self.mark_lisp_exp(&item);
                }
            }
            LispExp::HashMapData(map) => {
                for val in map.values() {
                    self.mark_lisp_exp(val);
                }
            }
            LispExp::VmClosure {
                params: _,
                chunk: _,
                env,
            } => {
                let mut curr_env = Some(env.clone());
                while let Some(env_ref) = curr_env {
                    let env_borrowed = env_ref.borrow();

                    for (_name, value) in env_borrowed.data.iter() {
                        self.mark_lisp_exp(value);
                    }

                    curr_env = env_borrowed.outer.clone();
                }
            }
            _ => {}
        }
    }

    pub fn mark_lisp_exp(&mut self, exp: &LispExp) {
        match exp {
            LispExp::Pair(car, cdr) => {
                self.mark(*car);
                self.mark(*cdr);
            }
            LispExp::Vector(v) | LispExp::HashMap(v) => self.mark(*v),
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
    protected_value: &LispExp,
    vm_stack: &[LispExp],
    debug_gc: bool,
) {
    heap.clear_marks();

    let mut curr_env = Some(env.clone());
    while let Some(env_ref) = curr_env {
        let env_borrowed = env_ref.borrow();

        for (_name, value) in env_borrowed.data.iter() {
            heap.mark_lisp_exp(value);
        }

        curr_env = env_borrowed.outer.clone();
    }

    heap.mark_lisp_exp(protected_value);

    for value in vm_stack {
        heap.mark_lisp_exp(value);
    }

    heap.sweep(debug_gc);
}
