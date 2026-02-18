use crate::env::Env;
use crate::heap::Heap;
use crate::vm::Chunk;

use std::collections::HashMap as RustHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcRef(pub usize);

pub type LispNative = Rc<dyn Fn(&[LispExp], &mut Env, &mut Heap) -> Result<LispExp, String>>;

#[derive(Clone, Debug)]
pub struct LispLambda {
    pub params: Rc<LispExp>,
    pub body: Rc<LispExp>,
    pub env: Env,
}

// Atom = Symbol or Number
#[derive(Clone)]
pub enum LispExp {
    Symbol(String),
    Number(f64),
    Bool(bool),
    Str(String),
    Nil,
    List(Vec<LispExp>),
    Native(LispNative),
    Lambda(LispLambda),
    Macro(LispLambda),
    Void,
    Pair(GcRef, GcRef),
    Vector(GcRef),
    HashMap(GcRef),
    VectorData(Vec<LispExp>),
    HashMapData(RustHashMap<String, LispExp>),
    VmClosure {
        params: Vec<String>,
        chunk: Rc<Chunk>,
        env: Env,
    },
}

use std::rc::Rc;

pub struct LispFmt<'a> {
    pub exp: &'a LispExp,
    pub heap: &'a Heap,
}

pub fn lisp_fmt<'a>(exp: &'a LispExp, heap: &'a Heap) -> LispFmt<'a> {
    LispFmt { exp, heap }
}

impl<'a> std::fmt::Display for LispFmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.exp {
            LispExp::Number(n) => write!(f, "{}", n),
            // LispExp::Str(s) => write!(f, "\"{}\"", s),
            LispExp::Str(s) => write!(f, "{}", s),
            LispExp::Symbol(s) => write!(f, "{}", s),
            LispExp::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            LispExp::Nil => write!(f, "()"),
            LispExp::Void => write!(f, ""),
            LispExp::Pair(car, cdr) => {
                write!(f, "(")?;

                let car_val = self.heap.get(*car).unwrap();
                write!(f, "{}", lisp_fmt(&car_val, self.heap))?;

                let mut current = *cdr;
                while let Some(LispExp::Pair(next_car, next_cdr)) = self.heap.get(current) {
                    let next_car_val = self.heap.get(*next_car).unwrap();
                    write!(f, " {}", lisp_fmt(&next_car_val, self.heap))?;
                    current = *next_cdr;
                }

                if let Some(LispExp::Nil) = self.heap.get(current) {
                    write!(f, ")")
                } else {
                    let last_val = self.heap.get(current).unwrap();
                    write!(f, " . {})", lisp_fmt(&last_val, self.heap))
                }
            }

            LispExp::Vector(vec_ref) => {
                if let Some(LispExp::VectorData(data)) = self.heap.get(*vec_ref) {
                    write!(f, "[")?;
                    for (i, item) in data.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", lisp_fmt(&item, self.heap))?;
                    }
                    write!(f, "]")
                } else {
                    write!(f, "<invalid-vector-ref>")
                }
            }

            LispExp::HashMap(map_ref) => {
                if let Some(LispExp::HashMapData(data)) = self.heap.get(*map_ref) {
                    write!(f, "{} ", '{')?;
                    for (i, item) in data.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(
                            f,
                            "{}: ",
                            lisp_fmt(&LispExp::Str(item.0.clone()), self.heap)
                        )?;
                        write!(f, "{}", lisp_fmt(&item.1, self.heap))?;
                        if i != data.len() - 1 {
                            write!(f, ",")?;
                        }
                    }
                    write!(f, " {}", '}')
                } else {
                    write!(f, "<invalid-hashmap-ref>")
                }
            }

            LispExp::VectorData(_) => write!(f, "<vector-data>"),
            LispExp::HashMapData(_) => write!(f, "<hashmap-data>"),
            _ => write!(f, "<unknown>"),
        }
    }
}

use std::fmt;
impl fmt::Debug for LispExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", lisp_fmt(self, &Heap::new()))
    }
}

impl PartialEq for LispExp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LispExp::Symbol(a), LispExp::Symbol(b)) => a == b,
            (LispExp::Str(a), LispExp::Str(b)) => a == b,
            (LispExp::Number(a), LispExp::Number(b)) => a == b,
            (LispExp::Bool(a), LispExp::Bool(b)) => a == b,

            (LispExp::List(a), LispExp::List(b)) => a == b,
            (LispExp::Vector(a), LispExp::Vector(b)) => a == b,
            (LispExp::HashMap(a), LispExp::HashMap(b)) => a == b,
            (LispExp::Native(a), LispExp::Native(b)) => Rc::ptr_eq(a, b),
            (LispExp::Lambda(a), LispExp::Lambda(b)) => *a.params == *b.params && a.body == b.body,
            (LispExp::Pair(a1, a2), LispExp::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (
                LispExp::VmClosure {
                    params: a_p,
                    chunk: a_c,
                    ..
                },
                LispExp::VmClosure {
                    params: b_p,
                    chunk: b_c,
                    ..
                },
            ) => a_p == b_p && Rc::ptr_eq(a_c, b_c),
            (LispExp::Nil, LispExp::Nil) | (LispExp::Void, LispExp::Void) => true,
            _ => false,
        }
    }
}

pub fn is_deep_equal(a: &LispExp, b: &LispExp, heap: &Heap) -> bool {
    use LispExp::*;
    match (a, b) {
        (Number(n1), Number(n2)) => n1 == n2,
        (Str(s1), Str(s2)) => s1 == s2,
        (Symbol(s1), Symbol(s2)) => s1 == s2,
        (Bool(b1), Bool(b2)) => b1 == b2,
        (Nil, Nil) | (Void, Void) => true,
        (Pair(car1, cdr1), Pair(car2, cdr2)) => {
            let v_car1 = heap.get(*car1).unwrap();
            let v_car2 = heap.get(*car2).unwrap();

            if !is_deep_equal(v_car1, v_car2, heap) {
                return false;
            }

            let v_cdr1 = heap.get(*cdr1).unwrap();
            let v_cdr2 = heap.get(*cdr2).unwrap();

            is_deep_equal(v_cdr1, v_cdr2, heap)
        }

        (Vector(ref1), Vector(ref2)) => {
            if ref1 == ref2 {
                return true;
            }

            if let (Some(VectorData(vec1)), Some(VectorData(vec2))) =
                (heap.get(*ref1), heap.get(*ref2))
            {
                if vec1.len() != vec2.len() {
                    return false;
                }

                for (i, j) in vec1.iter().zip(vec2.iter()) {
                    if !is_deep_equal(i, j, heap) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        }

        (HashMap(ref1), HashMap(ref2)) => {
            if ref1 == ref2 {
                return true;
            }

            if let (Some(HashMapData(map1)), Some(HashMapData(map2))) =
                (heap.get(*ref1), heap.get(*ref2))
            {
                if map1.len() != map2.len() {
                    return false;
                }

                for (key, val1) in map1.iter() {
                    match map2.get(key) {
                        Some(val2) => {
                            if !is_deep_equal(val1, val2, heap) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }
                true
            } else {
                false
            }
        }

        _ => false,
    }
}
