use crate::env::Env;
use crate::heap::Heap;
use crate::helpers::ast_to_value;
use crate::value::Value;
use crate::vm::Chunk;

use std::collections::HashMap as RustHashMap;

pub type LispNative = Rc<dyn Fn(&[Value], &mut Env, &mut Heap) -> Result<Value, String>>;

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
    Void,
    List(Vec<LispExp>),
    Native(LispNative),
    Lambda(LispLambda),
    Macro(LispLambda),
    Pair(Value, Value),
    HeapPtr(Value),
    Vector(Vec<Value>),
    HashMap(RustHashMap<String, Value>),
    VmClosure {
        params: Vec<String>,
        chunk: Rc<Chunk>,
        env: Env,
    },
}

use std::rc::Rc;

// ==========================================
// 1. O FORMATADOR DA ÁRVORE (Entende `&LispExp`)
// ==========================================
pub struct AstFmt<'a> {
    pub exp: &'a LispExp,
    pub heap: &'a Heap,
}

impl<'a> std::fmt::Display for AstFmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.exp {
            LispExp::Number(n) => write!(f, "{}", n),
            LispExp::Str(s) => write!(f, "{}", s),
            LispExp::Symbol(s) => write!(f, "{}", s),
            LispExp::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            LispExp::Nil => write!(f, "()"),
            LispExp::Void => write!(f, ""),
            LispExp::Native(_) | LispExp::Lambda(_) | LispExp::VmClosure { .. } => {
                write!(f, "<procedure>")
            }

            LispExp::List(vec) => {
                write!(f, "(")?;
                for (i, item) in vec.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(
                        f,
                        "{}",
                        AstFmt {
                            exp: item,
                            heap: self.heap
                        }
                    )?;
                }
                write!(f, ")")
            }

            LispExp::Pair(car, cdr) => {
                write!(f, "(")?;
                write!(f, "{}", lisp_fmt(*car, self.heap))?;

                let mut current = *cdr;
                loop {
                    if current.is_nil() {
                        write!(f, ")")?;
                        break;
                    }
                    if !current.is_gc_ref() {
                        write!(f, " . {}", lisp_fmt(current, self.heap))?;
                        write!(f, ")")?;
                        break;
                    }
                    if let Some(LispExp::Pair(next_car, next_cdr)) = self.heap.get(current) {
                        write!(f, " {}", lisp_fmt(*next_car, self.heap))?;
                        current = *next_cdr;
                    } else {
                        write!(f, " . {}", lisp_fmt(current, self.heap))?;
                        write!(f, ")")?;
                        break;
                    }
                }
                Ok(())
            }

            LispExp::Vector(vec) => {
                write!(f, "[")?;
                for (i, item) in vec.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", lisp_fmt(*item, self.heap))?;
                }
                write!(f, "]")
            }

            LispExp::HashMap(map) => {
                write!(f, "{{ ")?;
                for (i, (k, val)) in map.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\": {}", k, lisp_fmt(*val, self.heap))?;
                }
                write!(f, " }}")
            }
            _ => write!(f, "<unknown>"),
        }
    }
}

pub struct LispFmt<'a> {
    pub val: Value,
    pub heap: &'a Heap,
}

pub fn lisp_fmt<'a>(val: Value, heap: &'a Heap) -> LispFmt<'a> {
    LispFmt { val, heap }
}

impl<'a> std::fmt::Display for LispFmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v = self.val;
        if v.is_number() {
            write!(f, "{}", v.as_number())
        } else if v.is_boolean() {
            write!(f, "{}", if v.as_boolean() { "#t" } else { "#f" })
        } else if v.is_nil() {
            write!(f, "()")
        } else if v.is_void() {
            write!(f, "")
        } else if v.is_gc_ref() {
            if let Some(exp) = self.heap.get(v) {
                // A MÁGICA 3: Desempacotou do Galpão? Repassa pro AstFmt imprimir!
                write!(
                    f,
                    "{}",
                    AstFmt {
                        exp,
                        heap: self.heap
                    }
                )
            } else {
                write!(f, "<nil>")
            }
        } else {
            write!(f, "<invalid-tag>")
        }
    }
}

use std::fmt;
impl fmt::Debug for LispExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            AstFmt {
                exp: self,
                heap: &Heap::new()
            }
        )
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

pub fn is_deep_equal(a: Value, b: Value, heap: &Heap) -> bool {
    use LispExp::*;

    if a == b {
        return true;
    }

    if a.is_gc_ref() && b.is_gc_ref() {
        if let (Some(ea), Some(eb)) = (heap.get(a), heap.get(b)) {
            match (ea, eb) {
                (Str(s1), Str(s2)) => s1 == s2,
                (Symbol(s1), Symbol(s2)) => s1 == s2,
                (Pair(car1, cdr1), Pair(car2, cdr2)) => {
                    is_deep_equal(*car1, *car2, heap) && is_deep_equal(*cdr1, *cdr2, heap)
                }
                (Vector(v1), Vector(v2)) => {
                    if v1.len() != v2.len() {
                        return false;
                    }
                    for (i1, i2) in v1.iter().zip(v2.iter()) {
                        if !is_deep_equal(*i1, *i2, heap) {
                            return false;
                        }
                    }
                    true
                }
                (HashMap(m1), HashMap(m2)) => {
                    if m1.len() != m2.len() {
                        return false;
                    }
                    for (k, v1) in m1.iter() {
                        match m2.get(k) {
                            Some(v2) => {
                                if !is_deep_equal(*v1, *v2, heap) {
                                    return false;
                                }
                            }
                            None => return false,
                        }
                    }
                    true
                }
                _ => false,
            }
        } else {
            false
        }
    } else {
        false
    }
}
