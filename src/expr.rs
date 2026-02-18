use crate::env::Env;
use crate::vm::Chunk;

use std::collections::HashMap as RustHashMap;

pub type LispNative = Rc<dyn Fn(&[LispExp], &mut Env) -> Result<LispExp, String>>;

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
    List(Vec<LispExp>),
    Native(LispNative),
    Lambda(LispLambda),
    Macro(LispLambda),
    Void,
    Pair(Rc<LispExp>, Rc<LispExp>),
    Nil,
    Vector(Rc<RefCell<Vec<LispExp>>>),
    HashMap(Rc<RefCell<RustHashMap<String, LispExp>>>),
    VmClosure {
        params: Vec<String>,
        chunk: Rc<Chunk>,
        env: Env,
    },
}

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;
impl Display for LispExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LispExp::Symbol(s) => write!(f, "{}", s),
            LispExp::Str(s) => write!(f, "{}", s),
            LispExp::Number(num) => write!(f, "{}", num),
            LispExp::List(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
            LispExp::Native(_) => write!(f, "<procedure>"),
            LispExp::Lambda(_) => write!(f, "<procedure>"),
            LispExp::Macro(_) => write!(f, "<macro>"),
            LispExp::Bool(b) => write!(f, "{}", lisp_bool(b)),
            LispExp::Void => write!(f, "<void>"),
            #[allow(unused)]
            LispExp::VmClosure { params, chunk, env } => write!(f, "<vm:closure>"),
            LispExp::Pair(car, cdr) => {
                write!(f, "({}", car)?;
                let mut current = cdr.clone();

                while let LispExp::Pair(next_car, next_cdr) = &*current {
                    write!(f, " {}", next_car)?;
                    current = next_cdr.clone();
                }

                if let LispExp::Nil = &*current {
                    write!(f, ")")
                } else {
                    write!(f, " . {})", current)
                }
            }
            LispExp::Nil => write!(f, "()"),
            LispExp::Vector(vec) => {
                let strings: Vec<String> = vec.borrow().iter().map(|x| x.to_string()).collect();
                write!(f, "#({})", strings.join(" "))
            }
            LispExp::HashMap(map) => {
                let m = map.borrow();
                let mut entries = vec![];
                for (k, v) in m.iter() {
                    entries.push(format!("(\"{}\" . {})", k, v));
                }
                write!(f, "#hash({})", entries.join(" "))
            }
        }
    }
}

use std::fmt;
impl fmt::Debug for LispExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

fn lisp_bool(b: &bool) -> String {
    if *b {
        return "#t".to_string();
    }
    "#f".to_string()
}

impl PartialEq for LispExp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Comparação simples para tipos básicos
            (LispExp::Symbol(a), LispExp::Symbol(b)) => a == b,
            (LispExp::Str(a), LispExp::Str(b)) => a == b,
            (LispExp::Number(a), LispExp::Number(b)) => a == b,
            (LispExp::Bool(a), LispExp::Bool(b)) => a == b,

            // Comparação recursiva para Listas (o Rust faz isso sozinho para Vecs)
            (LispExp::List(a), LispExp::List(b)) => a == b,
            (LispExp::Vector(a), LispExp::Vector(b)) => **a == **b,
            (LispExp::HashMap(a), LispExp::HashMap(b)) => **a == **b,
            // Compara se os dois apontam para o mesmo local de memória (ponteiro)
            (LispExp::Native(a), LispExp::Native(b)) => Rc::ptr_eq(a, b),
            (LispExp::Lambda(a), LispExp::Lambda(b)) => *a.params == *b.params && a.body == b.body,
            (LispExp::Pair(a1, a2), LispExp::Pair(b1, b2)) => **a1 == **b1 && **a2 == **b2,
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
            (LispExp::Nil, LispExp::Nil) => true,
            _ => false,
        }
    }
}
