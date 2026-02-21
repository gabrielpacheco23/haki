use std::{cell::RefCell, rc::Rc};

use crate::value::Value;

#[derive(Debug, Clone)]
pub enum UpvalueState {
    // aberto: guarda indice da variavel na stack da vm
    Open(usize),
    // fechado: a função pai desapareceu, o valor vive aqui
    Closed(Value),
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub state: Rc<RefCell<UpvalueState>>,
}

impl Upvalue {
    pub fn new_open(loc: usize) -> Self {
        Upvalue {
            state: Rc::new(RefCell::new(UpvalueState::Open(loc))),
        }
    }

    pub fn get_value(&self, stack: &[Value]) -> Value {
        match &*self.state.borrow() {
            UpvalueState::Open(idx) => stack[*idx],
            UpvalueState::Closed(value) => *value,
        }
    }

    pub fn set_value(&self, new_val: Value, stack: &mut [Value]) {
        match &mut *self.state.borrow_mut() {
            UpvalueState::Open(idx) => stack[*idx] = new_val,
            UpvalueState::Closed(value) => *value = new_val,
        }
    }

    pub fn close(&self, value: Value) {
        *self.state.borrow_mut() = UpvalueState::Closed(value);
    }

    pub fn is_open(&self) -> bool {
        matches!(*self.state.borrow(), UpvalueState::Open(_))
    }
}

impl PartialEq for Upvalue {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.state, &other.state)
    }
}
