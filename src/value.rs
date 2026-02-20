// Quiet NaN
const QNAN: u64 = 0x7FFC000000000000;

// Bit de Sinal (diferenciar primitivos de ponteiros do heap)
const SIGN_BIT: u64 = 0x8000000000000000;

const TAG_NIL: u64 = 1; // 001
const TAG_FALSE: u64 = 2; // 010
const TAG_TRUE: u64 = 3; // 011
const TAG_VOID: u64 = 4; // 100

// mascara para GcRef (ponteiros do heap)
// se tiver QNAN e sign bit -> é um indice do heap
const PTR_MASK: u64 = QNAN | SIGN_BIT;

// value (8 bytes)
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Value(pub u64);

impl Value {
    #[inline]
    pub fn number(n: f64) -> Self {
        Value(n.to_bits())
    }

    #[inline]
    pub fn nil() -> Self {
        Value(QNAN | TAG_NIL)
    }

    #[inline]
    pub fn boolean(b: bool) -> Self {
        if b {
            Value(QNAN | TAG_TRUE)
        } else {
            Value(QNAN | TAG_FALSE)
        }
    }

    #[inline]
    pub fn void() -> Self {
        Value(QNAN | TAG_VOID)
    }

    #[inline]
    pub fn gc_ref(index: usize) -> Self {
        Value(PTR_MASK | (index as u64))
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        (self.0 & QNAN) != QNAN
    }

    #[inline]
    pub fn as_number(&self) -> f64 {
        f64::from_bits(self.0)
    }

    #[inline]
    pub fn is_nil(&self) -> bool {
        self.0 == (QNAN | TAG_NIL)
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        self.0 == (QNAN | TAG_VOID)
    }

    #[inline]
    pub fn is_boolean(&self) -> bool {
        self.0 == (QNAN | TAG_TRUE) || self.0 == (QNAN | TAG_FALSE)
    }

    #[inline]
    pub fn as_boolean(&self) -> bool {
        self.0 == (QNAN | TAG_TRUE)
    }

    #[inline]
    pub fn is_gc_ref(&self) -> bool {
        (self.0 & PTR_MASK) == PTR_MASK
    }

    #[inline]
    pub fn as_gc_ref(&self) -> usize {
        (self.0 & !PTR_MASK) as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nan_tagging() {
        let n = Value::number(3.14);
        let b = Value::boolean(true);
        let ptr = Value::gc_ref(1024);

        assert!(n.is_number());
        assert!(!n.is_boolean());

        assert!(b.is_boolean());
        assert!(b.as_boolean() == true);

        assert!(ptr.is_gc_ref());
        assert_eq!(ptr.as_gc_ref(), 1024);
        assert!(!ptr.is_number());
    }
}
