use crate::value::Value;
use crate::vm::{Chunk, OpCode};
use dynasmrt::{DynasmApi, DynasmLabelApi, dynasm};
use std::mem;

pub struct CompilerJIT {
    ops: dynasmrt::x64::Assembler,
}

impl CompilerJIT {
    pub fn new() -> Self {
        Self {
            ops: dynasmrt::x64::Assembler::new().unwrap(),
        }
    }

    pub fn compile(&mut self, chunk: &Chunk) {
        for instruction in &chunk.code {
            match instruction {
                OpCode::Constant(idx) => {
                    let val = chunk.constants[*idx];
                    let raw_bits: u64 = unsafe { mem::transmute(val) };
                    dynasm!(self.ops
                         ; mov rax, QWORD raw_bits as _
                         ; push rax
                    );
                }

                OpCode::Add => {
                    dynasm!(self.ops
                        ; pop rcx         // retira b e poe no rcx
                        ; pop rax         // retira a e poe no rax

                        // movemos os valores para registradores float
                        ; movq xmm1, rcx
                        ; movq xmm0, rax

                        // addsd = add scalar double
                        ; addsd xmm0, xmm1

                        // move o resultado para rax
                        ; movq rax, xmm0
                        ; push rax
                    );
                }
                OpCode::Return => {
                    dynasm!(self.ops
                        ; pop rax
                        ; ret
                    );
                }
                _ => {}
            }
        }
    }

    pub fn execute(mut self) -> Result<Value, String> {
        let buffer = self.ops.finalize().unwrap();

        let jitted_fn: extern "C" fn() -> u64 =
            unsafe { mem::transmute(buffer.ptr(dynasmrt::AssemblyOffset(0))) };

        let raw_result = jitted_fn();
        let lisp_value: Value = unsafe { mem::transmute(raw_result) };

        Ok(lisp_value)
    }
}
