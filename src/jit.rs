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
        let mut labels = vec![];
        for _ in 0..chunk.code.len() {
            labels.push(self.ops.new_dynamic_label());
        }

        dynasm!(self.ops
            ; push rbp         // salva rbp
            ; mov rbp, rsp     // define topo atual como novo rbp
            ; sub rsp, 256     // reserva 256 bytes (32 variaveis locais Lisp)
        );

        let false_bits: u64 = unsafe { mem::transmute(Value::boolean(false)) };
        let true_bits: u64 = unsafe { mem::transmute(Value::boolean(true)) };

        for (ip, instruction) in chunk.code.iter().enumerate() {
            // ancoramos a label da instrução atual na memoria
            dynasm!(self.ops ; => labels[ip]);

            match instruction {
                OpCode::Constant(idx) => {
                    let val = chunk.constants[*idx];
                    let raw_bits: u64 = unsafe { mem::transmute(val) };
                    dynasm!(self.ops
                         ; mov rax, QWORD raw_bits as _
                         ; push rax
                    );
                }

                OpCode::SetLocal(idx) => {
                    // local 0 = 8 bytes, local 1 = 16 bytes...
                    let offset = (*idx as i32 * 8) + 8;
                    dynasm!(self.ops
                        ; pop rax                  // tira do topo da stack
                        ; mov [rbp - offset], rax  // salva no slot da memoria local
                    );
                }

                OpCode::GetLocal(idx) => {
                    let offset = (*idx as i32 * 8) + 8;
                    dynasm!(self.ops
                        ; mov rax, [rbp - offset]  // busca do slot
                        ; push rax                 // joga pro topo da stack
                    );
                }

                OpCode::JumpIfFalse(target) => {
                    let target_label = labels[*target];
                    dynasm!(self.ops
                        ; pop rax
                        ; mov rcx, QWORD false_bits as _
                        ; cmp rax, rcx       // condição é false?
                        ; je =>target_label
                    );
                }

                OpCode::Jump(target) => {
                    let target_label = labels[*target];
                    dynasm!(self.ops
                        ; jmp =>target_label
                    );
                }

                OpCode::Add => {
                    dynasm!(self.ops
                        ; pop rcx
                        ; pop rax
                        ; movq xmm1, rcx
                        ; movq xmm0, rax
                        ; addsd xmm0, xmm1
                        ; movq rax, xmm0
                        ; push rax
                    );
                }

                OpCode::Sub => {
                    dynasm!(self.ops
                        ; pop rcx
                        ; pop rax
                        ; movq xmm1, rcx
                        ; movq xmm0, rax
                        ; subsd xmm0, xmm1
                        ; movq rax, xmm0
                        ; push rax
                    );
                }
                OpCode::Mul => {
                    dynasm!(self.ops
                        ; pop rcx
                        ; pop rax
                        ; movq xmm1, rcx
                        ; movq xmm0, rax
                        ; mulsd xmm0, xmm1
                        ; movq rax, xmm0
                        ; push rax
                    );
                }
                OpCode::Div => {
                    dynasm!(self.ops
                        ; pop rcx
                        ; pop rax
                        ; movq xmm1, rcx
                        ; movq xmm0, rax
                        ; divsd xmm0, xmm1
                        ; movq rax, xmm0
                        ; push rax
                    );
                }

                OpCode::Lt(_) => {
                    // Menor que (<)
                    dynasm!(self.ops
                        ; pop rcx                // b
                        ; pop rax                // a
                        ; movq xmm1, rcx
                        ; movq xmm0, rax
                        ; ucomisd xmm0, xmm1     // Compara (a) com (b) na unidade de ponto flutuante
                        ; mov rax, QWORD false_bits as _
                        ; mov rcx, QWORD true_bits as _
                        ; cmovb rax, rcx         // Se (a) for MENOR (Below) que (b), RAX recebe True!
                        ; push rax               // Joga o resultado (True ou False) na pilha
                    );
                }
                OpCode::Gt(_) => {
                    dynasm!(self.ops
                        ; pop rcx
                        ; pop rax
                        ; movq xmm1, rcx
                        ; movq xmm0, rax
                        ; ucomisd xmm0, xmm1
                        ; mov rax, QWORD false_bits as _
                        ; mov rcx, QWORD true_bits as _
                        ; cmova rax, rcx
                        ; push rax
                    );
                }

                OpCode::Le(_) => {
                    // Menor ou Igual (<=)
                    dynasm!(self.ops
                        ; pop rcx ; pop rax
                        ; movq xmm1, rcx ; movq xmm0, rax
                        ; ucomisd xmm0, xmm1
                        ; mov rax, QWORD false_bits as _
                        ; mov rcx, QWORD true_bits as _
                        ; cmovbe rax, rcx        // Condição: Below or Equal
                        ; push rax
                    );
                }
                OpCode::Ge(_) => {
                    // Maior ou Igual (>=)
                    dynasm!(self.ops
                        ; pop rcx ; pop rax
                        ; movq xmm1, rcx ; movq xmm0, rax
                        ; ucomisd xmm0, xmm1
                        ; mov rax, QWORD false_bits as _
                        ; mov rcx, QWORD true_bits as _
                        ; cmovae rax, rcx        // Condição: Above or Equal
                        ; push rax
                    );
                }

                OpCode::Eq(_) => {
                    // Igualdade (==)
                    dynasm!(self.ops
                        ; pop rcx
                        ; pop rax
                        ; movq xmm1, rcx
                        ; movq xmm0, rax
                        ; ucomisd xmm0, xmm1
                        ; mov rax, QWORD false_bits as _
                        ; mov rcx, QWORD true_bits as _
                        ; cmove rax, rcx         // Se forem IGUAIS (Equal), RAX recebe True!
                        ; push rax
                    );
                }

                OpCode::Return => {
                    dynasm!(self.ops
                        ; pop rax
                        ; mov rsp, rbp
                        ; pop rbp
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

pub fn can_jit(chunk: &Chunk) -> bool {
    for op in &chunk.code {
        match op {
            OpCode::Constant(_)
            | OpCode::GetLocal(_)
            | OpCode::SetLocal(_)
            | OpCode::Add
            | OpCode::Sub
            | OpCode::Mul
            | OpCode::Div
            | OpCode::Lt(_)
            | OpCode::Eq(_)
            | OpCode::Gt(_)
            | OpCode::Le(_)
            | OpCode::Ge(_)
            | OpCode::Jump(_)
            | OpCode::JumpIfFalse(_)
            | OpCode::Return => continue,

            // Se o código tiver qualquer outra coisa (Closures, Strings, HashMaps, Print)...
            _ => return false,
        }
    }
    true
}
