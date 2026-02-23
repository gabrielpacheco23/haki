use crate::env::Env;
use crate::expr::{LispExp, lisp_fmt};
use crate::heap::Heap;
use crate::value::Value;
use crate::vm::{Chunk, OpCode};
use dynasmrt::{DynasmApi, DynasmLabelApi, dynasm};
use std::mem;

extern "C" fn fmod_jit(a: f64, b: f64) -> f64 {
    a % b
}

extern "C" fn jit_display(val: u64, heap_ptr: *const Heap) -> u64 {
    use std::io::Write;
    let v: Value = unsafe { mem::transmute(val) };
    let heap = unsafe { &*heap_ptr };

    if v.is_gc_ref() {
        if let Some(LispExp::Str(s)) = heap.get(v) {
            print!("{}", s);
            std::io::stdout().flush().unwrap(); // Força a exibição imediata no terminal
            return unsafe { mem::transmute(Value::void()) };
        }
    }

    print!("{}", lisp_fmt(v, heap));
    std::io::stdout().flush().unwrap();
    unsafe { mem::transmute(Value::void()) }
}

extern "C" fn jit_newline() -> u64 {
    println!();
    unsafe { mem::transmute(Value::void()) }
}

extern "C" fn jit_car(pair_val: u64, heap_ptr: *const Heap) -> u64 {
    let val: Value = unsafe { mem::transmute(pair_val) };
    let heap = unsafe { &*heap_ptr };

    if val.is_gc_ref() {
        if let Some(LispExp::Pair(car, _)) = heap.get(val) {
            return unsafe { mem::transmute(*car) };
        }
    }
    // bailout
    0xBADBADBADBADBADB
}

extern "C" fn jit_cdr(pair_val: u64, heap_ptr: *const Heap) -> u64 {
    let val: Value = unsafe { mem::transmute(pair_val) };
    let heap = unsafe { &*heap_ptr };

    if val.is_gc_ref() {
        if let Some(LispExp::Pair(_, cdr)) = heap.get(val) {
            return unsafe { mem::transmute(*cdr) };
        }
    }
    0xBADBADBADBADBADB
}

extern "C" fn jit_displayln(val: u64, heap_ptr: *const Heap) -> u64 {
    let v: Value = unsafe { mem::transmute(val) };
    let heap = unsafe { &*heap_ptr };

    // Imprime na tela a partir do código de máquina!
    println!("{}", lisp_fmt(v, heap));
    unsafe { mem::transmute(Value::void()) }
}

extern "C" fn jit_string_eq(val_a: u64, val_b: u64, heap_ptr: *const Heap) -> u64 {
    let a: Value = unsafe { mem::transmute(val_a) };
    let b: Value = unsafe { mem::transmute(val_b) };
    let heap = unsafe { &*heap_ptr };

    if a.is_gc_ref() && b.is_gc_ref() {
        if let (Some(LispExp::Str(s1)), Some(LispExp::Str(s2))) = (heap.get(a), heap.get(b)) {
            let result = if s1 == s2 { true } else { false };
            return unsafe { mem::transmute(Value::boolean(result)) };
        }
    }
    0xBADBADBADBADBADB // Bailout se não forem strings
}

extern "C" fn jit_sin(val: u64) -> u64 {
    let v: Value = unsafe { mem::transmute(val) };
    if !v.is_number() {
        return 0xBADBADBADBADBADB;
    }
    unsafe { mem::transmute(Value::number(v.as_number().sin())) }
}
extern "C" fn jit_cos(val: u64) -> u64 {
    let v: Value = unsafe { mem::transmute(val) };
    if !v.is_number() {
        return 0xBADBADBADBADBADB;
    }
    unsafe { mem::transmute(Value::number(v.as_number().cos())) }
}

pub enum JitResult {
    Success(Value),
    Bailout(&'static str),
}

pub struct CompilerJIT {
    ops: dynasmrt::x64::Assembler,
}

impl CompilerJIT {
    pub fn new() -> Self {
        Self {
            ops: dynasmrt::x64::Assembler::new().unwrap(),
        }
    }

    pub fn compile_with_args(
        &mut self,
        chunk: &Chunk,
        args: &[Value],
        heap: &Heap,
        env: &Env,
        self_val: Value,
    ) {
        let mut labels = vec![];
        for _ in 0..chunk.code.len() {
            labels.push(self.ops.new_dynamic_label());
        }

        let bailout_label = self.ops.new_dynamic_label();

        let recursive_entry = self.ops.new_dynamic_label();

        // entry point do Rust
        // PROLOGUE
        dynasm!(self.ops
            ; push rbp
            ; mov rbp, rsp
            ; sub rsp, 256
        );

        for (i, arg) in args.iter().enumerate() {
            let raw_bits: u64 = unsafe { mem::transmute(*arg) };
            let offset = (i as i32 * 8) + 8;
            dynasm!(self.ops
                ; mov rax, QWORD raw_bits as _
                ; mov [rbp - offset], rax
            );
        }
        dynasm!(self.ops ; jmp =>labels[0]); // inicia codigo Lisp

        // entry point recursivo (jit chamando jit)
        dynasm!(self.ops
            ; =>recursive_entry
            ; push rbp
            ; mov rbp, rsp
            ; sub rsp, 256
            // convenção JIT: args da recursao chegam nos registradores r10 e r11
            ; mov [rbp - 8], r10
            ; mov [rbp - 16], r11
            ; jmp =>labels[0]  // inicia codigo Lisp no novo frame
        );

        let bailout_code: u64 = 0xBADBADBADBADBADB;

        let self_bits: u64 = unsafe { mem::transmute(self_val) };

        let false_bits: u64 = unsafe { mem::transmute(Value::boolean(false)) };
        let true_bits: u64 = unsafe { mem::transmute(Value::boolean(true)) };

        let fmod_ptr = fmod_jit as *const () as u64;

        //
        let heap_addr = heap as *const _ as u64;

        let car_ptr = jit_car as *const () as u64;
        let cdr_ptr = jit_cdr as *const () as u64;
        let display_ptr = jit_display as *const () as u64;
        let newline_ptr = jit_newline as *const () as u64;
        let string_eq_ptr = jit_string_eq as *const () as u64;
        let sin_ptr = jit_sin as *const () as u64;
        let cos_ptr = jit_cos as *const () as u64;

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
                        // ; pop rax                  // tira do topo da stack
                        ; mov rax, [rsp]
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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label


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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

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

                        // type guard
                        ; ucomisd xmm0, xmm0
                        ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1
                        ; jp =>bailout_label

                        ; ucomisd xmm0, xmm1
                        ; mov rax, QWORD false_bits as _
                        ; mov rcx, QWORD true_bits as _
                        ; cmove rax, rcx         // Se forem IGUAIS (Equal), RAX recebe True!
                        ; push rax
                    );
                }

                OpCode::Mod => {
                    dynasm!(self.ops
                        ; pop rdi
                        ; pop rsi
                        ; movq xmm1, rdi
                        ; movq xmm0, rsi

                        // Type Guards
                        ; ucomisd xmm0, xmm0 ; jp =>bailout_label
                        ; ucomisd xmm1, xmm1 ; jp =>bailout_label

                        // CHAMADA FFI PARA O RUST (C-ABI)
                        ; mov r15, QWORD fmod_ptr as _

                        ; mov r14, rsp
                        ; and rsp, -16
                        ; call r15 // chama a função 'fmod_jit' do Rust
                        ; mov rsp, r14

                        // O Rust retorna o float obrigatoriamente no xmm0
                        ; movq rax, xmm0
                        ; push rax
                    );
                }

                OpCode::GetGlobal(name) => {
                    if let Some(val) = env.borrow().get(name) {
                        let raw_bits: u64 = unsafe { mem::transmute(val) };
                        dynasm!(self.ops
                            ; mov rax, QWORD raw_bits as _
                            ; push rax
                        );
                    } else {
                        dynasm!(self.ops ; jmp =>bailout_label);
                    }
                }

                OpCode::TailCall(arg_count) => {
                    // transforma recursao num loop "while" do hardware
                    // desempilha os args novos e substitui vars locais
                    for i in (0..*arg_count).rev() {
                        let offset = (i as i32 * 8) + 8;
                        dynasm!(self.ops
                            ; pop rax
                            ; mov [rbp - offset], rax
                        );
                    }

                    // tira função fantasma da stack
                    dynasm!(self.ops ; pop rax);

                    // target guard
                    dynasm!(self.ops
                        ; mov rcx, QWORD self_bits as _
                        ; cmp rax, rcx
                        ; jne =>bailout_label
                    );

                    // pula fisicamente para inicio da função
                    // (loop sem estourar memoria)
                    let start = labels[0];
                    dynasm!(self.ops ; jmp =>start);
                }

                OpCode::Call(arg_count) => {
                    if *arg_count > 2 {
                        for _ in 0..*arg_count {
                            dynasm!(self.ops ;pop rax);
                        }
                        dynasm!(self.ops
                            ; pop rax
                            ; jmp =>bailout_label);
                    } else {
                        // MVP: suporta até 2 args de recursao pura
                        if *arg_count == 1 {
                            dynasm!(self.ops ; pop r10);
                        } else if *arg_count == 2 {
                            dynasm!(self.ops ; pop r11 ; pop r10);
                        }

                        // tira objeto função da stack
                        dynasm!(self.ops ; pop rax);
                        dynasm!(self.ops
                            ; mov rcx, QWORD self_bits as _
                            ; cmp rax, rcx
                            ; jne =>bailout_label

                            ; call =>recursive_entry

                            // AQUI ESTÁ A MAGIA! PROPAGAÇÃO DE BAILOUT:
                            // Se a recursão interna ejetou, repassa a ejeção pra VM!
                            ; mov rcx, QWORD bailout_code as _
                            ; cmp rax, rcx
                            ; je =>bailout_label
                            // -------------------------------------------------

                            ; push rax
                        );
                    }
                    // if *arg_count > 2 {
                    //     for _ in 0..*arg_count {
                    //         dynasm!(self.ops ;pop rax);
                    //     }
                    //     dynasm!(self.ops
                    //         ; pop rax
                    //         ; jmp =>bailout_label);
                    // } else {
                    //     // MVP: suporta até 2 args de recursao pura
                    //     if *arg_count == 1 {
                    //         dynasm!(self.ops ; pop r10);
                    //     } else if *arg_count == 2 {
                    //         dynasm!(self.ops ; pop r11 ; pop r10);
                    //     }

                    //     // tira objeto função da stack (assumimos recursao local no MVP)
                    //     dynasm!(self.ops ; pop rax);
                    //     dynasm!(self.ops
                    //         ; mov rcx, QWORD self_bits as _
                    //         ; cmp rax, rcx
                    //         ; jne =>bailout_label

                    //         ; call =>recursive_entry
                    //         ; push rax
                    //     );
                    // }
                }

                OpCode::Pop => {
                    dynasm!(self.ops ; add rsp, 8);
                }

                OpCode::Car => {
                    dynasm!(self.ops
                        ; pop rdi
                        ; mov rsi, QWORD heap_addr as _
                        ; mov r15, QWORD car_ptr as _

                        // === O SEGREDO DO ALINHAMENTO ===
                        ; mov r14, rsp      // Salva o topo da pilha Lisp num lugar seguro
                        ; and rsp, -16      // Hack binário: Força o RSP a ser múltiplo de 16!
                        ; call r15          // Chama o Rust com segurança absoluta
                        ; mov rsp, r14      // Restaura a pilha Lisp como se nada tivesse acontecido
                        // ================================

                        ; mov rcx, QWORD bailout_code as _
                        ; cmp rax, rcx
                        ; je =>bailout_label
                        ; push rax
                    );
                }
                OpCode::Cdr => {
                    dynasm!(self.ops
                        ; pop rdi
                        ; mov rsi, QWORD heap_addr as _
                        ; mov r15, QWORD cdr_ptr as _

                        // === O SEGREDO DO ALINHAMENTO ===
                        ; mov r14, rsp
                        ; and rsp, -16
                        ; call r15
                        ; mov rsp, r14
                        // ================================

                        ; mov rcx, QWORD bailout_code as _
                        ; cmp rax, rcx
                        ; je =>bailout_label
                        ; push rax
                    );
                }
                OpCode::Display => {
                    dynasm!(self.ops
                        ; pop rdi
                        ; mov rsi, QWORD heap_addr as _
                        ; mov r15, QWORD display_ptr as _

                        // C-ABI
                        ; mov r14, rsp
                        ; and rsp, -16
                        ; call r15
                        ; mov rsp, r14

                        ; push rax // Retorna Void
                    );
                }
                OpCode::Newline => {
                    dynasm!(self.ops
                        ; mov r15, QWORD newline_ptr as _
                        // C-ABI
                        ; mov r14, rsp
                        ; and rsp, -16
                        ; call r15
                        ; mov rsp, r14
                    );
                }

                OpCode::StringEq => {
                    dynasm!(self.ops
                        ; pop rsi                        // tira o argumento B (topo da stack)
                        ; pop rdi                        // tira o argumento A
                        ; mov rdx, QWORD heap_addr as _  // o ponteiro do Heap
                        ; mov r15, QWORD string_eq_ptr as _

                        // C-ABI (alinhamento da stack)
                        ; mov r14, rsp
                        ; and rsp, -16
                        ; call r15
                        ; mov rsp, r14

                        // target guard para strings
                        ; mov rcx, QWORD bailout_code as _
                        ; cmp rax, rcx
                        ; je =>bailout_label

                        // se não, empurra #t ou #f de volta
                        ; push rax
                    );
                }

                OpCode::Sqrt => {
                    dynasm!(self.ops
                        ; pop rax
                        ; movq xmm0, rax

                        // type guard
                        // ; ucomisd xmm0, xmm0
                        // ; jp =>bailout_label

                        ; sqrtsd xmm0, xmm0
                        ; movq rax, xmm0
                        ; push rax
                    );
                }

                OpCode::Sin | OpCode::Cos => {
                    let ptr = if matches!(instruction, OpCode::Sin) {
                        sin_ptr
                    } else {
                        cos_ptr
                    };

                    dynasm!(self.ops
                        ; pop rdi
                        ; mov r15, QWORD ptr as _

                        ; mov r14, rsp
                        ; and rsp, -16
                        ; call r15
                        ; mov rsp, r14

                        ; mov rcx, QWORD bailout_code as _
                        ; cmp rax, rcx
                        ; je =>bailout_label

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
        // rota de fuga (bailout)
        dynasm!(self.ops
            ; =>bailout_label
            ; mov rcx, QWORD bailout_code as _
            ; mov rax, rcx
            ; mov rsp, rbp
            ; pop rbp
            ; ret
        );
    }

    pub fn execute(mut self) -> Result<JitResult, String> {
        let buffer = self.ops.finalize().unwrap();

        let jitted_fn: extern "C" fn() -> u64 =
            unsafe { mem::transmute(buffer.ptr(dynasmrt::AssemblyOffset(0))) };

        let raw_result = jitted_fn();

        if raw_result == 0xBADBADBADBADBADB {
            return Ok(JitResult::Bailout("Type Guard Failed: Expected number"));
        }

        let lisp_value: Value = unsafe { mem::transmute(raw_result) };
        Ok(JitResult::Success(lisp_value))
    }
}

pub fn can_jit(chunk: &Chunk) -> bool {
    for constant in &chunk.constants {
        if !constant.is_number()
            && !constant.is_boolean()
            && !constant.is_void()
            && !constant.is_gc_ref()
        {
            return false;
        }
    }
    for op in &chunk.code {
        match op {
            OpCode::Constant(_)
            | OpCode::GetLocal(_)
            | OpCode::SetLocal(_)
            | OpCode::Add
            | OpCode::Sub
            | OpCode::Mul
            | OpCode::Div
            | OpCode::Mod
            | OpCode::Lt(_)
            | OpCode::Eq(_)
            | OpCode::Gt(_)
            | OpCode::Le(_)
            | OpCode::Ge(_)
            | OpCode::Jump(_)
            | OpCode::JumpIfFalse(_)
            | OpCode::Return
            | OpCode::GetGlobal(_)
            | OpCode::TailCall(_)
            | OpCode::Call(_)
            | OpCode::Car
            | OpCode::Cdr
            | OpCode::Display
            | OpCode::Newline
            | OpCode::StringEq
            | OpCode::Sqrt
            | OpCode::Sin
            | OpCode::Cos
            | OpCode::Pop => continue,
            _ => return false,
        }
    }
    true
}
