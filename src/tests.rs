#[cfg(test)]
mod tests {
    use crate::env::standard_env;
    use crate::expr::LispExp;
    use crate::heap::Heap;
    use crate::{ExecMode, run_source};

    // --- FUNÇÃO AJUDANTE DE TESTES ---
    // Esta função prepara o ambiente, carrega as macros e roda a string de teste.
    // DICA: Substitua `run_source` pelo nome real da função que você usa no seu main.rs
    // para compilar e rodar strings de código (a mesma usada no REPL).
    fn run_haki(code: &str) -> LispExp {
        let mut heap = Heap::new();
        let mut env = standard_env(&mut heap);

        // Carrega as macros essenciais (certifique-se de que o caminho está correto no seu projeto)
        // Se a função `run_source` retornar Result, o `let _` ignora o retorno com segurança.
        let _ = run_source(
            "(load \"std/macros.lsp\")",
            &mut env,
            ExecMode::Normal,
            &mut heap,
            false,
            false,
        );
        let _ = run_source(
            "(load \"std/lib.lsp\")",
            &mut env,
            ExecMode::Normal,
            &mut heap,
            false,
            false,
        );

        run_source(code, &mut env, ExecMode::Normal, &mut heap, false, false)
            .unwrap_or_else(|e| panic!("Erro no teste: {}\nCódigo: {}", e, code))
    }

    // --- 1. TESTES DE MATEMÁTICA E LÓGICA ---
    #[test]
    fn test_matematica() {
        assert_eq!(run_haki("(+ 10 20 5)"), LispExp::Number(35.0));
        assert_eq!(run_haki("(- 100 20 5)"), LispExp::Number(75.0));
        assert_eq!(run_haki("(* 2 3 4)"), LispExp::Number(24.0));
        assert_eq!(run_haki("(/ 100 2 2)"), LispExp::Number(25.0));
        assert_eq!(run_haki("(modulo 10 3)"), LispExp::Number(1.0));
    }

    #[test]
    fn test_comparacoes_e_logica() {
        assert_eq!(run_haki("(> 10 5)"), LispExp::Bool(true));
        assert_eq!(run_haki("(< 10 5)"), LispExp::Bool(false));
        assert_eq!(run_haki("(= 42 42)"), LispExp::Bool(true));
        assert_eq!(run_haki("(not #f)"), LispExp::Bool(true));
    }

    // --- 2. TESTES DE ESTRUTURAS NATIVAS (A Sintaxe Grimoire) ---
    #[test]
    fn test_vetores() {
        assert_eq!(run_haki("(vector-ref [10 20 30] 1)"), LispExp::Number(20.0));

        let mutacao = "
        (begin
            (define v [1 2 3])
            (vector-set! v 0 99)
            (vector-ref v 0))";
        assert_eq!(run_haki(mutacao), LispExp::Number(99.0));
    }

    #[test]
    fn test_hashmaps() {
        let codigo_hash = "
        (begin
            (define obj { \"hp\" 100 \"mp\" 50 })
            (hash-ref obj \"hp\"))";
        assert_eq!(run_haki(codigo_hash), LispExp::Number(100.0));

        let hash_miss_key = "(hash-ref { \"a\" 1 } \"b\")";
        assert_eq!(run_haki(hash_miss_key), LispExp::Nil);
    }

    // --- 3. TESTES DO MOTOR LISP CORE ---
    #[test]
    fn test_variaveis_e_funcoes() {
        let codigo_funcao = "
        (begin
            (define (dobro x) (* x 2))
            (dobro 15))";
        assert_eq!(run_haki(codigo_funcao), LispExp::Number(30.0));
    }

    #[test]
    fn test_listas_e_pairs() {
        assert_eq!(run_haki("(car '(1 2 3))"), LispExp::Number(1.0));
        assert_eq!(run_haki("(car (cdr '(1 2 3)))"), LispExp::Number(2.0));

        // Verifica as correções recém-implementadas do List vs Pair!
        assert_eq!(run_haki("(list? '(1 2 3))"), LispExp::Bool(true));
        assert_eq!(run_haki("(list? (cons 1  2))"), LispExp::Bool(false));
        assert_eq!(run_haki("(pair? (cons 1 2))"), LispExp::Bool(true));
    }

    // --- 4. TESTES DE MACROS ---
    #[test]
    fn test_macros_let_e_cond() {
        let codigo_let = "(let ((x 10) (y 20)) (+ x y))";
        assert_eq!(run_haki(codigo_let), LispExp::Number(30.0));

        let codigo_cond = "
        (cond
            ((> 5 10) \"maior\")
            ((< 5 10) \"menor\")
            (else     \"igual\"))";
        assert_eq!(run_haki(codigo_cond), LispExp::Str("menor".to_string()));
    }

    #[test]
    fn test_macro_pipe() {
        let codigo_pipe = "
        (|> [1 2 3]
            (vector->list)
            (length))";
        assert_eq!(run_haki(codigo_pipe), LispExp::Number(3.0));
    }
}
