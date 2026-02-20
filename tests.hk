;; ==========================================
;; FRAMEWORK DE TESTES - HAKI
;; ==========================================

(define *testes-rodados* 0)
(define *testes-passaram* 0)
(define *testes-falharam* 0)

(defmacro (test nome esperado recebido)
  `(let ((exp ,esperado)
         (rec ,recebido))
     (set! *testes-rodados* (+ *testes-rodados* 1))
     (if (equal? exp rec)
         (begin
           (set! *testes-passaram* (+ *testes-passaram* 1))
           (display "[PASS] ")
           (displayln ,nome))
         (begin
           (set! *testes-falharam* (+ *testes-falharam* 1))
           (display "[FAIL] ")
           (displayln ,nome)
           (display "  --> Esperado: ") (displayln exp)
           (display "  --> Recebido: ") (displayln rec)))))

(define (relatorio-testes)
  (displayln "\n==================================")
  (displayln "      RESULTADO DOS TESTES        ")
  (displayln "==================================")
  (display "Total de testes: ") (displayln *testes-rodados*)
  (display "Sucessos:        ") (displayln *testes-passaram*)
  (display "Falhas:          ") (displayln *testes-falharam*)
  (displayln "=================================="))


;; ==========================================
;; SUÍTE DE TESTES UNITÁRIOS
;; ==========================================

(displayln "Iniciando a bateria de testes do Haki Lisp...\n")

(test "Soma Múltipla" 10 (+ 1 2 3 4))
(test "Subtração Unária e Normal" 15 (- 10 (- 5)))
(test "Precedência Matemática" 14 (+ 2 (* 3 4)))
(test "Igualdade Numérica" #t (= 10 10))

(test "car de uma lista" 1 (car '(1 2 3)))
(test "cdr de uma lista" '(2 3) (cdr '(1 2 3)))
(test "length da lista" 4 (length '(a b c d)))
(test "map com lambda" '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))
(test "filter" '(2 4) (filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5)))
(test "fold (soma de lista)" 15 (fold + 0 '(1 2 3 4 5)))

(define (cria-multiplicador fator)
  (lambda (x) (* x fator)))
(define dobro (cria-multiplicador 2))
(test "Closure capturando variável (Lexical Scope)" 20 (dobro 10))

(test "Macro let" 100 (let ((x 10) (y 10)) (* x y)))
(test "Macro let* (Aninhamento)" 20 (let* ((x 10) (y (+ x 10))) y))
(test "Macro cond" "ok" (cond (#f "nao") (#t "ok")))
(test "Macro and" #f (and #t #t #f))
(test "Macro or" #t (or #f #f #t))

(define acao '("comprar" "pocao-de-vida" 50))
(define resultado-match
  (match acao
    (("atacar" alvo) "combate")
    (("comprar" item preco) item)
    (_ "nada")))

(test "Macro match extraindo dados" "pocao-de-vida" resultado-match)

(define (soma-recursiva n acc)
  (if (= n 0)
      acc
      (soma-recursiva (- n 1) (+ acc n))))

(test "Tail Call Optimization (Recursão Profunda)" 50005000 (soma-recursiva 10000 0))

(test "String Interning (eq? compara ponteiros)" #t (eq? "Haki" (string-append "Ha" "ki")))

(define meu-vec (vector 10 20 30))
(test "Acesso a Vector Nativo" 20 (vector-ref meu-vec 1))

(define meu-hash (hash "nome" "Haki" "tipo" "Lisp"))
(test "Acesso a HashMap Nativo" "Lisp" (hash-ref meu-hash "tipo"))

(relatorio-testes)
