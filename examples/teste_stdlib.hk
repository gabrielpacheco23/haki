(displayln "=== BRINCANDO COM A NOVA STDLIB ===")

(displayln "\n1. Testando as Funcionais (range, reverse, zip):")
(define numeros (range 1 6))
(define letras '("a" "b" "c" "d" "e"))
(display "Range 1 a 5: ") (displayln numeros)
(display "Reverso:     ") (displayln (reverse numeros))
(display "Zippado:     ") (displayln (zip numeros letras))

(displayln "\n2. Testando (any?) e (all?):")
(display "Tem par?     ") (displayln (any? (lambda (x) (= (modulo x 2) 0)) numeros))
(display "Tudo é par?  ") (displayln (all? (lambda (x) (= (modulo x 2) 0)) numeros))

(displayln "\n3. Testando Loop (while):")
(let ((x 0))
  (while (< x 3)
    (display "Enquanto x for menor que 3: ") (displayln x)
    (set! x (+ x 1))))

(displayln "\n4. Testando Loop (dotimes):")
(dotimes (i 3)
  (display "Iteracao rapida: ") (displayln i))

(displayln "\n5. Testando a Macro de Performance (time):")
(define (teste-pesado)
  (dotimes (i 100000)
    (+ i i))) ;; Executa 100 mil vezes usando TCO puro

(display "Rodando o dotimes de 100.000... ")
(time (teste-pesado))
