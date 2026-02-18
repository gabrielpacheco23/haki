(load "math.scm") ;; Importa o outro arquivo!

(define base 2)
(define expoente 10)

(let ((resultado (fast-expt base expoente)))
  (display "O calculo de 2 elevado a 10 é: ") ;; Se tiver função de display
  (displayln resultado))
