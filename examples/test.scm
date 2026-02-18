(load "math.scm") ;; Imports another file

(define base 2)
(define expoente 10)

(let ((resultado (fast-expt base expoente)))
  (display "O calculo de 2 elevado a 10 é: ")  
  (displayln resultado))
