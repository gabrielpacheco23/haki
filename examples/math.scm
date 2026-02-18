(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? x) (= (modulo x 2) 0))

(define e 2.718281828459045)

(define euler (expt e 1))
