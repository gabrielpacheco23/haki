(define print (lambda (x) x)) 

; (define cadr (lambda (x) (car (cdr x))))

(define (not x)
  (if x #f #t))

(define append (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (modulo a b))))

(define (for-each f ls)
  (if (null? ls)
      '()
      (begin
        (f (car ls))
        (for-each f (cdr ls)))))

(define (square x) (* x x))

(define (even? x)
 (= (modulo x 2) 0))

(define (odd? x)
  (not (= (modulo x 2) 0)))

(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (map f L)
            (if (null? L)
                (list)
                (cons (f (car L)) (map f (cdr L)))))

(define (filter f lst)
  (cond ((null? lst) '())
        ((f (car lst)) (cons (car lst) (filter f (cdr lst))))
        (else (filter f (cdr lst)))))

(define (fold f acc lst)
  (if (null? lst)
      acc
      (fold f (f acc (car lst)) (cdr lst))))

(define (reverse lst)
  (define (rev-acc l acc)
    (if (null? l)
        acc
        (rev-acc (cdr l) (cons (car l) acc))))
  (rev-acc lst '()))

(define (length lst)
  (define (len-acc l count)
    (if (null? l)
        count
        (len-acc (cdr l) (+ count 1))))
  (len-acc lst 0))

(define (list-ref list index)
  (if (= index 0)
      (car list)
      (list-ref (cdr list) (- index 1))))


;; ======= web ===============
(define (http-get url)
  (shell (string-append "curl -s " url)))
