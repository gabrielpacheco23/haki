;; ==========================================
;; BIBLIOTECA PADRÃO - HAKI LISP
;; ==========================================

(define print display x) 
(define println displayln x) 

; (define cadr (lambda (x) (car (cdr x))))

; (define (caddr x) (car (cdr (cdr x))))

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

; (define (reverse lst)
;   (define (rev-acc l acc)
;     (if (null? l)
;         acc
;         (rev-acc (cdr l) (cons (car l) acc))))
;   (rev-acc lst '()))

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
; (define (http-get url)
;   (shell (string-append "curl -s " url)))

;; 1. GERADORES E MANIPULADORES
;; Cria uma lista de números de 'start' até 'end' (exclusivo)
(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

;; Inverte uma lista completamente: '(1 2 3) -> '(3 2 1)
(define (reverse lst)
  (fold (lambda (acc x) (cons x acc)) '() lst))

;; Pega os primeiros N elementos de uma lista
(define (take n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;; Zíper: Junta duas listas em pares (como o Python) 
;; Ex: (zip '(1 2) '(a b)) -> '((1 a) (2 b))
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

;; 2. CHECAGENS DE ALTO NÍVEL (Predicados de Coleção)
;; Retorna #t se ALGUM item da lista passar no teste (predicado)
(define (any? pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (any? pred (cdr lst)))))

;; Retorna #t apenas se TODOS os itens da lista passarem no teste
(define (all? pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          (all? pred (cdr lst))
          #f)))

;; 3. FERRAMENTA DE TESTE/VALIDAÇÃO
(define (assert condition msg)
  (if condition
      #t
      (error "[ASSERT FAILED]" msg)))

;; Junta uma lista de strings com um separador
;; Ex: (string-join '("A" "B" "C") "-") -> "A-B-C"
(define (string-join lst sep)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (string-join (cdr lst) sep)))))

;; Função Absoluta (Módulo)
(define (abs x)
  (if (< x 0) (- x) x))

;; Composição de funções: f(g(x))
;; Ex: (define dobro-absoluto (compose abs (lambda (x) (* x 2))))
(define (compose f g)
  (lambda (x) (f (g x))))

;; Verifica se um item existe na lista
;; Ex: (contains? '(1 2 3) 2) -> #t
(define (contains? lst item)
  (any? (lambda (x) (equal? x item)) lst))

;; ==========================================
;; ORDENAÇÃO (QUICK SORT)
;; Ex: (sort < '(3 1 4 1 5 9)) -> '(1 1 3 4 5 9)
;; ==========================================
(define (sort cmp? lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst 
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append 
          ;; 1. Ordena os menores (ou maiores) que o pivô
          (sort cmp? (filter (lambda (x) (cmp? x pivot)) rest))
          
          ;; 2. Aninhamos o segundo append aqui!
          (append (list pivot)
                  (sort cmp? (filter (lambda (x) (not (cmp? x pivot))) rest)))))))
