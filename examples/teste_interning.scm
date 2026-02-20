
(displayln "=== TESTE DE STRING INTERNING ===")

;; 1. Criamos duas strings literais idênticas
(define s1 "Haki")
(define s2 "Haki")

;; 2. Criamos uma string idêntica dinamicamente (para provar que 
;;    o string-append também está usando o alloc_string do Pool!)
(define s3 (string-append "Ha" "ki"))

(displayln "\nUsando (equal?) - Compara o conteudo (Lento sem Interning):")
(display "s1 equal? s2: ") (displayln (equal? s1 s2)) ;; Sempre foi #t
(display "s1 equal? s3: ") (displayln (equal? s1 s3)) ;; Sempre foi #t

(displayln "\nUsando (eq?) - Compara EXATAMENTE a Tag/Endereço de Memória (Super Rápido):")
(display "s1 eq? s2: ") (displayln (eq? s1 s2))
(display "s1 eq? s3: ") (displayln (eq? s1 s3))

(displayln "\nSe os testes do (eq?) derem #t, significa que o String Interning está 100% PERFEITO!")
