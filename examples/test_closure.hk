;; 1. Criamos uma fábrica de contadores
(define (criar-contador)
  (let ((total 0))
    (lambda () 
      (set! total (+ total 1))
      total)))

;; 2. Instanciamos o primeiro contador
(define contador-a (criar-contador))

;; NESSA LINHA EXATA, o GC vai rodar (pois o run_source aciona no top-level).
;; O ambiente temporário do 'let' teoricamente seria lixo, 
;; MAS o seu código novo no VmClosure vai salvá-lo!

;; 3. Testamos se o estado sobreviveu e se atualiza
(displayln "Testando Contador A:")
(displayln (contador-a)) ;; Deve imprimir 1
(displayln (contador-a)) ;; Deve imprimir 2
(displayln (contador-a)) ;; Deve imprimir 3

;; 4. Criamos um SEGUNDO contador para provar que os escopos são independentes
(define contador-b (criar-contador))

(displayln "Testando Contador B (Estado isolado):")
(displayln (contador-b)) ;; Deve imprimir 1 (nasceu com o próprio 'total' zerado)

(displayln "Voltando pro Contador A:")
(displayln (contador-a)) ;; Deve imprimir 4 (o estado dele continuou intacto!)
