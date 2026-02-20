;; ==========================================
;; O TESTE DO VALE DA MORTE
;; ==========================================

(displayln "Iniciando o Teste de Fogo do Haki Lisp...")
(displayln "Abra o Gerenciador de Tarefas. A memória RAM deve ficar cravada e estável!\n")

(define (teste-de-fogo contador maximo)
  (if (= contador maximo)
      (begin
        (displayln "\n=========================================")
        (display "VITÓRIA! O loop rodou ")
        (display maximo)
        (displayln " de vezes sem quebrar a pilha (TCO Funciona!).")
        (displayln "E a memória não estourou (GC e NaN Tagging Perfeitos!).")
        (displayln "=========================================")
        #t) ;; Retorna Booleano (Testa o NaN Tagging na saída)

      ;; CASO RECURSIVO (Onde a magia acontece)
      (begin
        ;; 1. TESTE DE GC E NAN TAGGING:
        ;; Alocamos uma lista cheia de números e strings na Arena.
        ;; Como não retornamos essa variável, ela vira "lixo" imediatamente.
        (define lixo-temporario (list "Haki" "Lisp" contador (* contador 2)))
        
        ;; 2. Forçamos o GC a rodar a cada 100.000 iterações para vermos os logs no terminal
        (if (= (modulo contador 100000) 0)
            (begin
              (display ">>> Atingiu a iteração: ")
              (displayln contador)
              (gc)) ;; Chama o nosso GC manualmente!
            void)

        ;; 3. TESTE DE TCO: Recursão de Cauda Perfeita
        ;; A VM não pode criar um novo CallFrame para isso, deve reaproveitar o atual!
        (teste-de-fogo (+ contador 1) maximo))))

;; Vamos rodar 1 MILHÃO de iterações!
;; (Se o seu PC for mais modesto, diminua para 500000)
(teste-de-fogo 0 1000000)
