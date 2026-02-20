;; ==========================================
;; TESTE DE STACK TRACING - HAKI LISP
;; ==========================================

;; Camada 4: O fundo do poço.
;; Tenta extrair o primeiro item de um número (Erro Fatal!)
(define (extrai-valor dado)
  (displayln "  [3] Extraindo valor...")
  (car dado))

;; Camada 3: Uma função auxiliar genérica
(define (aplica-taxa base)
  (displayln "  [2] Aplicando taxa...")
  (extrai-valor base)) ;; Passa um número em vez de uma lista!

;; Camada 2: A regra de negócio principal
(define (calcula-imposto salario)
  (displayln "  [1] Calculando imposto...")
  ;; O 'let' internamente vira uma Closure (Lambda), 
  ;; adicionando mais uma camada invisível na pilha da VM!
  (let ((taxa 10)) 
    (aplica-taxa taxa)))

;; Camada 1: O ponto de entrada
(define (main)
  (displayln "Iniciando sistema fiscal...")
  (calcula-imposto 5000)
  (displayln "Sucesso!")) ;; Isso nunca vai ser impresso

;; Dispara a bomba!
(main)
