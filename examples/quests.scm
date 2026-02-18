;; 1. Simulando um JSON recebido do app Flutter
(define json-do-app 
  "{ 
     \"jogador\": \"Aventureiro\",
     \"quests_concluidas\": [
       { \"id\": 1, \"tipo\": \"foco\", \"xp\": 100 },
       { \"id\": 2, \"tipo\": \"física\", \"xp\": 50 },
       { \"id\": 3, \"tipo\": \"foco\", \"xp\": 150 }
     ]
   }")

;; 2. Parse nativo para as nossas novas estruturas (HashMaps e Vectors)
(define payload (parse-json json-do-app))
(define quests-array (hash-ref payload "quests_concluidas"))

;; 3. Filtro de missões usando o Operador Pipe (|>)!
;; Lemos de cima para baixo de forma perfeitamente natural:
(define quests-de-foco
  (|> quests-array
      (vector->list) ;; Converte o Vetor de quests para Lista Ligada
      (filter (λ (q) (equal? (hash-ref q "tipo") "foco")))))

;; 4. Calculando XP total
(define xp-ganho
  (|> quests-de-foco
      (map (λ (q) (hash-ref q "xp")))
      (fold + 0)))

(displayln "XP Total ganho com missões de foco: ")
(displayln xp-ganho) ;; Vai imprimir: 250!
