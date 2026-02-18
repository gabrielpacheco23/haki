(define cadr (lambda (x) (car (cdr x))))

(define (map f L)
            (if (null? L)
                (list)
                (cons (f (car L)) (map f (cdr L)))))

(defmacro (let bindings &rest body)
          (cons (cons 'lambda (cons (map car bindings) body))
                (map cadr bindings)))

(defmacro (let* bindings body)
  (if (null? bindings)
      `(let () ,body)
      `(let (,(car bindings))
         (let* ,(cdr bindings) ,body))))
           
(defmacro (cond &rest clauses)
  (if (null? clauses)
      '()
      (let ((clause (car clauses))
            (rest   (cdr clauses)))
        (if (equal? (car clause) 'else)
            (cadr clause)
            (list 'if 
                  (car clause)        
                  (cadr clause)       
                  (cons 'cond rest)   
            )))))

(defmacro (and &rest args)
  (if (null? args)
      #t
      (if (null? (cdr args))
          (car args)
          (list 'if (car args) 
                    (cons 'and (cdr args)) 
                    #f))))

(defmacro (or &rest args)
  (if (null? args)
      #f
      (if (null? (cdr args))
          (car args)
          (let ((temp (car args)))
            (list 'let (list (list 'temp temp))
                  (list 'if 'temp 'temp (cons 'or (cdr args))))))))

(defmacro (|> x &rest forms)
  (if (null? forms)
      x
      (let ((form (car forms))
            (rest (cdr forms)))
        (let ((new-x (if (list? form)
                         (append form (list x))
                         (list form x))))
          (cons '|> (cons new-x rest))))))

;; A Macro de Pattern Matching Básica
(defmacro (match expr &rest clauses)
  ;; 1. Envolvemos tudo num 'let' e criamos uma variável oculta '__val
  (list 'let (list (list '__val expr))
        ;; 2. Iniciamos a construção do 'cond'
        (cons 'cond
              ;; 3. Mapeamos cada cláusula para o formato do 'cond'
              (map (lambda (clause)
                     (let ((pattern (car clause))
                           (body (cadr clause)))
                       (if (equal? pattern '_)
                           ;; Se o padrão for '_', vira o 'else' do cond
                           (list 'else body)
                           ;; Se não, vira um teste de equal?
                           (list (list 'equal? '__val pattern) body))))
                   clauses))))


;; COMPILADOR DE PADRÕES
;; Retorna sempre uma lista com 2 itens: (condição bindings)
(define (compile-pattern pat target)
  (cond
    ;; 1. Se for o wildcard '_', sempre dá verdadeiro e não captura nada
    ((equal? pat '_) 
     (list #t '()))
    
    ;; 2. Se for um símbolo (ex: alvo), é uma variável! Capturamos ela!
    ((symbol? pat) 
     (list #t (list (list pat target))))
    
    ;; 3. Se for uma lista (ex: ("atacar" alvo)), faz a recursão profunda
    ((list? pat) 
     (compile-list-pattern pat target))
    
    ;; 4. Se for qualquer outra coisa (string, número), testa a igualdade
    (else 
     (list (list 'equal? target pat) '()))))

;; COMPILADOR DE LISTAS PROFUNDAS
(define (compile-list-pattern pat-list target)
  (if (null? pat-list)
      ;; Se a lista acabou, a condição é que o alvo também seja vazio (null?)
      (list (list 'null? target) '())
      
      ;; Se não, quebramos a cabeça e a cauda da lista (car / cdr)
      (let ((head-res (compile-pattern (car pat-list) (list 'car target)))
            (tail-res (compile-pattern (cdr pat-list) (list 'cdr target))))
        
        ;; Juntamos a condição (o alvo TEM que ser um par + condições filhas)
        (list (list 'and (list 'pair? target) (car head-res) (car tail-res))
              ;; E concatenamos as variáveis capturadas!
              (append (cadr head-res) (cadr tail-res))))))

(defmacro (match expr &rest clauses)
  ;; Função auxiliar que compila uma única cláusula do match
  (let ((compile-clause 
         (lambda (clause)
           (let* ((pat (car clause))
                  (body-exprs (cdr clause)) ;; Pega todas as linhas de código do corpo
                  ;; Chama o nosso cérebro compilador!
                  (compiled (compile-pattern pat '__match_target))
                  (condition (car compiled))
                  (bindings (cadr compiled)))
             ;; Monta a AST final para esta cláusula:
             ;; (condicao (let ((var extraida)) codigo1 codigo2))
             (list condition (cons 'let (cons bindings body-exprs)))))))
    
    ;; Envelopa tudo para avaliar a expressão apenas uma vez
    (list 'let (list (list '__match_target expr))
          (cons 'cond (map compile-clause clauses)))))
