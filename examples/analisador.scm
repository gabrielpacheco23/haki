
(define arquivos-brutos (shell "ls -1 *.scm"))
(define lista-arquivos (string-split arquivos-brutos "\n"))

(define (nao-vazio? str)
  (not (string-empty? str)))

(define arquivos-validos (filter nao-vazio? lista-arquivos))

(define (contar-linhas nome-arquivo)
  (let* ((conteudo (slurp nome-arquivo)))
    (if (string? conteudo) ;; Proteção contra leitura falha
        (let ((linhas (string-split conteudo "\n")))
          (length linhas))
        0)))

; (define (contar-linhas nome-arquivo)
;   (let* ((conteudo (slurp nome-arquivo))
;         (linhas (string-split conteudo "\n")))
;     (length linhas)))

(define linhas-por-arquivo (map contar-linhas arquivos-validos))

(define total-linhas (fold + 0 linhas-por-arquivo))

(define relatorio
  (string-append 
    "=== Relatorio do Projeto Lisp ===\n"
    "Arquivos encontrados: " (number->string (length arquivos-validos)) "\n"
    "Total de Linhas de Codigo: " (number->string total-linhas) "\n"
    "=================================\n"))

(display relatorio)
(write-file "STATUS_PROJETO.txt" relatorio)
