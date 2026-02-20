; (define resposta-bruta
;   (shell "curl -s https://dummyjson.com/products/4"))

(define api-url "https://dummyjson.com/products/")
(define raw-response
  (http-get (string-append api-url (number->string 1))))

(define product (parse-json raw-response))

(define title (hash-ref product "title"))
(define desc (hash-ref product "description"))
(define price (hash-ref product "price"))
(define stock (hash-ref product "stock"))

(define tags (hash-ref product "tags"))
(define first-tag (vector-ref tags 0))

(displayln "=== API RESULTS ===")
(displayln (string-append "Name: " title))
(displayln (string-append "Description: " desc))
(displayln (string-append "Price: $" (number->string price)))
(displayln (string-append "Stock: " (number->string stock) " units"))
(displayln (string-append "Category: " first-tag))

