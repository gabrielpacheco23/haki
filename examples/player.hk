
;; 1. Using Vector Literal (Square Brackets)
(define inventary ["Sword" "Health Potion" "Shield"])

;; Reading from vector (index 1 = Health Potion)
(display "Inventary's items: ")
(displayln (vector-ref inventary 1))


;; 2. Using HashMap Literal (Curly Brackets)
(define player
  { 
    "name" "Arthur"
    "class" "Paladin"
    "level" 5
    "attributes" { "strength" 18 "magic" 12 } ;; Nested hashmaps!
  })

;; Reading the HashMap properties
(display "The hero's name is: ")
(displayln (hash-ref player "name"))

;; Accessing a HashMap inside another HashMap
(define player-attributes (hash-ref player "attributes"))
(display "The hero's strength is: ")
(displayln (hash-ref player-attributes "strength"))


;; 3. Mixing both (Object array)
(define quests
  [
    { "id" 1 "title" "Slay Goblins" "done" #t }
    { "id" 2 "title" "Save the King"  "done" #f }
  ])

;; Taking the first quest from the array and reading its title
(define first-quest (vector-ref quests 0))
(displayln (hash-ref first-quest "title"))
