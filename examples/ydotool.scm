(define (auto-press-key k ms)
  (cond
    ((= k 2) (begin (shell "ydotool key 3:1 3:0") (sleep ms)))
    ((= k 3) (begin (shell "ydotool key 4:1 4:0") (sleep ms)))
    ((= k 4) (begin (shell "ydotool key 5:1 5:0") (sleep ms)))
    (else (void))))
