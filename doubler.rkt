#lang racket
(define (doubler input)
  (helper '() input)
  )

(define (helper result input)
  (cond
    [(empty? input) result]
    [else (helper (append result (append (list (car input)) (list (car input)) ) ) (cdr input))]
    )
  )