#lang racket
(define (eq-pairs? lst)
  (cond
    [(empty? lst) #t]
    [(empty? (cdr lst)) #f]
    [else
     (if (equal? (car lst) (cadr lst))
         (eq-pairs? (cddr lst))
         #f
         )
     ]
    )
  )