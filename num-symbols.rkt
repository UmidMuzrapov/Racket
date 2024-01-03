#lang racket

(define (num-symbols lst)
  (if (equal? lst '())
      0
      (begin
        (if (symbol? (car lst))
            (+ 1 (num-symbols (cdr lst)))
            (num-symbols (cdr lst))
         )
        )
      )
  )