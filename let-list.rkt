#lang racket
(define-syntax-rule (let-list head tail list body ...)
  (let
    [(head (car list))
    (tail (cdr list))]
     body ...
   )
  )
