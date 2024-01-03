#lang racket

(define (longer-syms lst1 lst2)
  (cond
   [(or (empty? lst1) (empty? lst2)) '()]
   [(equal? (string-length ( symbol->string (car lst1))) (string-length (symbol->string (car lst2))))
      (append (list ':samelen:) (longer-syms (cdr lst1) (cdr lst2)))]
   [(< (string-length (symbol->string (car lst1))) (string-length (symbol->string(car lst2))))
         (append (list(car lst2)) (longer-syms (cdr lst1) (cdr lst2))) ]
   [else (append (list(car lst1)) (longer-syms (cdr lst1) (cdr lst2)))]
   )
  )
