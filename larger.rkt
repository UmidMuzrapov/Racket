#lang racket

(define (larger lower-bound . args)
  (helper lower-bound args)
  )

(define (helper lower-bound args)
  (cond
    [(empty? args) '()]
    [(< lower-bound (car args)) (append (list (car args)) (helper lower-bound (cdr args)))]
    [else (helper lower-bound (cdr args))]
    )
  )