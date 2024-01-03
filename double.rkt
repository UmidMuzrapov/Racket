#lang racket

(define-syntax double
  (syntax-rules ()
    ((_) (void))
    ((_ var)
     (begin
       (set! var (cond
                   [(number? var) (* var 2)]
                   [(string? var)(string-append var var)]
                   [(list? var) (append var var)]
                   [else (vector-append var var)]
                   ))))
    ((_ var rest ...)
     (begin
       (set! var (cond
                   [(number? var) (* var 2)]
                   [(string? var) (string-append var var)]
                   [(list? var) (append var var)]
                   [else (vector-append var var)]
                    ))
       (double rest ...)))))

