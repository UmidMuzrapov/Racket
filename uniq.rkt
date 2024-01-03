#lang racket

(define (uniq input)
    (display (foldl format "" (foldl f '() input)))
  )


(define (f element accum)
  (cond
    [(empty? accum) (append accum (list (list->vector (cons 1 (list element)))))]
    [else (define last-elem (vector-ref (last accum) 1))
     (if (equal? element last-elem)
         (begin
           (vector-set! (last accum) 0 (add1 (vector-ref (last accum) 0)))
           accum
           )
         
         (append accum (list (list->vector (cons 1 (list element)))))
         )]))

(define (format input accum)
  (string-append accum (string-append (number->string (vector-ref input 0)) " " (vector-ref input 1) "\n"))
  )

(define input (port->lines (current-input-port)))

(uniq input) 