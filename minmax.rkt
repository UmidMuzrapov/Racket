#lang racket
(define (minmax)
    (define args (minmax-helper 1 '(-inf.0 0) '(+inf.0 0)))
    (cond
      [(equal? 1 (car args)) (display "Empty file\n")]
      [(equal? (length ( cdr(last args))) (sub1 (car args))) (display "All same length\n")]
      [else  (printf "Min length: ~a ~a\nMax length: ~a ~a\n" (car(last args)) (format (cdr (last args)) '()) (car (cadr args)) (format (cdr (cadr args)) '()))]
      ) 
  
  
  
)

(define (format lst accum)
  (cond
    [(empty? lst) accum]
    [(equal? (length lst) 1) (append accum (list (car lst)))]
    [else (format (cdr lst) (append accum (list(string-append(number->string(car lst)) ","))))]
    )
  )

(define (minmax-helper cur-index max min)
    (define cur-line (read-line))
    
    (cond
        [(eof-object? cur-line) (cons cur-index (cons max (list min)))]
        [else (define current-length (string-length cur-line))
              (define new-max
                (cond
                  [(< (car max) current-length) (cons current-length (list cur-index))]
                  [(equal? (car max) current-length) (append max (list cur-index))]
                  [else max]
                  ))
              (define new-min
                (cond
                  [(> (car min) current-length) (cons current-length (list cur-index))]
                  [(equal? (car min) current-length ) (append min (list cur-index))]
                  [else min]
                 )
                )
              (minmax-helper (add1 cur-index) new-max new-min)
              ]
        )
    )

(minmax)