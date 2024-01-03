(module ruler racket
    (provide (all-defined-out))

(define (ruler x)
  (if (and (number? x) (> x 0) (< x 100))
 
      (begin
             (when (> (quotient x 10) 0)
             (print-big-general (quotient x 10) 1))
             (newline)
             (print-small-general (quotient x 10))
             (print-remainder (remainder x 10))
  )
      (display(make-string 1 (integer->char 63)))
  )
  )

(define (print-small-general x)
  (if (equal? x 0)
      (+ 1 3)
      (begin
        (print-small)
        (print-small-general (sub1 x))
        )
      )
  )

( define (print-big-general x x2)
   (if (equal? x x2)
       (print-big x)
       (begin
         (print-big x2)
         (print-big-general x (+ x2 1))
         )
       )
   )


(define (print-big x)
  (display (string-append (make-string 9 (integer->char 32)) (number->string x)))
  )

(define (print-?)
  (display (make-string 1 (integer->char 63)))
  )

(define (print-small)
  (display (number->string 1234567890))
  )

(define (check-condition x)
  (and (number? x) (integer? x))
  )

(define (print-remainder x)
  (for ([i (in-range 1 (add1 x))])
    (display i))
  )
  )