(module unary racket
    (provide (all-defined-out))

    (define (unary-zero) '())
    
    (define (unary-one) '(1))
    
    (define (unary-zero? n) (equal? n (unary-zero)))
    
    (define (unary-one? n)  (equal? n (unary-one)))

    ; Put your (define ...)s below for the procedures you are to write,
    ; being sure all are enclosed in this `module` declaration.
    
    (define (unary? n)
      (cond
        [(not (list? n)) #f]
        [(empty? n) #t]
        [else
         (if (equal? 1 (car n))
             (unary? (cdr n))
             #f
             )
         ]
        )
      )
  
  (define (unary->decimal n)
    (if (equal? n (unary-zero))
        0
        (add1 (unary->decimal (cdr n))) 
        )
    )


  
  (define (unary-add a b)
    (cond
      [(empty? a) b]
      [else (unary-add (cdr a) (cons (car a) b))]
      )
    
    )
  

  (define (unary-sub a b)
    ( cond
       [(and(empty? a) (empty? b)) '()]
       [(empty? a) 'negative]
       [(empty? b) a]
       [else (unary-sub (cdr a) (cdr b))]
       )
    )
  
  (define (decimal->unary n)
  (if (equal? n 0)
      '()
      (cons 1 (decimal->unary (sub1 n)))
      )
    )

  (define (unary-lt a b)
    (cond
      [(empty? b) #f]
      [(empty? a) #t]
      [else  (unary-lt (cdr a) (cdr b))]
      )
    )

  (define (unary-mul a b)
    (helper a '() b)
    )

  (define (helper a result b)
    (cond
      [(empty? b) result]
      [else (helper a (unary-add result a) (cdr b) )]
      )
    )
)



