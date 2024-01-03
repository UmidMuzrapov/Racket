#lang racket
(define (fsort . args)
   (append (cons 'stack: (list args)) (cons 'flips (list (fsort-helper args '()))))
  )

(define (fsort-helper lst accum)
  (cond
    [(empty? lst) accum]
    [(equal? (length lst) (length (sorted-part lst))) accum]
    [(equal? (car lst) (find-max (unsorted-part lst)))
     (fsort-helper (flip(unsorted-part lst)) (append accum (list (length (unsorted-part lst)))))]
    [else (fsort-helper
           (append (flip (flip-part (unsorted-part lst))) (unflip-part (unsorted-part lst)))
           (append accum (list (flip-point (unsorted-part lst))))
           )
          ]
    
    )
  )

(define (flip lst)
  (cond
    [(empty? lst) '()]
    [ else (append (list(last lst)) (flip (init lst)))]
    )
  )

(define (flip-part lst)
  (take lst (flip-point lst))
  )
  
(define (unflip-part lst)
  (drop lst (flip-point lst))
  )
  
(define (flip-point lst)
  ( add1(index-of lst (find-max lst)))
  )

(define (begins-with-max lst)
  (if (equal? (car lst) (find-max lst))
      #t
      #f
   )
  )

(define (unsorted-part lst)
  (take lst (how-many-to-take lst))
  )

(define (sorted-part lst)
  (drop lst (how-many-to-take lst))
  )

(define (how-many-to-take lst)
  (- (length lst) (how-many-to-ignore lst 0))
  )

(define (how-many-to-ignore lst n)
  (cond
    [(empty? lst) n]
    [(equal? (last lst) (find-max lst)) (how-many-to-ignore (init lst) (add1 n))]
    [else  n ]
  ))

(define (find-max lst)
  (cond
    [(empty? lst) 'none]
    [(equal? 1 (length lst)) (car lst)]
    [(< (car lst) (cadr lst)) (find-max (append (list (cadr lst)) (cddr lst)))]
    [else (find-max (append (list(car lst)) (cddr lst)))]
    )
  )

  (define (init lst)
    (cond
      [(empty? lst) '()]
      [else (take lst (sub1 (length lst)))]
      )
    )