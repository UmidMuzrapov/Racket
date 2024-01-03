#lang racket
(define (process lst)
  (cond
    [(and (pair? (cadr lst)) (> (length lst) 2))
     (define procedure-name (caadr lst))
     (define form-number (length (cddr lst)))
     (define arguments (get-arguments (cadr lst)))
     (define uses (get-uses-2 (cddr lst) '()))
     (print-result procedure-name form-number arguments uses)
     ]
    [else (void)]) )

(define (pinfo filename)
  (define (read-and-print port)
    (let ([expr (read port)])
      (cond
        [(equal? expr eof) (void)]
        [else ( process expr)
              (read-and-print port)])))
  (read-and-print (open-input-file filename)))

(define (print-result procedure-name form-number arguments uses)
  (printf "=== ~a ===\n" procedure-name)
  (if (equal? form-number 1)
      (printf "1 body form\n")
      (printf "~a body forms\n" form-number)
   )
  (print-arguments arguments)
  (printf "Uses: ~a\n\n" (format-list (remove-duplicates (sort uses symbol<?)) ""))
  )


(define (print-arguments arguments)
  (cond
    [(empty? arguments) (printf "No arguments\n")]
    [(equal? 'variadic (car arguments))
     (if (empty? (cdr arguments))
         (printf "Variadic, none required\n")
         (begin
           (if (equal? 2 (length arguments))
               (printf "Variadic, ~a required argument: ~a\n" (length (cdr arguments)) (format-list(cdr arguments) ""))
               (printf "Variadic, ~a required arguments: ~a\n" (length (cdr arguments)) (format-list(cdr arguments) ""))
            )  
          )
      )
     ]
    [else
     (if (equal? 1 (length arguments))
         (printf "~a argument: ~a\n" (length arguments) (format-list (remove-duplicates  arguments) ""))
         (printf "~a arguments: ~a\n" (length arguments) (format-list (remove-duplicates  arguments) ""))
      )
      ]
    )
  )

(define (get-uses-2 lst accum)
  (cond
    [(empty? lst) accum]
    [(list? (car lst)) (define internal-list (get-uses-2 (car lst) accum)) (get-uses-2 (get-lists (cdr lst) '()) internal-list)]
    [else (define internal-list (cons (car lst) accum)) (get-uses-2 (get-lists lst '()) internal-list)]
     )
   )
 
(define (get-lists lst accum)
  (cond
    [(empty? lst) accum]
    [else
     (if (list? (car lst))
         (get-lists (cdr lst) (cons (car lst) accum))
         (get-lists (cdr lst) accum))
     ]))


(define (get-arguments arguments)
  (cond
    [(equal? 'args (take-right arguments 0)) (cons 'variadic (cdr (drop-right arguments 0)))]
    [else (cdr arguments)]
    )
  )

(define (format-list lst string-ac)
  (cond
    [(empty? lst) string-ac]
    [(equal? 1 (length lst)) (string-append string-ac (symbol->string(car lst)))]
    [else (format-list (cdr lst) (string-append string-ac (string-append (symbol->string(car lst)) ", ")))]
    )
  )


