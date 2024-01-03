#lang racket

(define-syntax-rule
  (j-for (in-var in-val) test-ex post body ...)

  (let [(in-var in-val)]
  (let loop()
    (if test-ex
        (begin
          body ...
          post
          (loop)
          )
        (void)
        )
       ))
  )

(define-syntax-rule
(+= var value)
(let ([result (+ var value)])
(set! var result)
result))