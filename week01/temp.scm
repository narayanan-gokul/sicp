(define (func x)
  (define (aux x)
    (+ x 1))
  (* (aux x) x))

(define (aux x)
  (+ x 2))
