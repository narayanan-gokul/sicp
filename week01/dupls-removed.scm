(define (dupls-removed arr)
  (cond
    ((equal? arr '()) arr)
    ((member? (first arr) (bf arr)) (dupls-removed (bf arr)))
    (else (sentence (first arr) (dupls-removed (bf arr))))))
