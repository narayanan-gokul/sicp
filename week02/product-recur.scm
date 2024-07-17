(define (product term next a b)
  (if (> a b) 
    1
    (* (term a) (product term next (next a) b))))
