(define (accumulate combiner null-value term next a b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term next (next a) b))))
