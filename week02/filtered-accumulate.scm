(define (filtered-accumulate filter? combiner null-value term next a b)
  (cond
    ((> a b) null-value)
    ((filter? a) (combiner (term a) (filtered-accumulate filter? combiner null-value term next (next a) b)))
    (else (filtered-accumulate filter? combiner null-value term next (next a) b))))
