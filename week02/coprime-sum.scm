(define (coprime-product n)
  (define (filter? a)
    (= (gcd a n) 1))
  (filtered-accumulate filter? product-combiner 1 identity increment 1 n))
