(define (ordered? nums)
  (cond
    ((equal? (bf nums) '()) #t)
    ((<= (first nums) (first (bf nums))) (ordered? (bf nums)))
    (else #f)))
