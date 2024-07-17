(define (squares nums)
  (if (equal? nums '()) 
    nums 
    (sentence (word (square (first nums))) (squares (bf nums)))))

(define (square x)
  (* x x))
