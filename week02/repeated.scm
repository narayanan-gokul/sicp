(define (repeated f n)
  (if (= 1 n) 
    (lambda (x) (f x))
    (compose f (repeated f (- n 1)))))
