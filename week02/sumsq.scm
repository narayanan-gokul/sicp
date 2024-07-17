(define (sumsq a b)
  (define (square x) (* x x))
  (+ (square a) (square b)))

;; Equivaent to just sumsq in lambda:
((lambda (a b) (+ (square a) (square b))) 3 4)

;; Equivalent of square using lambda:
(lambda (x) (* x x))

((lambda (a b) 
  ((lambda (square) (+ (square a) (square b))) (lambda (x) (* x x)))) 3 4)

(define (fact n)
  (if (= n 0)
    1
    (* n (fact (- n 1)))))
