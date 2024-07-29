(define (cubert guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cubert (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
