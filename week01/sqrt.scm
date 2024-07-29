(define (sqrt guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; Improved good-enough?:
(define (good-enough? curr-guess new-guess)
  (< (/ (abs (- curr-guess new-guess)) curr-guess) 0.001))

;; Updated sqrt:
(define (sqrt guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt (improve guess x) x)))
