;; Standard library for the CS course.
(load "simply.scm")

;; Standard math functions
(define (square x) 
  (* x x))

(define (average a b)
  (/ (+ a b) 2.0))

(define (cube x)
  (/ (* x x x)))

(define (identity x)
  x)

(define (increment x)
  (+ x 1))

(define (close-enough? x y tolerance)
  (< (abs (- x y)) tolerance))
