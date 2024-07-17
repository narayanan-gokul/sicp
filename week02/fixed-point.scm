(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess 0.0001)
        next
        (try next))))
  (try first-guess))
