(define (smooth f)
  (lambda (x) (average (f (- x 0.0001)) (f (+ x 0.0001)))))

(define (n-smooth f n)
  (lambda (x) ((repeated (smooth f) n) x)))
