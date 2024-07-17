(define (pi-sum a b) 
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (* 8 (sum pi-term pi-next a b)))
