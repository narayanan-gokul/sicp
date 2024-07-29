(define (f x)
  (cond 
   ((< x 3) x)
   (else (+ (f (- x 1)) (* 2 (f (- x 2))) (* 3 (f (- x 3)))))))

(define (f x)
  (define (iter n n-1 n-2 n-3)
    (let ((curr (+ n-1 (* 2 n-2) (* 3 n-3)))) 
      (if (= x n)
        curr
        (iter (+ n 1) curr n-1 n-2))))

  (if (< x 3)
    x
    (iter 3 2 1 0)))
