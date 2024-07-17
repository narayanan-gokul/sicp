(define (cont-frac n d k)
  (define (frac-iter n d i)
    (if (> i k) 
      0
      (/ (n i) (+ (d i) (frac-iter n d (+ i 1))))))
  (frac-iter n d 1))

(define (cont-frac n d k)
  (define (frac-iter i result)
    (if ( = 0 i)
      result
      (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (frac-iter k 0))

(define (euler-d i)
  (cond
    ((= (modulo (+ i 1) 3) 0) (* 2 (/ (+ i 1) 3)))
    (else 1)))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x -1))) (lambda (i) (- (* 2 i) 1)) k ))
