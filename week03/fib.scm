(define (fib x)
  (cond ((= x 0) 0)
	((= x 1) 1)
	(else (+ (fib (- x 1)) (fib (- x 2))))))

(define (fib x)
  (define (iter n-1 n-2 n)
    (cond ((= n 0) n-2)
	  (else (iter (+ n-1 n-2) n-1 (- n 1)))))
  (iter 1 0 x))

(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (iter a
		     b
		     (+ (square p) (square q))
		     (+ (square q) (* 2 p q))
		     (/ count 2)))
	  (else (iter (+ (* b q)
			 (* a p)
			 (* a q))
		      (+ (* b p)
			 (* a q))
		      p
		      q
		      (- count 1)))))
  (iter 1 0 0 1 n))

