(define (sum-list l)
  (define (iter ans items)
	(if (null? items)
	  ans
	  (iter (+ ans (car items)) (cdr items))))
  (iter 0 l))

(define (sum-list l)
  (if (null? l)
	0
	(+ (car l) (sum-list (cdr l)))))
