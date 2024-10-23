(define (length l)
  (if (null? l)
    0
    (+ 1 (length (cdr l)))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))
