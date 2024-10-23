(define (square-list items)
  (if (null? items)
	#nil
	(cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square n)
  (* n n))

(define (square-list items)
  (map square items))
