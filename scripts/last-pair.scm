(define (last-pair l)
	(if (null? (cdr l))
	    (cons (car l) #nil)
	    (last-pair (cdr l))))
