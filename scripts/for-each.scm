(define (for-each proc l)
  (if (null? l)
	#nil
	((lambda (x) (proc x) (for-each proc (cdr l))) (car l))))
