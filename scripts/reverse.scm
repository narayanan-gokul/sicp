(define (reverse l)
  (define (reverse-iter res items)
	(if (null? items)
		res
		(reverse-iter (cons (car items) res) (cdr items))))
  (reverse-iter #nil l))
