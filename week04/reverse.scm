(define (reverse seq)
  (define (iter l res)
    (if (empty? l)
      res
      (iter (cdr l) (cons (car l) res))))
  (iter seq '()))
