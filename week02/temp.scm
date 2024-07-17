(every
  (lambda (x) (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) x))) '(10 100 1000 10000 100000 10000000))
