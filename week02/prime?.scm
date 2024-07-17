(define (prime? x)
  (define (iter a)
    (cond
      ((< x (* a a)) #t)
      ((= x (* a a)) #f)
      ((= (modulo x a) 0) #f)
      (else (iter (+ a 1)))))
  (if (= 1 x) 
    #t
    (iter 2)))
