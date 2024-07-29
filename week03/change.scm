(define (count-change amount)
  (define (counter amount kind-of-coin)
    (cond
      ((= amount 0) 1)
      ((or (< amount 0) (= kind-of-coin 0)) 0)
      (else
	(+ (counter amount (- kind-of-coin 1))
	   (counter (- amount (denomination kind-of-coin)) kind-of-coin)))))

  (define (denomination coin)
    (cond 
      ((= coin 1) 1)
      ((= coin 2) 5)
      ((= coin 3) 10)
      ((= coin 4) 25)
      ((= coin 5) 50)))

  (counter amount 5))
