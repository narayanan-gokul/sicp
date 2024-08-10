(define (count-change amount currency)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (null? currency)) 0)
    (else (+
      (count-change amount (cdr currency))
      (count-change (- amount (car currency)) currency)))))
