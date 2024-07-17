(define (ends-e sent)
  (cond
    ((equal? sent '()) sent)
    ((equal? 'e (last (first sent))) (sentence (first sent) (ends-e (bf sent))))
    (else (ends-e (bf sent)))))
