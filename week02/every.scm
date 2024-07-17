(define (every procedure sent)
  (if (equal? sent '())
    sent
    (sentence (word (procedure (first sent))) (every procedure (bf sent)))))
