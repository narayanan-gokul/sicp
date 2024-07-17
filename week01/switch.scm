;; Sentence -> Sentence
;; Produces a sentence with all occurrences of 'I' and 'me' is replaced with
;; 'you' and every instance of 'you' is replaced with 'me' except for the first
;; word in a sentence where it is replaced with 'I'

;(define (switch sent) '())  ;Stub
(define (switch sent)
  (if (equal? (first sent) 'you) 
    (sentence 'i (switch-helper (bf sent))) 
    (switch-helper sent)))

(define (switch-helper sent)
  (cond 
    ((equal? sent '()) sent)
    ((equal? (first sent) 'you) (sentence 'me (switch-helper (bf sent))))
    ((equal? (first sent) 'i) (sentence 'you (switch-helper (bf sent))))
    ((equal? (first sent) 'me) (sentence 'you (switch-helper (bf sent))))
    (else (sentence (first sent) (switch-helper (bf sent))))))
