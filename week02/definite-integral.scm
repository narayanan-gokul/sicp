(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx (sum f add-dx (+ a (/ dx 2.0)) b)))


(define (simpson-integral f a b n)
  (define h (/ (- b a) n ))
  (define (term k) (f (+ a (* k h))))
  (define (next x) (+ x 2))
  (* (/ h 3.0) (+ (f a) (f b) (* 2 (+ (* 2 (sum term next 1 (- n 1))) (sum term next 2 (- n 2)))))))
