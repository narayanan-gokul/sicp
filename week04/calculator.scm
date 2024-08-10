(define (calc)
  (display "calc> ")
  (display (calc-eval (read)))
  (newline)
  (calc))

(define (calc-eval exp)
  (cond
    ((number? exp) exp)
    ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
    (else (error "Calc: bad expresson:" exp))))

(define (calc-apply op args)
  (cond
    ((equal? op '+) (accumulate + args))
    ((equal? op '*) (accumulate * args))
    ((equal? op '-) (accumulate - args))
    ((equal? op '/) (accumulate / args))
    (else (error "Calc: bad operator:" op))))
