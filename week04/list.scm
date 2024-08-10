(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(define (reverse l)
  (if (null? (cdr l))
    l
    (append (reverse (cdr l)) (list (car l)))))

(define (same-parity x . y)
  (define (filter predicate? l)
    (cond
      ((null? l) (list))
      ((predicate? (car l)) (cons (car l) (filter predicate? (cdr l))))
      (else (filter predicate? (cdr l)))))

  (if (even? x)
    (filter even? (cons x y))
    (filter odd? (cons x y))))

(define (map f l)
  (if (null? l)
    '()
    (cons (f (car l)) (map f (cdr l)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
	    (cons (square (car things)) answer))))
  (iter items '()))

(define (for-each proc items)
  (if (null? items)
    #t
    ((lambda () (proc (car items)) (for-each proc (cdr items))))))
