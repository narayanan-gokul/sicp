#!/usr/local/bin/guile -s
!#
; Nested loops are constructed using nested mappings.

(load "sequences.scm")
(define (enumerate-interval start end)
  (if (> start end)
    #nil
    (cons start (enumerate-interval (+ start 1) end))))

(define (pairs n)
  (accumulate
    append
    #nil
    (map
      (lambda (i) (map (lambda (j) (list j i)) (enumerate-interval 1 (- i 1)))) 
      (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append #nil (map proc seq)))

(define (pairs n)
  (flatmap
    (lambda
      (i)
      (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (even-sum? pair)
  (even? (+ (car pair) (cadr pair))))

(define (make-sum-pair pair)
  (list (car pair)
	(cadr pair)
	(+ (car pair) (cadr pair))))

(define (even-sum-pairs n)
  (map make-sum-pair
       (filter even-sum?
	       (pairs n))))

(define (remove x s)
  (filter (lambda (y) (not (= x y))) s))

(define (permutations s)
  (if (null? s)
    (list #nil)
    (flatmap
      (lambda (x) (map (lambda (p) (cons x p)) (permutations (remove x s))))
      s)))

(define (ordered-triples n)
  (flatmap
    (lambda
      (i)
      (flatmap
	(lambda
	  (j)
	  (map
	    (lambda
	      (k)
	      (list k j i))
	    (enumerate-interval 1 (- j 1))))
	(enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (sum-triples sum n)
  (filter
    (lambda (triple)
      (= sum (+ (car triple) (cadr triple) (caddr triple))))
    (ordered-triples n)))

(display (sum-triples 9 5))
(newline)

; (display (ordered-triples 5))
; (newline)

; (display (permutations (list 1 2 3)))
; (newline)

; (display (even-sum-pairs 5))
; (newline)
; 
; (display (pairs 5))
; (newline)
