#!/usr/local/bin/guile -s
!#

(define (sum-odd-leaves tree)
  (cond
    ((null? tree) 0)
    ((not (pair? tree)) (if (odd? tree) tree 0))
    (else (+ (sum-odd-leaves (car tree)) (sum-odd-leaves (cdr tree))))))

(define (filter pred? seq)
  (cond
    ((null? seq) #nil)
    ((pred? (car seq)) (cons (car seq) (filter pred? (cdr seq))))
    (else (filter pred? (cdr seq)))))

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (reverse-left seq)
  (fold-left (lambda (x y) (cons y x)) #nil seq))

(define (reverse-right seq)
  (accumulate (lambda (x y) (append y (list x))) #nil seq))

(define (enumerate tree)
  (cond
    ((null? tree) #nil)
    ((not (pair? tree)) (list tree))
    (else (append (enumerate (car tree)) (enumerate (cdr tree))))))

(define (sum-odd-leaves tree)
  (accumulate + 0 (filter odd? (enumerate tree))))

(define (append seq1 seq2) 
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* x higher-terms)))
    0
    coefficient-sequence))

; The tree can be reduced to a sequence of number of leaves on the sub tree at
; every node.
(define (count-leaves tree)
  (accumulate 
    +
    0
    (map (lambda (x) (if (pair? x) (count-leaves x) 1)) tree)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    #nil
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
	  (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))  

(define fold-right accumulate)

; (define l1 (list 7 6 5 4 3 2 1))
; (display (reverse-right l1))
; (newline)
; (define l2 (list 1 2 3 4 5 6 7))
; (define tree (list 1 (list 3 4) (list 5 6 (list 7 8 (list 9)))))
; (define l3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; (display (map (lambda (x) (* x 10))  l1))
; (newline)
; 
; (display (append l1 l2))
; (newline)
; 
; (display (length l1))
; (newline)
; 
; (display (horner-eval 2 (list 1 3 0 5 0 1)))
; (newline)
; 
; (display (count-leaves tree))
; (newline)
; 
; (display (accumulate-n * 1 l3))
; (newline)
