#!/usr/local/bin/guile -s
!#

(load "sequences.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map 
    (lambda (x) (map * x v)) 
    m))

; Collect all column elements into separate lists and string them together.
(define (transpose mat)
  (accumulate-n cons #nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
      (lambda (row) 
; For every row in the matrix do:
; For every column in cols, multiply every element with corresponding element in 
; the row and calculate sum
	(map (lambda (col) (accumulate + 0 (map * row col))) cols)) 
      m)))

(define v1 (list 1 2 3))
(define v2 (list 2 3 4))
(define v3 (list -2 1 0))
(define m1 (list (list 1 2 3) (list 1 2 3) (list 1 2 3)))

(display (dot-product v1 v2))
(newline)

(display (dot-product v1 v3))
(newline)

(display (matrix-*-vector m1 v1))
(newline)

(display (transpose m1))
(newline)

(display (matrix-*-matrix m1 m1))
(newline)
