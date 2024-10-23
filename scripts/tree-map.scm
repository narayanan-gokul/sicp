(define tree (list 1 2 (list 3 4) (list 5 (list 6 7)) (list 8 (list 9))))

(define (scale-tree tree factor)
  (cond
    ((null? tree) #nil)
    ((not (pair? tree)) (* tree factor))
    (else
      (cons (scale-tree (car tree) factor)
	    (scale-tree (cdr tree) factor)))))

(scale-tree tree 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree) (if (not (pair? sub-tree))
			(* factor sub-tree)
			(scale-tree sub-tree factor))) tree))

(scale-tree tree 10)

(define (square-tree tree)
  (cond ((null? tree) #nil)
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree) (if (not (pair? sub-tree))
			    (* sub-tree sub-tree)
			    (square-tree sub-tree))) tree))

(define (tree-map proc tree)
  (cond ((null? tree) #nil)
	((not (pair? tree)) (proc tree))
	(else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (define (square x) (* x x))
  (tree-map square tree))

(square-tree tree)
(tree-map (lambda (x) (* x 20)) tree)

(define (subsets s)
  (if (null? s)
    (list #nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3 4))
