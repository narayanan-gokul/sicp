(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((not (pair? mobile)) mobile)
	((null? mobile) 0)
	(else (+ (total-weight (branch-structure (left-branch mobile)))
		 (total-weight (branch-structure (right-branch mobile)))))))

(define b0 (make-branch 3 4))
(define b1 (make-branch 4 3))
(define m0 (make-mobile b0 b1))
(define b2 (make-branch 5 m0))
(define b3 (make-branch 4 3))
(define m1 (make-branch b2 b3))

(define (make-result is-child-balanced? child-weight)
  (cons is-child-balanced? child-weight))

(define (is-child-balanced? result)
  (car result))

(define (child-weight result)
  (cdr result))

(define (branch-torque branch weight)
  (* (branch-length branch) weight))

(define (helper branch) 
  (cond
    ((not (pair? (branch-structure branch)))
     (make-result #t (branch-structure branch)))
    ((null? branch) (make-result #t 0))
    (else (let ((left-branch (left-branch (branch-structure branch)))
		(right-branch (right-branch (branch-structure branch))))
	    (let ((left-result (helper left-branch))
		  (right-result (helper right-branch)))
	      (let ((left-weight (child-weight left-result))
		    (right-weight (child-weight right-result))) 
		(if (and (is-child-balanced? left-result)
			 (is-child-balanced? right-result))
		  (if (equal? (branch-torque left-branch right-weight)
			      (branch-torque right-branch left-weight))
		    (make-result #t (+ right-weight left-weight))
		    (make-result #f 0))
		  (make-result #f 0))))))))

(define (balanced? mobile)
  (let 
    ((left-result (helper (left-branch mobile)))
     (right-result (helper (right-branch mobile))))
    (and
      (is-child-balanced? left-result)
      (is-child-balanced? right-result)
      (equal? (branch-torque (left-branch mobile) (child-weight left-result))
	      (branch-torque (right-branch mobile) (child-weight right-result))))))
(define right-result (helper (right-branch m0)))
(define left-result (helper (left-branch m0)))
left-result
right-result
(balanced? m0)
