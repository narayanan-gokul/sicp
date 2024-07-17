(define (search f neg-point pos-point)
  (let ((midpoint 
          (average neg-point pos-point))) 
    (if (close-enough? neg-point pos-point 0.001)
      midpoint
      (let ((test-value (f midpoint)))
        (cond
          (((lambda (x) (> x 0)) test-value) 
           (search f neg-point midpoint))
          (((lambda (x) (< x 0)) test-value)
           (search f midpoint pos-point))
          (else midpoint))))))

(define (half-interval f a b)
  (let ((a-val (f a)) 
        (b-val (f b))) 
    (cond
      ((and (negative? a-val) 
            (positive? b-val)) 
       (search f a b))
      ((and (positive? a-val) 
            (negative? b-val)) 
       (search f b a))
      (else 
        (error "Values are not of opposite signs" a b)))))
