;; The interval object

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (span-interval x)
  (- (upper-bound x) (lower-bound x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
		    (lower-bound y))
		 (+ (upper-bound x)
		    (upper-bound y))))

(define (sub-interval x y)
  (let
    ((d1 (- (lower-bound x) (lower-bound y)))
     (d2 (- (lower-bound x) (upper-bound y)))
     (d3 (- (upper-bound x) (lower-bound y)))
     (d4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min d1 d2 d3 d4) (max d1 d2 d3 d4))))

(define (mul-interval x y)
  (let
    ((p1 (* (lower-bound x) (lower-bound y)))
     (p2 (* (lower-bound x) (upper-bound y)))
     (p3 (* (upper-bound x) (lower-bound y)))
     (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (span-interval y) 0)
    (error "The interval spans 0" y)
    (mul-interval x
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))

(define (print-interval x)
  (display (lower-bound x))
  (display " - ")
  (display (upper-bound x))
  (newline))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x))
     2))

(define (make-center-percent c t)
  (let ((w (* c (/ t 100))))
    (make-interval (- c w) (+ c w))))

(define (percent x)
  (* 100 (/ (- (upper-bound x) (center x)) (center x))))


