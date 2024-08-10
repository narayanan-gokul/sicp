(define (make-rat n d)
  (define (helper n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (if (< d 0)
    (helper (- n) (- d))
    (helper n d)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (add-rat x y)
  (let
    ((nx (numer x))
     (ny (numer y))
     (dx (denom x))
     (dy (denom y)))
    (make-rat (+ (* nx dy) (* ny dx)) (* dx dy))))

(define (mult-rat x y)
  (let
    ((nx (numer x))
     (ny (numer y))
     (dx (denom x))
     (dy (denom y)))
    (make-rat (* nx ny) (* dx dy))))
