(define (pascal row col)
  (if (or (= row col) (= 1 col) (= 1 row))
    1
    (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))
