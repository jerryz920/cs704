(define (main n) (call shift n 0))
(define (shift x y)
  (if (= x 0) y
    (call shift (- x 1) (+ y 1))))
