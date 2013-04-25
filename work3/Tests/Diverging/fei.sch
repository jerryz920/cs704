(define (main x) (call proc x 0))
(define (proc x y) (if (< x 0) (call proc (- x 1) (+ y 1)) y))

