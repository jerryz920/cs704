(define (main x) (call f x 0))
(define (f x y)  (if (> x 0) (call f (-x 1)(+ y 1))
	                     y
                 )
)

 