(define (main x) (call f x 1))

(define (f x y)  (if (> x (call f (-x 1)(+ y 1)))
                     1
	             2
                 )
)

