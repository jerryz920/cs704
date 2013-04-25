(define (main x)
   (call process x 0) )

(define (process x y)
   (if (> x 0)
      (call process (- x 1) (+ y 1)) y)
     )
