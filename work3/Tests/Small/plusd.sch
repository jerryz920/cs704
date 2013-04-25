(define (main x)
  (call foo false x)
)

(define (foo x y)
  (+ (if x 5 y) 2)
)