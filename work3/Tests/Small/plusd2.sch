(define (main x)
  (call foo true x)
)

(define (foo x y)
  (+ (if x 5 y) 2)
)