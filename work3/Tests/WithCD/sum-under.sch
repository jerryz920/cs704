(define (main n) 
  ( - (+ (call sumunder n 3) (call sumunder n 5)) (call sumunder n 15)))

(define (sumunder n k) (call sumbetween (- n n) k n))
(define (sumbetween start step end)
  (if (< start end)
      (+ start (call sumbetween (+ start step) step end))
      0 ))
