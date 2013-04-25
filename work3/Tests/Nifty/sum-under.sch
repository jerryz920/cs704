(define (main n) 
  ( - (+ (call sumunder n 3) (call sumunder n 5)) (call sumunder n 15)))

(define (sumunder n k) (call sumbetween 0 k (+ k 2)))

(define (sumbetween start step end)
  (if (< start end)
      (+ start (call sumbetween (+ start step) step end))
      0 ))
