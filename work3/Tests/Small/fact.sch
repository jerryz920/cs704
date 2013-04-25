(define (fact n)
  (if (< n 1) 1
      (* n (call fact (- n 1))) ))

(define (main) (call fact 7))