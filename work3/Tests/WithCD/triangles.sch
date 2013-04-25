(define (main n)
  (if (= n 0)
      0
      (+ (call main (- n 1)) (call tri 6))))

(define (tri k)
  (if (= k 0) 0
      (+ k (call tri (- k 1)))))