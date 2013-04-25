(define (main n)
  (call foo (- n (call tri 2)) (call tri 6))
)

(define (tri k)
  (if (= k 0) 0
      (+ k (call tri (- k 1)))))

(define (foo d s) (+ d s))