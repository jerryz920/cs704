(define (collatz n)
  (if
   (= n 1) (cons 1 nil)
   (if
    (= 0 (mod n 2))
    (cons n (call collatz (div n 2)))
    (cons n (call collatz (+ 1 (* n 3)))))))

(define (main) (call collatz 27))
