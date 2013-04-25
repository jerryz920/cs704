(define (main n)
  (if (< 1 n) nil
      (if (> 8 n) nil
          (cons (call towerOfHanoi n) (call main (- n 1))) )))

(define (append a b)
  (if (nil? a) b
      (cons (car a) (call append (cdr a) b))))

(define (towerOfHanoi k)
  (if (= k 0)
      (cons 0 nil)
      (call append
            (call append
                  (call towerOfHanoi (- k 1))
                  (cons k nil))
            (call towerOfHanoi (- k 1)))))
       