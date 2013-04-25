(define (main n)
  (call upTo 0 3))

(define (upTo k n)
  (if (< k n)
      (if (> k 8) nil
          (cons (call towerOfHanoi k) (call upTo (+ 1 k) n)) )
      nil))

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
       