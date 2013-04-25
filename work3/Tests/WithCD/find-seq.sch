(define (main list) (call search (quote (a a b a c)) list))

(define (search target input)
  (if (nil? input) false
      (if (call match target input)
          true
          (call search target (cdr input)) )))

(define (match target input)
  (if (nil? target) true
      (if (nil? input) false
          (call match (cdr target) (cdr input)) )))
      