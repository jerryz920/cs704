(define (main)
  (if (= (quote (a . b)) (cons (quote a) (quote b)))
     1
     0
  )
)
