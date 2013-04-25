(define (main vlist) (call find (quote ann) (quote (susan john ann)) vlist))

(define (find name list vList)
   (if (call match name (car list))
          (car vList)
          (call find name (cdr list) (cdr vList))
   )
)

(define (match v1 v2)
   (= v1 v2)
)

      