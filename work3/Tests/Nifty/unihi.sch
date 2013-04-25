(define (main a) 
  (call foldSum 0
        (call mulPairs
              (quote (23 42 68 39 20 19))
              (cons a (quote (19 24 -73 16 34))) )))

(define (foldSum acc l)
  (if (nil? l) acc
      (call foldSum (+ (car l) acc) (cdr l)) ))

(define (mulPairs list1 list2)
  (if (nil? list1) nil
      (cons (* (car list1) (car list2)) (call mulPairs (cdr list1) (cdr list2))) ))
  