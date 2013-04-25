(define (nth n list)
  (if (= n 0)
      (car list)
      (call nth (- n 1) (cdr list))))

(define (main list)
  (cons
   (call nth 3 list)
   (call nth 7 list) ))
  
  