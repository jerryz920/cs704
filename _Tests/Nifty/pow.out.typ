(letrec
  ((pow
     (lambda (b)
       (lambda (exp)
         (if ((== 0) exp) 1
           (if ((== 1) exp) b
             (let ((a ((pow b) ((div exp) 2))))
               (if ((== 0) ((mod exp) 2)) ((* a) a) ((* b) ((* a) a))))))))))
  pow)
Unhandled exception:
