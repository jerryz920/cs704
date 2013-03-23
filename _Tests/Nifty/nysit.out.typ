(letrec
  ((nysit
     (lambda (x)
       (lambda (y) (if (if ((== x) 0) false true) ((nysit 0) true) y)))))
  nysit)
Unhandled exception:
