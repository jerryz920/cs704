(letrec
  ((length
     (lambda (x) (if ((== x) nil) 0 ((lambda (length) ((+ 1) length)) 22)))))
  (length nil))
Unhandled exception:
