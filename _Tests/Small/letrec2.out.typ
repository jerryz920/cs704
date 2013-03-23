(letrec ((length (lambda (x) (if ((== x) nil) 0 (length (cdr x))))))
  (length nil))
Unhandled exception:
