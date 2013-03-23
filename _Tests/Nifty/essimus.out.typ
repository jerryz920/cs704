(letrec
  ((divit
     (lambda (a)
       (lambda (b)
         (lambda (c) (lambda (d) (lambda (e) (if ((> (a e)) 0) e 'Vitates))))))))
  (letrec
    ((essimus
       (lambda (a)
         (lambda (b) (lambda (c) (lambda (d) (lambda (e) (car c))))))))
    (letrec
      ((haessum
         (lambda (a)
           (lambda (b)
             (lambda (c)
               (lambda (d) (lambda (e) (((((haessum b) a) c) (cdr d)) e))))))))
      ((cons divit) ((cons essimus) ((cons haessum) nil))))))
Unhandled exception:
