(lambda (a)
  (lambda (b) (lambda (c) (lambda (d) (if ((== a) 'one) b ((cons c) d))))))

(lambda (b) (lambda (c) (lambda (d) (if ((== a) 'one) b ((cons c) d)))))

(lambda (c) (lambda (d) (if ((== a) 'one) b ((cons c) d))))
(lambda (d)
                                                              (if
                                                                ((== a) 'one)
                                                                b
                                                                ((cons c) d)))

(if ((== a) 'one) b ((cons c) d))
((== a) 'one)
(== a)
==
a
begin
(int->
                                                                    (int->
                                                                    bool)) typ1 
('t1-> 't5) typ2
####
int typ1 't1 typ2
####
't1 typ1 int typ2
####
(int->
                                                                    bool) typ1 
't5 typ2
####
't5 typ1 (int-> bool) typ2
####
end
'one
begin
(int->
                                                               (int-> bool)) typ1 
(sym-> 't6) typ2
####
int typ1 sym typ2
####
(int-> bool) typ1 't6 typ2
####

't6 typ1 (int-> bool) typ2
####
end
(sym-> (int-> bool)) typ1 bool typ2
####

(sym-> (int-> bool)) bool
####
Unhandled exception:
