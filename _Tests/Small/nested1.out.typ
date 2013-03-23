((((lambda (x) (lambda (y) (lambda (x) x))) true) 'a) 22)
(((lambda (x)
                                                              (lambda (y)
                                                                (lambda (x)
                                                                  x))) true)
                                                            'a)
((lambda (x)
                                                                   (lambda
                                                                    (y)
                                                                    (lambda
                                                                    (x) x)))
                                                                  true)

(lambda (x) (lambda (y) (lambda (x) x)))
(lambda (y) (lambda (x) x))

(lambda (x) x)
x
true
begin
('t1-> ('t2-> ('t3-> 't3))) typ1 (bool-> 't4) typ2
####

't1 typ1 bool typ2
####
('t2-> ('t3-> 't3)) typ1 't4 typ2
####
't4 typ1 
('t2-> ('t3-> 't3)) typ2
####
end
'a
begin
(bool-> ('t2-> ('t3-> 't3))) typ1 
(sym-> 't5) typ2
####
bool typ1 sym typ2
####
('t2-> ('t3-> 't3)) typ1 
't5 typ2
####
't5 typ1 ('t2-> ('t3-> 't3)) typ2
####
end
22
begin
(sym->
                                                                    ('t2->
                                                                    ('t3->
                                                                    't3))) typ1 
(int-> 't6) typ2
####
sym typ1 int typ2
####
('t2-> ('t3-> 't3)) typ1 
't6 typ2
####
't6 typ1 ('t2-> ('t3-> 't3)) typ2
####
end
(int->
                                                           ('t2->
                                                             ('t3-> 't3)))
