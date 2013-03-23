(let ((s (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))) s)
(lambda
                                                                    (x)
                                                                    (lambda
                                                                    (y)
                                                                    (lambda
                                                                    (z)
                                                                    ((x z)
                                                                    (y z)))))

(lambda (y) (lambda (z) ((x z) (y z))))
(lambda (z) ((x z) (y z)))
((x z)
                                                                    (y z))

(x z)
x
z
begin
't1 typ1 ('t3-> 't4) typ2
####
end
(y z)
y
z
begin
't2 typ1 
('t3-> 't5) typ2
####
end
begin
('t3-> 't4) typ1 (('t3-> 't5)-> 't6) typ2
####

't3 typ1 ('t3-> 't5) typ2
####
Unhandled exception:
