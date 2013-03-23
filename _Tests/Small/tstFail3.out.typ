(lambda (f) (lambda (g) (if ((< f) g) (f 1) (g 2))))
(lambda (g)
                                                       (if ((< f) g) 
                                                         (f 1) (g 2)))

(if ((< f) g) (f 1) (g 2))
((< f) g)
(< f)
<
f
begin
(int-> (int-> bool)) typ1 
('t1-> 't3) typ2
####
int typ1 't1 typ2
####
't1 typ1 int typ2
####
(int->
                                                                    bool) typ1 
't3 typ2
####
't3 typ1 (int-> bool) typ2
####
end
g
begin
(int->
                                                            (int-> bool)) typ1 
('t2-> 't4) typ2
####
int typ1 't2 typ2
####
't2 typ1 int typ2
####
(int->
                                                                    bool) typ1 
't4 typ2
####
't4 typ1 (int-> bool) typ2
####
end
(int-> (int-> bool)) typ1 
bool typ2
####
(int-> (int-> bool)) bool
####
Unhandled exception:
