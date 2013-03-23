(lambda (x) (lambda (y) (lambda (z) ((+ x) (if y z 5)))))
(lambda (y)
                                                            (lambda (z)
                                                              ((+ x)
                                                                (if y z 5))))

(lambda (z) ((+ x) (if y z 5)))
((+ x) (if y z 5))
(+ x)
+
x
begin
(int->
                                                                    (int->
                                                                    int)) typ1 
('t1-> 't4) typ2
####
int typ1 't1 typ2
####
't1 typ1 int typ2
####
(int->
                                                                    int) typ1 
't4 typ2
####
't4 typ1 (int-> int) typ2
####
end
(if y z 5)
y
't2 typ1 
bool typ2
####
z
5
't3 typ1 int typ2
####
begin
(int-> (int-> int)) typ1 
(int-> 't5) typ2
####
int typ1 int typ2
####
(int-> int) typ1 't5 typ2
####

't5 typ1 (int-> int) typ2
####
end
(int->
                                     (bool-> (int-> (int-> (int-> int)))))
