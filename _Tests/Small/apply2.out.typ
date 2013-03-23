(((lambda (f) (lambda (y) (y f))) 17) (lambda (x) ((+ x) 6)))
((lambda (f)
                                                                 (lambda (y)
                                                                   (y f)))
                                                                17)
(lambda
                                                                    (f)
                                                                    (lambda
                                                                    (y)
                                                                    (y f)))

(lambda (y) (y f))
(y f)
y
f
begin
't2 typ1 ('t1-> 't3) typ2
####
end
17
begin

('t1-> (('t1-> 't3)-> ('t1-> 't3))) typ1 (int-> 't4) typ2
####
't1 typ1 
int typ2
####
(('t1-> 't3)-> ('t1-> 't3)) typ1 't4 typ2
####
't4 typ1 
(('t1-> 't3)-> ('t1-> 't3)) typ2
####
end
(lambda (x) ((+ x) 6))
((+ x) 6)

(+ x)
+
x
begin
(int-> (int-> int)) typ1 ('t5-> 't6) typ2
####
int typ1 
't5 typ2
####
't5 typ1 int typ2
####
(int-> int) typ1 't6 typ2
####
't6 typ1 
(int-> int) typ2
####
end
6
begin
(int-> (int-> int)) typ1 (int-> 't7) typ2
####

int typ1 int typ2
####
(int-> int) typ1 't7 typ2
####
't7 typ1 (int-> int) typ2
####
end
begin

(int-> ((int-> 't3)-> (int-> 't3))) typ1 ((int-> (int-> (int-> int)))-> 't8) typ2
####

int typ1 (int-> (int-> (int-> int))) typ2
####
int (int->
                                                     (int-> (int-> int)))
####
Unhandled exception:
