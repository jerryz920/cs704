(let ((x (lambda (foo) ((+ foo) 3))))
  (let ((y (lambda (bar) ((* bar) 2)))) (lambda (z) (x (y (x (y z)))))))

(lambda (foo) ((+ foo) 3))
((+ foo) 3)
(+ foo)
+
foo
begin
(int->
                                                             (int-> int)) typ1 
('t1-> 't2) typ2
####
int typ1 't1 typ2
####
't1 typ1 int typ2
####
(int->
                                                                    int) typ1 
't2 typ2
####
't2 typ1 (int-> int) typ2
####
end
3
begin
(int-> (int-> int)) typ1 
(int-> 't3) typ2
####
int typ1 int typ2
####
(int-> int) typ1 't3 typ2
####

't3 typ1 (int-> int) typ2
####
end
(let ((y (lambda (bar) ((* bar) 2))))
                                     (lambda (z) (x (y (x (y z))))))

(lambda (bar) ((* bar) 2))
((* bar) 2)
(* bar)
*
bar
begin
(int->
                                                             (int-> int)) typ1 
('t4-> 't5) typ2
####
int typ1 't4 typ2
####
't4 typ1 int typ2
####
(int->
                                                                    int) typ1 
't5 typ2
####
't5 typ1 (int-> int) typ2
####
end
2
begin
(int-> (int-> int)) typ1 
(int-> 't6) typ2
####
int typ1 int typ2
####
(int-> int) typ1 't6 typ2
####

't6 typ1 (int-> int) typ2
####
end
(lambda (z) (x (y (x (y z)))))
(x
                                                                    (y
                                                                    (x (y z))))
x

(y (x (y z)))
y
(x (y z))
x
(y z)
y
z
begin
(int-> (int-> (int-> int))) typ1 
('t7-> 't8) typ2
####
int typ1 't7 typ2
####
't7 typ1 int typ2
####
(int->
                                                                    (int->
                                                                    int)) typ1 
't8 typ2
####
't8 typ1 (int-> (int-> int)) typ2
####
end
begin
(int->
                                                                 (int->
                                                                   (int->
                                                                    int))) typ1 
((int-> (int-> (int-> int)))-> 't9) typ2
####
int typ1 (int->
                                                         (int-> (int-> int))) typ2
####

int (int-> (int-> (int-> int)))
####
Unhandled exception:
