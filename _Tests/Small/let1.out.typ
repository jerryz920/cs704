(let ((x (lambda (foo) ((+ foo) 3))))
  (let ((y (lambda (bar) ((* bar) 2)))) (x (y (x (y 3))))))
(lambda (foo)
                                                              ((+ foo) 3))

((+ foo) 3)
(+ foo)
+
foo
begin
(int-> (int-> int)) typ1 ('t1-> 't2) typ2
####

int typ1 't1 typ2
####
't1 typ1 int typ2
####
(int-> int) typ1 't2 typ2
####

't2 typ1 (int-> int) typ2
####
end
3
begin
(int-> (int-> int)) typ1 (int->
                                                                    't3) typ2
####

int typ1 int typ2
####
(int-> int) typ1 't3 typ2
####
't3 typ1 (int-> int) typ2
####
end

(let ((y (lambda (bar) ((* bar) 2)))) (x (y (x (y 3)))))
(lambda (bar)
                                                           ((* bar) 2))

((* bar) 2)
(* bar)
*
bar
begin
(int-> (int-> int)) typ1 ('t4-> 't5) typ2
####

int typ1 't4 typ2
####
't4 typ1 int typ2
####
(int-> int) typ1 't5 typ2
####

't5 typ1 (int-> int) typ2
####
end
2
begin
(int-> (int-> int)) typ1 (int->
                                                                    't6) typ2
####

int typ1 int typ2
####
(int-> int) typ1 't6 typ2
####
't6 typ1 (int-> int) typ2
####
end

(x (y (x (y 3))))
x
(y (x (y 3)))
y
(x (y 3))
x
(y 3)
y
3
begin
(int->
                                                                  (int->
                                                                    (int->
                                                                    int))) typ1 
(int-> 't7) typ2
####
int typ1 int typ2
####
(int-> (int-> int)) typ1 
't7 typ2
####
't7 typ1 (int-> (int-> int)) typ2
####
end
begin
(int->
                                                                 (int->
                                                                   (int->
                                                                    int))) typ1 
((int-> (int-> (int-> int)))-> 't8) typ2
####
int typ1 (int->
                                                         (int-> (int-> int))) typ2
####

int (int-> (int-> (int-> int)))
####
Unhandled exception:
