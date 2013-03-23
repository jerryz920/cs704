(lambda (a)
  (lambda (b) (lambda (c) (if ((< a) b) (lambda (x) c) (lambda (y) 'fnord)))))

(lambda (b) (lambda (c) (if ((< a) b) (lambda (x) c) (lambda (y) 'fnord))))

(lambda (c) (if ((< a) b) (lambda (x) c) (lambda (y) 'fnord)))
(if ((< a) b)
                                                                 (lambda (x)
                                                                   c)
                                                                 (lambda (y)
                                                                   'fnord))

((< a) b)
(< a)
<
a
begin
(int-> (int-> bool)) typ1 ('t1-> 't4) typ2
####

int typ1 't1 typ2
####
't1 typ1 int typ2
####
(int-> bool) typ1 't4 typ2
####

't4 typ1 (int-> bool) typ2
####
end
b
begin
(int-> (int-> bool)) typ1 
('t2-> 't5) typ2
####
int typ1 't2 typ2
####
't2 typ1 int typ2
####
(int->
                                                                    bool) typ1 
't5 typ2
####
't5 typ1 (int-> bool) typ2
####
end
(int-> (int-> bool)) typ1 
bool typ2
####
(int-> (int-> bool)) bool
####
Unhandled exception:
