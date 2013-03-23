(let ((x (lambda (x) x))) (x 3))
(lambda (x) x)
x
(x 3)
x
3
begin
('t2-> 't2) typ1 
(int-> 't3) typ2
####
't2 typ1 int typ2
####
't2 typ1 't3 typ2
####
int typ1 
't3 typ2
####
't3 typ1 int typ2
####
end
(int-> int)
