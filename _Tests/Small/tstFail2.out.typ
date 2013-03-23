(lambda (x) (let ((val (car x))) (if val val 3)))
(let ((val (car x)))
                                                    (if val val 3))
(car x)
car
x
begin

(['t2]-> 't2) typ1 ('t1-> 't3) typ2
####
['t2] typ1 't1 typ2
####
't1 typ1 
['t2] typ2
####
't2 typ1 't3 typ2
####
end
(if val val 3)
val
(['t4]-> 't4) typ1 
bool typ2
####
(['t4]-> 't4) bool
####
Unhandled exception:
