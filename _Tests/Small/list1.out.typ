((cons 1) ((cons 2) ((cons 3) nil)))
(cons 1)
cons
1
begin
('t1->
                                                             (['t1]-> ['t1])) typ1 
(int-> 't2) typ2
####
't1 typ1 int typ2
####
(['t1]-> ['t1]) typ1 't2 typ2
####

't2 typ1 (['t1]-> ['t1]) typ2
####
end
((cons 2) ((cons 3) nil))
(cons 2)
cons
2
begin

('t3-> (['t3]-> ['t3])) typ1 (int-> 't4) typ2
####
't3 typ1 int typ2
####

(['t3]-> ['t3]) typ1 't4 typ2
####
't4 typ1 (['t3]-> ['t3]) typ2
####
end

((cons 3) nil)
(cons 3)
cons
3
begin
('t5-> (['t5]-> ['t5])) typ1 (int-> 't6) typ2
####

't5 typ1 int typ2
####
(['t5]-> ['t5]) typ1 't6 typ2
####
't6 typ1 (['t5]->
                                                                    ['t5]) typ2
####
end
nil
begin

(int-> ([int]-> [int])) typ1 (['t7]-> 't8) typ2
####
int typ1 ['t7] typ2
####

int ['t7]
####
Unhandled exception:
