(lambda (c) (lambda (d) ((cons c) d)))
(lambda (d) ((cons c) d))
((cons c) d)

(cons c)
cons
c
begin
('t3-> (['t3]-> ['t3])) typ1 ('t1-> 't4) typ2
####

't3 typ1 't1 typ2
####
(['t3]-> ['t3]) typ1 't4 typ2
####
't4 typ1 (['t3]->
                                                                    ['t3]) typ2
####
end
d
begin

('t1-> (['t1]-> ['t1])) typ1 ('t2-> 't5) typ2
####
't1 typ1 't2 typ2
####

(['t1]-> ['t1]) typ1 't5 typ2
####
't5 typ1 (['t1]-> ['t1]) typ2
####
end

('t2-> ('t2-> ('t2-> (['t2]-> ['t2]))))
