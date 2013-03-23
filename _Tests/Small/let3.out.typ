(let ((and (lambda (p) (lambda (q) (if p q false)))))
  (let ((or (lambda (p) (lambda (q) (if p true q)))))
    (let ((xor (lambda (p) (lambda (q) (if p (if q false true) q)))))
      ((cons and) ((cons or) ((cons xor) nil))))))
(lambda (p)
                                                     (lambda (q)
                                                       (if p q false)))

(lambda (q) (if p q false))
(if p q false)
p
't1 typ1 bool typ2
####
q
false

't2 typ1 bool typ2
####
(let ((or (lambda (p) (lambda (q) (if p true q)))))
                          (let
                            ((xor
                               (lambda (p)
                                 (lambda (q) (if p (if q false true) q)))))
                            ((cons and) ((cons or) ((cons xor) nil)))))

(lambda (p) (lambda (q) (if p true q)))
(lambda (q) (if p true q))
(if p true
                                                                    q)
p

't3 typ1 bool typ2
####
true
q
bool typ1 't4 typ2
####
't4 typ1 bool typ2
####

(let ((xor (lambda (p) (lambda (q) (if p (if q false true) q)))))
  ((cons and) ((cons or) ((cons xor) nil))))
(lambda (p)
                                               (lambda (q)
                                                 (if p (if q false true) q)))

(lambda (q) (if p (if q false true) q))
(if p (if q false true) q)
p

't5 typ1 bool typ2
####
(if q false true)
q
't6 typ1 bool typ2
####
false
true

bool typ1 bool typ2
####
q
bool typ1 bool typ2
####
((cons and)
                                                      ((cons or)
                                                        ((cons xor) nil)))

(cons and)
cons
and
begin
('t7-> (['t7]-> ['t7])) typ1 ((bool->
                                                          (bool-> bool))->
                                                         't8) typ2
####

't7 typ1 (bool-> (bool-> bool)) typ2
####
(['t7]-> ['t7]) typ1 't8 typ2
####

't8 typ1 (['t7]-> ['t7]) typ2
####
end
((cons or) ((cons xor) nil))
(cons or)
cons
or
begin

('t9-> (['t9]-> ['t9])) typ1 ((bool-> (bool-> bool))-> 't10) typ2
####

't9 typ1 (bool-> (bool-> bool)) typ2
####
(['t9]-> ['t9]) typ1 't10 typ2
####

't10 typ1 (['t9]-> ['t9]) typ2
####
end
((cons xor) nil)
(cons xor)
cons
xor
begin

('t11-> (['t11]-> ['t11])) typ1 ((bool-> (bool-> bool))-> 't12) typ2
####

't11 typ1 (bool-> (bool-> bool)) typ2
####
(['t11]-> ['t11]) typ1 't12 typ2
####

't12 typ1 (['t11]-> ['t11]) typ2
####
end
nil
begin
((bool-> (bool-> bool))->
                                                      ([(bool->
                                                          (bool-> bool))]->
                                                        [(bool->
                                                           (bool-> bool))])) typ1 
(['t13]-> 't14) typ2
####
(bool-> (bool-> bool)) typ1 ['t13] typ2
####

(bool-> (bool-> bool)) ['t13]
####
Unhandled exception:
