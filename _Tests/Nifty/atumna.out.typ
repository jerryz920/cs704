(let ((atumna (lambda (x) (lambda (y) (lambda (z) x)))))
  (let ((eiecum (lambda (x) (lambda (y) (lambda (z) y)))))
    (lambda (a)
      (lambda (b) (lambda (c) (if a (((atumna b) c) a) (((eiecum c) b) a)))))))

(lambda (x) (lambda (y) (lambda (z) x)))
(lambda (y) (lambda (z) x))

(lambda (z) x)
x
(let ((eiecum (lambda (x) (lambda (y) (lambda (z) y)))))
                   (lambda (a)
                     (lambda (b)
                       (lambda (c)
                         (if a (((atumna b) c) a) (((eiecum c) b) a))))))

(lambda (x) (lambda (y) (lambda (z) y)))
(lambda (y) (lambda (z) y))

(lambda (z) y)
y
(lambda (a)
                   (lambda (b)
                     (lambda (c)
                       (if a (((atumna b) c) a) (((eiecum c) b) a)))))

(lambda (b) (lambda (c) (if a (((atumna b) c) a) (((eiecum c) b) a))))

(lambda (c) (if a (((atumna b) c) a) (((eiecum c) b) a)))
(if a
                                                            (((atumna b) c)
                                                              a)
                                                            (((eiecum c) b)
                                                              a))
a
't7 typ1 
bool typ2
####
(((atumna b) c) a)
((atumna b) c)
(atumna b)
atumna
b
begin

('t13-> ('t12-> ('t11-> 't13))) typ1 ('t8-> 't14) typ2
####
't13 typ1 
't8 typ2
####
('t12-> ('t11-> 't13)) typ1 't14 typ2
####
't14 typ1 ('t12->
                                                                    ('t11->
                                                                    't13)) typ2
####
end
c
begin

('t8-> ('t12-> ('t11-> 't8))) typ1 ('t9-> 't15) typ2
####
't8 typ1 't9 typ2
####

('t12-> ('t11-> 't8)) typ1 't15 typ2
####
't15 typ1 ('t12-> ('t11-> 't8)) typ2
####
end
a
begin

('t9-> ('t12-> ('t11-> 't9))) typ1 (bool-> 't16) typ2
####
't9 typ1 bool typ2
####

('t12-> ('t11-> 't9)) typ1 't16 typ2
####
't16 typ1 ('t12-> ('t11-> 't9)) typ2
####
end

(((eiecum c) b) a)
((eiecum c) b)
(eiecum c)
eiecum
c
begin
('t20->
                                                              ('t19->
                                                                ('t18-> 't19))) typ1 
(bool-> 't21) typ2
####
't20 typ1 bool typ2
####
('t19-> ('t18-> 't19)) typ1 
't21 typ2
####
't21 typ1 ('t19-> ('t18-> 't19)) typ2
####
end
b
begin

(bool-> ('t19-> ('t18-> 't19))) typ1 (bool-> 't22) typ2
####
bool typ1 
bool typ2
####
('t19-> ('t18-> 't19)) typ1 't22 typ2
####
't22 typ1 ('t19->
                                                                    ('t18->
                                                                    't19)) typ2
####
end
a
begin

(bool-> ('t19-> ('t18-> 't19))) typ1 (bool-> 't23) typ2
####
bool typ1 
bool typ2
####
('t19-> ('t18-> 't19)) typ1 't23 typ2
####
't23 typ1 ('t19->
                                                                    ('t18->
                                                                    't19)) typ2
####
end

(bool-> ('t12-> ('t11-> bool))) typ1 (bool-> ('t19-> ('t18-> 't19))) typ2
####

bool typ1 bool typ2
####
('t12-> ('t11-> bool)) typ1 ('t19-> ('t18-> 't19)) typ2
####

't12 typ1 't19 typ2
####
('t11-> bool) typ1 ('t18-> 't19) typ2
####
't11 typ1 
't18 typ2
####
bool typ1 't19 typ2
####
't19 typ1 bool typ2
####
(bool->
                                                                   (bool->
                                                                    (bool->
                                                                    (bool->
                                                                    (bool->
                                                                    ('t18->
                                                                    bool))))))
