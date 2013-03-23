(lambda (x) (lambda (y) (if true (if true (if true x y) y) (if true x y))))

(lambda (y) (if true (if true (if true x y) y) (if true x y)))
(if true
                                                                 (if true
                                                                   (if true x
                                                                    y) y)
                                                                 (if true x
                                                                   y))
true

bool typ1 bool typ2
####
(if true (if true x y) y)
true
bool typ1 bool typ2
####

(if true x y)
true
bool typ1 bool typ2
####
x
y
't1 typ1 't2 typ2
####
y

't2 typ1 't2 typ2
####
(if true x y)
true
bool typ1 bool typ2
####
x
y

't2 typ1 't2 typ2
####
't2 typ1 't2 typ2
####
('t2-> ('t2-> 't2))
