(lambda (w) (lambda (x) (lambda (y) (if true (x y) (w (x true))))))
(lambda
                                                                    (x)
                                                                    (lambda
                                                                    (y)
                                                                    (if true
                                                                    (x y)
                                                                    (w
                                                                    (x true)))))

(lambda (y) (if true (x y) (w (x true))))
(if true (x y) (w (x true)))
true

bool typ1 bool typ2
####
(x y)
x
y
begin
't2 typ1 ('t3-> 't4) typ2
####
end

(w (x true))
w
(x true)
x
true
begin
('t3-> 't4) typ1 (bool-> 't5) typ2
####

't3 typ1 bool typ2
####
't4 typ1 't5 typ2
####
end
begin
't1 typ1 ((bool->
                                                                    't5)->
                                                                    't6) typ2
####
end

(bool-> 't5) typ1 ((bool-> 't5)-> 't6) typ2
####
bool typ1 (bool-> 't5) typ2
####

bool (bool-> 't5)
####
Unhandled exception:
