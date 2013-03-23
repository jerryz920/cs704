(let ((id (lambda (a) a)))
  (let ((audam (lambda (a) (lambda (b) (if (id a) ((+ b) b) (id b))))))
    audam))
(lambda (a) a)
a
(let
                               ((audam
                                  (lambda (a)
                                    (lambda (b) (if (id a) ((+ b) b) (id b))))))
                               audam)
(lambda (a)
                                        (lambda (b)
                                          (if (id a) ((+ b) b) (id b))))

(lambda (b) (if (id a) ((+ b) b) (id b)))
(if (id a) ((+ b) b) (id b))

(id a)
id
a
begin
('t4-> 't4) typ1 ('t2-> 't5) typ2
####
't4 typ1 't2 typ2
####

't4 typ1 't5 typ2
####
't2 typ1 't5 typ2
####
end
('t5-> 't5) typ1 bool typ2
####

('t5-> 't5) bool
####
Unhandled exception:
