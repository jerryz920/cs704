(let ((x 5)) (let ((x ((> 3) x))) (let ((x (if x 'Asinec 'Quius))) x)))
5

(let ((x ((> 3) x))) (let ((x (if x 'Asinec 'Quius))) x))
((> 3) x)
(> 3)
>
3
begin

(int-> (int-> bool)) typ1 (int-> 't1) typ2
####
int typ1 int typ2
####

(int-> bool) typ1 't1 typ2
####
't1 typ1 (int-> bool) typ2
####
end
x
begin

(int-> (int-> bool)) typ1 (int-> 't2) typ2
####
int typ1 int typ2
####

(int-> bool) typ1 't2 typ2
####
't2 typ1 (int-> bool) typ2
####
end
(let
                                                                    ((x
                                                                    (if x
                                                                    'Asinec
                                                                    'Quius)))
                                                                    x)

(if x 'Asinec 'Quius)
x
(int-> (int-> bool)) typ1 bool typ2
####
(int->
                                                                   (int->
                                                                    bool)) 
bool
####
Unhandled exception:
