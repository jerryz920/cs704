((lambda (a) ((== a) 2)) 3)
(lambda (a) ((== a) 2))
((== a) 2)
(== a)
==
a
begin

(int-> (int-> bool)) typ1 ('t1-> 't2) typ2
####
int typ1 't1 typ2
####

't1 typ1 int typ2
####
(int-> bool) typ1 't2 typ2
####
't2 typ1 (int-> bool) typ2
####
end
2
begin

(int-> (int-> bool)) typ1 (int-> 't3) typ2
####
int typ1 int typ2
####

(int-> bool) typ1 't3 typ2
####
't3 typ1 (int-> bool) typ2
####
end
3
begin

(int-> (int-> (int-> bool))) typ1 (int-> 't4) typ2
####
int typ1 int typ2
####

(int-> (int-> bool)) typ1 't4 typ2
####
't4 typ1 (int-> (int-> bool)) typ2
####
end

(int-> (int-> (int-> bool)))
