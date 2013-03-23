(let ((x 22)) (let ((x false)) (let ((x nil)) x)))
22
(let ((x false))
                                                        (let ((x nil)) x))
false

(let ((x nil)) x)
nil
x
['t1]
