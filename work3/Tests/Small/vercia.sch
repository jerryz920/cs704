(define (main n) (call frotz n 3 4))
(define (frotz a b c) 
  (if (= b 0)
      (if (< b c) (call thrice a) (call twice c))
      (call frotz (* c a) (- b 1) (call twice b)) ))
  
(define (twice x) (* 2 x))
(define (thrice x) (* 3 x))