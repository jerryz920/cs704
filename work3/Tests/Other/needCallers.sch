(define (main x) (+ (call foo 1) (call bar x)))

(define (foo a) (call f (call g 0)))

(define (bar x) (call g x))

(define (f a) a)

(define (g x) 20)