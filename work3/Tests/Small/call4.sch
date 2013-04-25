(define (main a) (call rezrov 3 4 a))
(define (rezrov f g h) (if (< f g) (call zifmia f g) (call zifmia f h)))
(define (zifmia x y) (if (= 0 x) y (call rezrov (- x 1) (* 2 x) (* 2 y))))