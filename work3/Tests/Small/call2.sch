(define (main a) (call rezrov 3 4 5))
(define (rezrov f g h) (if (< f g) (call zifmia f g) (call zifmia f h)))
(define (zifmia x y) (if (= 0 x) 23 (call rezrov (- x 1) y (* 2 y))))