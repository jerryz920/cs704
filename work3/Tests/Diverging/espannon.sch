(define (main a b) (call vermicelli a b 0))
(define (vermicelli c d e) (if (> c 0) (call penne e 0 2) 5))
(define (penne f g h) (call vermicelli  g h (+ f 1)))
