#lang sicp
(define (* a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (else (+ (* a (- b 1)) a))))
