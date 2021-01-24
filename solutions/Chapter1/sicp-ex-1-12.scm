#lang sicp
(define (triangle m n)
  (cond ((= m 1) 1)
        ((= n 1) 1)
        ((= n m) 1)
        ((< n 0) 0)
        ((> n m) 0)
        (else (+ (triangle (- m 1) (- n 1))
                 (triangle (- m 1) n)))))
