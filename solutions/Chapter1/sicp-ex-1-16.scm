#lang sicp
(define (square x) (* x x))

(define (expt b n)
  (define (fast-expt a b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= n 0) a)
        ((even? n) (fast-expt a (square b) (/ n 2)))
        (else (fast-expt (* a b) b (- n 1)))))
  (fast-expt 1 b n))

(expt 2 16)
