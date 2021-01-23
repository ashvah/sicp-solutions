#lang sicp
(define (3-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (3-root-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square  guess)) (* 2  guess)) 3))

(define (3-root x)
  (3-root-iter 1.0 x))

(define (square x) (* x x))

(define (good-enough? guess-new guess-old)
  (= guess-new guess-old))

(3-root 4096)
; 16.0
(3-root 0.001)
; 0.09999999999999999
(3-root 0.0001)
; 0.04641588833612779
(3-root 10000000000000000000000000000000000000000000000)
; 2154434690031883.7
