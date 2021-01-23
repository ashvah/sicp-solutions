#lang sicp
(define (<= x y) (or (< x y) (= x y)))
(define (sumOfLargestTwoSquared x y z)
  (cond ((and (<= x y) (<= x z)) (+ (* y y) (* z z)))
        ((and (<= y x) (<= y z)) (+ (* x x) (* z z)))
        ((and (<= z y) (<= z x)) (+ (* y y) (* x x)))))
(sumOfLargestTwoSquared 1 2 3) 
; 13 
(sumOfLargestTwoSquared 1 1 1) 
; 2 
(sumOfLargestTwoSquared 1 2 2) 
; 8 
(sumOfLargestTwoSquared 1 1 2) 
; 5 
(sumOfLargestTwoSquared 1 4 3) 
;25

;I used to forget to consider the case of equality and wrote the following error code:
;(define (sumOfLargestTwoSquared x y z)
;  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
;        ((and (< y x) (< y z)) (+ (* x x) (* z z)))
;        ((and (< z y) (< z x)) (+ (* y y) (* x x)))))
