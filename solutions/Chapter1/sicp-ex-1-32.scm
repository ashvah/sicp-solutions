#lang sicp

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; product
(define (product term a next b)
  (accumulate * 1 term a next b))

; sum
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (factorial n)
  (define (inc x) (+ 1 x))
  (product identity 1 inc n))

(define (cube-sum n)
  (define (inc x) (+ 1 x))
  (define (cube x) (* x x x))
  (sum cube 1 inc 10))

(factorial 5)
; 120
(factorial 6)
; 720
(factorial 7)
; 5040
(cube-sum 10)
; 3025
