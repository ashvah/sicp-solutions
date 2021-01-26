#lang sicp

; a) iterative
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

; b) recursive
(define (accumulate-re combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate-re combiner null-value term (next a) next b))))

; product
(define (product-re term a next b)
  (accumulate-re * 1 term a next b))

; sum
(define (sum-re term a next b)
  (accumulate-re + 0 term a next b))

(define (factorial-re n)
  (define (inc x) (+ 1 x))
  (product-re identity 1 inc n))

(define (cube-sum-re n)
  (define (inc x) (+ 1 x))
  (define (cube x) (* x x x))
  (sum-re cube 1 inc 10))

(factorial-re 5)
; 120
(factorial-re 6)
; 720
(factorial-re 7)
; 5040
(cube-sum-re 10)
; 3025
