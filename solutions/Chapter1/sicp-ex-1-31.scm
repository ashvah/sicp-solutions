#lang sicp

; a) iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (inc x) (+ 1 x))
  (product identity 1 inc n))

(define (pi)
  (define (next x) (+ x 2))
  (define (f x) (* (/ x (+ 1 x)) (/ (+ 2 x) (+ 1 x))))
  (* 4.0 (product f 2 next 10000)))

(factorial 5)
; 120
(factorial 6)
; 720
(factorial 7)
; 5040
(pi)
; 3.1417497057380523

; b) recursive
(define (product-recursive term a next b)
    (if (> a b)
        0
        (* (term a) (product term (next a) next b))))

(define (factorial-recursive n)
  (define (inc x) (+ 1 x))
  (product-recursive identity 1 inc n))

(define (pi-recursive)
  (define (next x) (+ x 2))
  (define (f x) (* (/ x (+ 1 x)) (/ (+ 2 x) (+ 1 x))))
  (* 4.0 (product-recursive f 2 next 10000)))

(factorial-recursive 5)
; 120
(factorial-recursive 6)
; 720
(factorial-recursive 7)
; 5040
(pi-recursive)
; 3.1417497057380523
