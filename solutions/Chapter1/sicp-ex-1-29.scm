#lang sicp
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (simpson f a b n)
  (define (add-dx x) (+ x (/ (- b a) n)))
  (define (add-dx-2 x) (+ x (double (/ (- b a) n))))
  (define (sub-dx x) (- x (/ (- b a) n)))
  (define (double x) (* 2.0 x))
  (define (quad x) (* 4.0 x))
  (define (term x) (+ (double (f x)) (quad (f (add-dx x)))))
  (* (/ (/ (- b a) n) 3.0)
     (+ (f a) (quad (f (add-dx a))) (sum term (add-dx-2 a) add-dx-2 (sub-dx b)) (f b))))

(integral cube 0 1 0.01)
; 0.24998750000000042
(integral cube 0 1 0.001)
; 0.249999875000001
(simpson cube 0 1 100)
; 0.2500000000000003
(simpson cube 0 1 1000)
; 0.2500000000000006
