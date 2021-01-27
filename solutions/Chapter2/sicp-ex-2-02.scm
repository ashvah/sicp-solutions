#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point z)
  (car z))

(define (y-point z)
  (cdr z))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (midpoint-segment l)
  (make-point (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2.0)
              (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2.0)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define a (make-point 1 5))
(define b (make-point 3 -8))
(print-point a)
; (1,5)
(print-point b)
; (3,-8)
(define l (make-segment a b))
(define mid (midpoint-segment l))
(print-point mid)
; (2.0,-1.5)
