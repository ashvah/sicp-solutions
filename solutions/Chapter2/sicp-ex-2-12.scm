#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (max (car z) (cdr z)))

(define (lower-bound z)
  (min (car z) (cdr z)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))

(define (percent i)
  (/ (width i) (center i)))

(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]\n"))

(define a (make-interval 5 9))
(define b (make-center-width 5 0.01))
(define c (make-center-percent 5 0.01))
(print-interval a)
; [5, 9]
(print-interval b)
; [4.99, 5.01]
(print-interval c)
; [4.95, 5.05]
(width c)
; 0.04999999999999982
(percent b)
; 0.0019999999999999575
