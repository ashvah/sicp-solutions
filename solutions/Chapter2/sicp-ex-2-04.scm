#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

; (define z (cons  x y))
; (define z (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))
; (car z)
; (z (lambda (p q) p))
; ((lambda (m) (m x y)) (lambda (p q) p))
; ((lambda (p q) p) x y)
; x

(define (cdr z)
  (z (lambda (p q) q)))
; (car z)
; (z (lambda (p q) q))
; ((lambda (m) (m x y)) (lambda (p q) q))
; ((lambda (p q) q) x y)
; y

(define z (cons 1 2))
(car z)
; 1
(cdr z)
; 2
