#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define a zero)
; (lambda (f) (lambda (x) x))
(define b (add-1 a))
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; (lambda (f) (lambda (x) (f x)))
(define c (add-1 b))
; (lambda (f) (lambda (x) (f ((b f) x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (plus a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

; for example
((zero inc) 0)
; 0
((one inc) 0)
; 1
((two inc) 0)
(define three (plus one two))
; 2
((three inc) 0)
; 3
(define eight (plus three (plus two three)))
((eight inc) 0)
; 8
(((three eight) inc) 0)
; 8^3=512
