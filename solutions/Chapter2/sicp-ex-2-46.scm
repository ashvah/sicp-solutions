#lang sicp

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (car (cdr v)))

(define (add-vect v1 v2)
  (map + v1 v2))

(define (sub-vect v1 v2)
  (map - v1 v2))

(define (scale-vect s v)
  (map (lambda (x) (* x s)) v))


(define v1 (make-vect 1 2))
(define v2 (make-vect 3 -9))
(xcor-vect v1)
; 1
(ycor-vect v2)
; -9
(add-vect v1 v2)
; (4 -7)
(sub-vect v1 v2)
; (-2 11)
(scale-vect 0.5 v2)
; (1.5 -4.5)
