#lang sicp

;----------------------------------------
; definition
(define zero
  (lambda (f x) x))

(define (succ n)
  (lambda (f x)
    (f (n f x))))

(define (add a b)
  (lambda (f x)
    (a f (b f x))))

(define (pred n)
  (lambda (f x)
    ((n (lambda (g) (lambda (h) (h (g f))))
        (lambda (u) x))
     (lambda (u) u))))

(define (sub a b)
  (b pred a))

(define (mul a b)
  (lambda (f x)
    (a (lambda (x) (b f x)) x)))


;----------------------------------------
; test

(zero inc 0)
; 0 
(define five (add (succ (succ (succ zero))) (succ (succ zero))))
(five inc 0)
; 5
(define four (pred five))
(four inc 0)
; 4
(define one (sub five four))
(one inc 0)
; 1
(define twenty (mul four five))
(twenty inc 0)
; 20
