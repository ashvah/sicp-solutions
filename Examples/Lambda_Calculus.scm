#lang sicp

;----------------------------------------
; definition of natural numbers

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

(define (display-nat n) (n inc 0))

; test

(display-nat zero)
; 0 
(define five (add (succ (succ (succ zero))) (succ (succ zero))))
(display-nat five)
; 5
(define four (pred five))
(display-nat four)
; 4
(define one (sub five four))
(display-nat one)
; 1
(define twenty (mul four five))
(display-nat twenty)
; 20

;------------------------------------------
; definition of boolean

(define true
  (lambda (t f)
    t))

(define false
  (lambda (t f)
    f))

(define (test e1 e2 e3)
  (e1 e2 e3))

(define (and a b)
  (a b false))

(define (or a b)
  (a true b))

(define (iszero? n)
  (n (lambda (f) false) true))

(define (equal? x y)
  (and (iszero? (sub  x y)) (iszero? (sub y x))))

(define (display-bool x) (test x 'true 'false))

; test

(display-bool (iszero? zero))
; true
(display-bool (iszero? one))
; false
(display-bool (equal? one one))
; true
(display-bool (equal? one four))
; false

;------------------------------------------
; definition of pairs

(define (pair x y)
  (lambda (m) (m x y)))

(define (fst p)
  (p true))

(define (snd p)
  (p false))

; test

(define pair-test (pair four five))
(display-nat (fst pair-test))
; 4
(display-nat (snd pair-test))
; 5
